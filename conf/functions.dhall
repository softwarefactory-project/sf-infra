{- The package contains common symbol shared by the other dhall files -}
let Prelude =
        env:DHALL_PRELUDE
      ? https://prelude.dhall-lang.org/v17.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let Infra = ./schemas/package.dhall

let {- Generate sequence like unix seq command -} seq =
      \(count : Natural) ->
        let seq = Prelude.List.replicate count Natural 1

        let indexed = Prelude.List.indexed Natural seq

        let IndexedType = { index : Natural, value : Natural }

        in  Prelude.List.map
              IndexedType
              Natural
              (\(index : IndexedType) -> index.index + 1)
              indexed

let seq-test = assert : seq 5 === [ 1, 2, 3, 4, 5 ]

let mkNetwork =
      \(network : Text) ->
      \(name : Text) ->
        { name = name ++ "-network"
        , external_network = network
        , port_security_enabled = False
        }

let mkSubnetWithMask =
      \(mask : Text) ->
      \(name : Text) ->
      \(network_prefix : Text) ->
      \(dns_nameservers : List Text) ->
        { name = name ++ "-subnet"
        , cidr = network_prefix ++ ".0/" ++ mask
        , gateway_ip = network_prefix ++ ".1"
        , dns_nameservers
        , network_name = name ++ "-network"
        }

let mkSubnet = mkSubnetWithMask "24"

let mkRouter =
      \(network : Text) ->
      \(name : Text) ->
      \(network_prefix : Text) ->
        { name = name ++ "-router"
        , network
        , port_security_enabled = None Bool
        , interfaces =
          [ { net = name ++ "-network"
            , subnet = name ++ "-subnet"
            , portip = network_prefix ++ ".1"
            }
          ]
        }

let Group = ./types/Group.dhall

let GroupOfGroup = ./types/GroupOfGroup.dhall

let Groups = ./types/Groups.dhall

let HostVarValue = < int : Natural | str : Text | bool : Bool >

let mapServer = Prelude.List.map Infra.Server.Type Infra.Server.Type

let mapInstance = Prelude.List.map Infra.Instance.Type Infra.Instance.Type

let emptyVars = [] : List { mapKey : Text, mapValue : HostVarValue }

let mkHost =
      Prelude.List.map
        Infra.Instance.Type
        { mapKey : Text
        , mapValue : List { mapKey : Text, mapValue : HostVarValue }
        }
        ( \(instance : Infra.Instance.Type) ->
            { mapKey = instance.name
            , mapValue =
                  toMap
                    { ansible_user =
                        HostVarValue.str instance.connection.ansible_user
                    , ansible_python_interpreter =
                        HostVarValue.str
                          instance.connection.ansible_python_interpreter
                    , ansible_port =
                        HostVarValue.int instance.connection.ansible_port
                    }
                # merge
                    { None = emptyVars
                    , Some =
                        \(some : Bool) ->
                          toMap { ansible_become = HostVarValue.bool some }
                    }
                    instance.connection.ansible_become
                # merge
                    { None = emptyVars
                    , Some =
                        \(command : Text) ->
                          toMap
                            { ansible_ssh_common_args =
                                HostVarValue.str
                                  "-o ProxyCommand=\"${command}\""
                            }
                    }
                    instance.connection.proxy_command
                # merge
                    { None = emptyVars
                    , Some =
                        \(ip-addr : Text) ->
                          toMap { ansible_host = HostVarValue.str ip-addr }
                    }
                    instance.connection.ansible_host
            }
        )

let mkGroup =
      \(instances : List Infra.Instance.Type) ->
      \(group-of-groups : List GroupOfGroup) ->
        let ChildrenValue = { mapKey : Group, mapValue : {} }

        let HostsValue = { mapKey : Text, mapValue : {} }

        let ChildrensValue =
              { Type =
                  { hosts : Optional (List HostsValue)
                  , children : Optional (List ChildrenValue)
                  }
              , default =
                { hosts = None (List HostsValue)
                , children = None (List ChildrenValue)
                }
              }

        let {- Create a group value for a given groupType
            -} mkGroupOfHost =
              let {- Convert list of instance to their name dict
                  -} instanceName =
                    Prelude.List.map
                      Infra.Instance.Type
                      HostsValue
                      ( \(instance : Infra.Instance.Type) ->
                          { mapKey = instance.name, mapValue = {=} }
                      )

              let {- Check if a instance is part of a group
                  -} filterInstance =
                    \(group : Groups.Type) ->
                    \(instance : Infra.Instance.Type) ->
                      Prelude.List.fold
                        Group
                        instance.groups
                        Bool
                        ( \(group : Group) ->
                          \(acc : Bool) ->
                            acc || group@1.test group
                        )
                        False

              in  \(group : Groups.Type) ->
                    { mapKey = group.value
                    , mapValue = ChildrensValue::{
                      , hosts = Some
                          ( instanceName
                              ( Prelude.List.filter
                                  Infra.Instance.Type
                                  (filterInstance group)
                                  instances
                              )
                          )
                      }
                    }

        let {- Create a group of group value in inventory
            -} mkGroupOfGroup =
              \(gog : GroupOfGroup) ->
                let {- Convert list of group to their name dict
                    -} groupName =
                      Prelude.List.map
                        Group
                        ChildrenValue
                        (\(group : Group) -> { mapKey = group, mapValue = {=} })

                in  { mapKey = gog.name
                    , mapValue = ChildrensValue::{
                      , children = Some (groupName gog.children)
                      }
                    }

        let ChildrenDict = { mapKey : Group, mapValue : ChildrensValue.Type }

        in    Prelude.List.map
                Groups.Type
                ChildrenDict
                mkGroupOfHost
                Groups.groups
            # Prelude.List.map
                GroupOfGroup
                ChildrenDict
                mkGroupOfGroup
                group-of-groups

let setFqdn =
      \(fqdn : Text) ->
        mapInstance
          ( \(instance : Infra.Instance.Type) ->
              instance // { name = instance.name ++ "." ++ fqdn }
          )

let securityGroupRuleToFirewallRule
    : Infra.Rule.Type -> Infra.Firewall.Type
    = \(rule : Infra.Rule.Type) ->
        let proto =
              merge
                { None = "tcp", Some = \(proto : Text) -> proto }
                rule.protocol

        let port = Integer/clamp rule.port

        in  merge
              { None = Infra.Firewall::{
                , port = Some "${Natural/show port}/${proto}"
                }
              , Some =
                  \(address : Text) ->
                    Infra.Firewall::{
                    , rich_rule = Some
                        (     "rule family=ipv4 "
                          ++  "source address=${address} "
                          ++  "port port=${Natural/show port} "
                          ++  "protocol=${proto} accept"
                        )
                    }
              }
              rule.remote_ip_prefix

let securityGroupRulesToFirewallRules
    : List Infra.Rule.Type -> List Infra.Firewall.Type
    = Prelude.List.map
        Infra.Rule.Type
        Infra.Firewall.Type
        securityGroupRuleToFirewallRule

let {- This is a function transformer,
       it transforms a `Text -> Rule` function to a `List Text -> List Rule` function
    -} text-to-rule-map =
      Prelude.List.map Text Infra.Rule.Type

let {- This function takes a port Integer and it returns a function
       that takes an IP as an input and returns a Rule
    -} access-rule =
      \(proto : Text) ->
      \(port : Integer) ->
      \(ip : Text) ->
        Infra.Rule::{ port, protocol = Some proto, remote_ip_prefix = Some ip }

let tcp-access-rule = access-rule "tcp"

let udp-access-rule = access-rule "udp"

in  { Prelude
    , text-to-rule-map
    , tcp-access-rule
    , udp-access-rule
    , tcp-ports-rule =
        \(ip : Text) -> \(port : Integer) -> tcp-access-rule port ip
    , udp-ports-rule =
        \(ip : Text) -> \(port : Integer) -> udp-access-rule port ip
    , getReachable =
        \(instances : List Infra.Instance.Type) ->
          Prelude.List.filter
            Infra.Instance.Type
            ( \(i : Infra.Instance.Type) ->
                merge
                  { None = True, Some = \(proxy : Text) -> False }
                  i.connection.proxy_command
            )
            instances
    , getServers =
        \(instances : List Infra.Instance.Type) ->
          Prelude.List.map
            Infra.Instance.Type
            Infra.Server.Type
            (\(i : Infra.Instance.Type) -> i.server // { name = i.name })
            ( Prelude.List.filter
                Infra.Instance.Type
                (\(i : Infra.Instance.Type) -> i.skip_os_server_task == False)
                instances
            )
    , getVolumes =
        \(instances : List Infra.Instance.Type) ->
          Prelude.List.concat
            Infra.Volume.Type
            ( Prelude.List.map
                Infra.Instance.Type
                (List Infra.Volume.Type)
                ( \(i : Infra.Instance.Type) ->
                    Prelude.List.map
                      Infra.Volume.Type
                      Infra.Volume.Type
                      (\(v : Infra.Volume.Type) -> v // { server = i.name })
                      i.volumes
                )
                instances
            )
    , mkHost
    , mkGroup
    , mkSubnetWithMask
    , mkSubnet
    , mkNetwork
    , mkRouter
    , setSecurityGroups =
        \(security-groups : List Text) ->
          mapInstance
            ( \(instance : Infra.Instance.Type) ->
                    instance
                //  { server =
                            instance.server
                        //  { security_groups =
                                  security-groups
                                # instance.server.security_groups
                            }
                    }
            )
    , setIp =
        \(ip : Text) -> { auto_ip = None Bool, floating_ips = Some [ ip ] }
    , seq
    , map = Prelude.List.map
    , setFqdn
    , mapServerText = Prelude.List.map Infra.Server.Type Text
    , securityGroupRulesToFirewallRules
    }
