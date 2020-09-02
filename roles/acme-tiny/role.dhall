{-|
This file describes an ansible role made of

 { Tasks : List Ansible.Task
 , Defaults : Map Text Text
 , Templates : Map Text Text
 }

-}

let --| External resources
    Ansible =
        env:DHALL_ANSIBLE
      ? ~/src/github.com/TristanCacqueray/dhall-ansible/package.dhall
      ? https://raw.githubusercontent.com/TristanCacqueray/dhall-ansible/682a3a587ab40bada03ab0f28944b587131d9102/package.dhall sha256:6081f2916f7d8a07f5852cbea50727a6a04426c1bcc7b88867d306db9cb4f486

let List/map =
      https://prelude.dhall-lang.org/v17.0.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Text/concatSep =
      https://prelude.dhall-lang.org/Text/concatSep.dhall sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let --| Define stringly variables
    RawVars =
      { domain = "item.domain"
      , vhost_root = "item.vhost_root"
      , domains = "acme_domains"
      , challenges-dir = "acme_challenges_dir"
      , challenges-dir-value = "/var/www/challenges"
      , vhost-owner = "acme_vhost_owner"
      , vhost-owner-value = "root"
      , keys-dir = "acme_keys_dir"
      , keys-dir-value = "/var/lib/software-factory/bootstrap-data/acme-tiny"
      , certs-dir = "acme_certs_dir"
      , certs-dir-value = "/etc/letsencrypt/pem"
      }

let Defaults =
      let StringOrList = < Str : Text | ListOfString : List Text >

      let SOL/Show =
            \(sol : StringOrList) ->
              merge
                { Str = \(s : Text) -> s
                , ListOfString = \(s : List Text) -> "[]"
                }
                sol

      let str = StringOrList.Str

      let emptyList = StringOrList.ListOfString ([] : List Text)

      let DefaultsType = { mapKey : Text, mapValue : StringOrList, doc : Text }

      let DefaultsYaml = { mapKey : Text, mapValue : StringOrList }

      let def2yaml =
            \(default : DefaultsType) ->
              { mapKey = default.mapKey, mapValue = default.mapValue }

      let def2text =
            \(default : DefaultsType) ->
              ''
              - `${default.mapKey}`: ${default.doc}
                :default: ${SOL/Show default.mapValue}
              ''

      in  { Items =
            [ { mapKey = RawVars.challenges-dir
              , mapValue = str RawVars.challenges-dir-value
              , doc = "The directory to store challenges"
              }
            , { mapKey = RawVars.keys-dir
              , mapValue = str RawVars.keys-dir-value
              , doc = "The directory to store keys"
              }
            , { mapKey = RawVars.certs-dir
              , mapValue = str RawVars.certs-dir-value
              , doc = "The directory to store certs"
              }
            , { mapKey = RawVars.vhost-owner
              , mapValue = str RawVars.vhost-owner-value
              , doc = "The name of the vhost owner"
              }
            , { mapKey = RawVars.domains
              , mapValue = emptyList
              , doc = "The list of domains"
              }
            ]
          , mkJSONMap =
              \(items : List DefaultsType) ->
                List/map DefaultsType DefaultsYaml def2yaml items
          , mkDoc =
              \(items : List DefaultsType) ->
                Text/concatSep "\n" (List/map DefaultsType Text def2text items)
          }

let Vars =
      let jinjaEscape = \(var : Text) -> "{{ ${var} }}"

      in  { domain = jinjaEscape RawVars.domain
          , vhost-root = jinjaEscape RawVars.vhost_root
          , domains = jinjaEscape RawVars.domains
          , challenges-dir = jinjaEscape RawVars.challenges-dir
          , vhost-owner = jinjaEscape RawVars.vhost-owner
          , keys-dir = jinjaEscape RawVars.keys-dir
          , certs-dir = jinjaEscape RawVars.certs-dir
          }

let Files =
      { challenge = "${Vars.challenges-dir}/${Vars.domain}"
      , account = "${Vars.keys-dir}/account"
      , domain = "${Vars.keys-dir}/${Vars.domain}"
      , raw-cert =
          let --| `raw-cert` is similar to `cert` but to be used in jinja without the '{{' '}}' escape
              value =
                "${RawVars.certs-dir} + '/' + ${RawVars.domain}"

          in  value
      , cert = "${Vars.certs-dir}/${Vars.domain}"
      , key = "${Vars.keys-dir}/${Vars.domain}"
      , refresh-script = "acme-tiny-refresh.sh"
      , vhost-conf = "vhost.conf"
      }

let --| Useful functions
    mkDir =
      \(owner : Text) ->
      \(dir : Text) ->
        Ansible.Task::{
        , name = Some "Ensure directory ${dir} exists"
        , file = Some Ansible.File::{
          , path = dir
          , state = Some "directory"
          , mode = Some "0755"
          , owner = Some owner
          }
        }

let runOpenssl =
      \(command : Text) ->
      \(key-file : Text) ->
        Ansible.Task::{
        , name = Some "Run openssl ${command} for: ${key-file}"
        , shell = Some Ansible.Shell::{
          , cmd = "openssl ${command} > ${key-file}"
          , creates = Some key-file
          }
        }

let genKey = runOpenssl "genrsa 4096"

let genCsr =
      \(key-file-prefix : Text) ->
      \(domain : Text) ->
        runOpenssl
          "req -new -sha256 -key ${key-file-prefix}.key -subj \"/CN=${domain}\""
          "${key-file-prefix}.csr"

let --| A convenient record to make a task loop for each domain
    per-domains =
      { loop = Some Vars.domains }

let --| The roles tasks
    tasks =
      let requirements =
            [ Ansible.Task::{
              , name = Some "Install acme-tiny"
              , package = Some Ansible.Package::{
                , name = "acme-tiny"
                , state = "present"
                }
              }
            ]

      let create-crypto-file-tasks =
            [ mkDir "root" Vars.keys-dir
            , mkDir "root" Vars.certs-dir
            , genKey "${Files.account}.key"
            , genKey "${Files.domain}.key" // per-domains
            , genCsr "${Files.domain}" Vars.domain // per-domains
            , mkDir "root" "${Files.challenge}" // per-domains
            , mkDir "${Vars.vhost-owner}" "${Vars.vhost-root}" // per-domains
            ]

      let ensure-apache-conf =
            [     Ansible.Task::{
                  , name = Some "Check if cert exists"
                  , stat = Some Ansible.Stat::{ path = "${Files.cert}.pem" }
                  , register = Some "_cert_files"
                  }
              //  per-domains
            ,     Ansible.Task::{
                  , name = Some "Install vhost configuration"
                  , template = Some Ansible.Template::{
                    , dest = "/etc/httpd/conf.d/${Vars.domain}.conf"
                    , src = Files.vhost-conf
                    }
                  , register = Some "_vhost_conf"
                  }
              //  per-domains
            , Ansible.Task::{
              , name = Some "Reload apache"
              , when = Some "_vhost_conf is changed"
              , service = Some Ansible.Service::{
                , name = "httpd"
                , state = Some "reloaded"
                }
              }
            ]

      let refreshCerts =
            \(script : Text) ->
              [ Ansible.Task::{
                , name = Some "Setup acme-tiny refresh script ${script}"
                , template = Some Ansible.Template::{
                  , dest = script
                  , src = Files.refresh-script
                  , mode = Some "0755"
                  }
                }
              , Ansible.Task::{
                , name = Some "Run the acme-tiny script"
                , command = Some "${script} no-reload"
                }
              ]

      let --| Re-run apache-conf setup in case new keys has been created
          rerun-ensure-apache-conf =
            ensure-apache-conf

      in    requirements
          # create-crypto-file-tasks
          # ensure-apache-conf
          # refreshCerts "/usr/local/bin/acme-tiny-refresh.sh"
          # rerun-ensure-apache-conf

let set-become =
      \(become : Bool) ->
        List/map
          Ansible.Task.Type
          Ansible.Task.Type
          (\(task : Ansible.Task.Type) -> task // { become = Some become })

in  { Tasks = set-become True tasks
    , Templates =
      [ { mapKey = Files.refresh-script
        , mapValue =
            ./templates/acme-tiny-refresh.sh.dhall
              "${Files.account}.key"
              "${Files.domain}.csr"
              "${Files.cert}.pem"
              "${Files.challenge}"
        }
      , { mapKey = Files.vhost-conf
        , mapValue =
            ./templates/vhost.conf.dhall
              RawVars.domain
              Vars.domain
              Vars.challenges-dir
              Vars.vhost-root
              "${Files.cert}.pem"
              "${Files.key}.key"
        }
      ]
    , Defaults = Defaults.mkJSONMap Defaults.Items
    , README =
        ''
        # acme-tiny : manage let's encrypt certificate

        ## Variables

        ${Defaults.mkDoc Defaults.Items}

        ## Contribute

        This role is entirely defined in the `role.dhall` file.
        Update the content by running these commands:

          dhall to-directory-tree --output templates <<< "(./role.dhall).Templates"
          dhall-to-yaml --output tasks/main.yaml     <<< "(./role.dhall).Tasks"
          dhall-to-yaml --output defaults/main.yaml  <<< "(./role.dhall).Defaults"
          dhall text > README.md                     <<< "(./role.dhall).README"
        ''
    }
