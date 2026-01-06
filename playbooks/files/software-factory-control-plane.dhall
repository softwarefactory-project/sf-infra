-- | This file define the SoftwareFactory resources for the sf-operator.
let -- The generated schema from the openapi definitions
    SF =
      https://softwarefactory-project.io/cgit/software-factory/sf-operator/plain/schemas/package.dhall?id=658d9c72c3eb1778d90636b3a696c4127e002499 using (toMap
                                                                                                                                                            { User-Agent =
                                                                                                                                                                "dhall"
                                                                                                                                                            })
        sha256:4ac950ff976601ed4c4b7136de1e26a297611ecb9f638b659deba54fac013d76

let fqdn = "gateway-cloud-softwarefactory.apps.ocp.cloud.ci.centos.org"

let zuul_connections =
      { gerritconns = Some
        [ SF.GerritConn::{
          , name = "softwarefactory-project.io"
          , hostname = "softwarefactory-project.io"
          , puburl = Some "https://softwarefactory-project.io/r"
          , sshkey = Some "zuul-sf-ssh-secret"
          , username = Some "microzuul"
          }
        , SF.GerritConn::{
          , name = "rdoproject.org"
          , hostname = "review.rdoproject.org"
          , canonicalhostname = Some "rdoproject.org"
          , puburl = Some "https://review.rdoproject.org/r"
          , username = Some "zuul"
          }
        , SF.GerritConn::{
          , name = "opendev.org"
          , canonicalhostname = Some "opendev.org"
          , hostname = "review.opendev.org"
          , puburl = Some "https://review.opendev.org"
          , username = Some "sf-project-io"
          }
        ]
      , gitconns = Some
        [ SF.GitConn::{
          , name = "github.com-git-driver"
          , baseurl = "https://github.com"
          , pollDelay = Some 7200
          }
        ]
      , githubconns = Some
        [ SF.GithubConn::{
          , name = "github.com"
          , appID = Some 846254
          , secrets = Some "zuul-github-com-connection"
          , server = Some "github.com"
          , verifySsl = Some True
          }
        ]
      , gitlabconns = Some
        [ SF.GitlabConn::{
          , name = "gitlab.com"
          , baseUrl = Some "https://gitlab.com"
          , secrets = "zuul-gitlab-com-connection"
          , server = Some "gitlab.com"
          }
        ]
      , pagureconns = Some
        [ SF.PagureConn::{
          , name = "pagure.io"
          , baseUrl = Some "https://pagure.io"
          , secrets = Some "zuul-pagure-io-connection"
          , server = Some "pagure.io"
          , sourceWhitelist = Some "8.43.85.75"
          }
        , SF.PagureConn::{
          , name = "src.fedoraproject.org"
          , baseUrl = Some "https://src.fedoraproject.org"
          , secrets = Some "zuul-src-fedoraproject-org-connection"
          , server = Some "src.fedoraproject.org"
          , sourceWhitelist = Some "38.102.83.40"
          }
        ]
      }

let Zuul = SF.Zuul // { default = SF.Zuul.default // zuul_connections }

let FluentBitLogForwarding =
      Some
        SF.FluentBitLogForwarding::{
        , forwardInputHost = "fluent-bit"
        , forwardInputPort = Some 24224
        }

let main_spec =
      SF.Spec::{
      , fqdn
      , prometheusMonitorsDisabled = Some True
      , FluentBitLogForwarding
      , config-location = Some SF.ConfigLocation::{
        , name = "softwarefactory-project/config"
        , zuul-connection-name = "gitlab.com"
        , k8s-api-url = Some "https://api.ocp.cloud.ci.centos.org:6443"
        , logserver-host = Some "{{ logserver_public_hostname }}"
        , branch = Some "main"
        }
      , mariadb = Some SF.Mariadb::{
        , dbStorage = Some SF.Storage::{ size = "5Gi" }
        }
      , zookeeper = Some SF.Zookeeper::{
        , storage = Some SF.Storage::{ size = "5Gi" }
        }
      , logserver = Some SF.Logserver::{
        , loopDelay = Some 7
        , retentionDays = Some 7
        , storage = Some SF.Storage::{ size = "300Gi" }
        }
      , zuul = Some Zuul::{
        , smtpconns = Some
          [ SF.SmtpConn::{
            , name = "smtp"
            , server = "smtp.ci.centos.org"
            , defaultFrom = Some "zuul@softwarefactory-project.io"
            }
          ]
        , oidcAuthenticators = Some
          [ SF.ZuulOidcAuthenticators::{
            , name = "keycloak_sfio"
            , clientID = "zuul-microshift"
            , issuerID = "https://softwarefactory-project.io/auth/realms/SF"
            , realm = "SF"
            }
          ]
        , defaultAuthenticator = Some "keycloak_sfio"
        , scheduler = Some SF.ZuulScheduler::{
          , logLevel = Some "DEBUG"
          , storage = Some SF.Storage::{ size = "1Gi" }
          }
        , executor = Some SF.ZuulExecutor::{ enabled = Some False }
        , merger = Some SF.ZuulMerger::{
          , storage = Some SF.Storage::{ size = "30Gi" }
          , logLevel = Some "DEBUG"
          }
        }
      , nodepool = Some SF.Nodepool::{
        , launcher = Some SF.NodepoolLauncher::{
          , limits = Some SF.Limits::{ cpu = "2000m", memory = "4Gi" }
          }
        , builder = Some SF.NodepoolBuilder::{
          , storage = Some SF.Storage::{ size = "300Gi" }
          }
        }
      , codesearch = Some SF.Codesearch::{ enabled = Some True }
      }

let executor =
      Some
        SF.ZuulExecutor::{
        , limits = Some SF.Limits::{ cpu = "6000m", memory = "16Gi" }
        , storage = Some SF.Storage::{ size = "30Gi" }
        , diskLimitPerJob = Some +8096
        , logLevel = Some "DEBUG"
        , standalone = Some
          { controlPlanePublicZKHostname = "{{ zk_public_hostname }}"
          , controlPlanePublicGSHostname = "{{ gitserver_public_hostname }}"
          , publicHostname = "{{ executor_public_ip }}"
          }
        }

let executor_spec =
      SF.Spec::{
      , fqdn
      , FluentBitLogForwarding
      , prometheusMonitorsDisabled = Some True
      , zuul = Some Zuul::{ executor }
      }

let base =
      { apiVersion = "sf.softwarefactory-project.io/v1"
      , kind = "SoftwareFactory"
      }

in  { software_factory_cr =
            base
        //  { metadata =
              { name = "sf-on-centos", namespace = "cloud-softwarefactory" }
            , spec = main_spec
            }
    , executor_cr =
        base // { metadata.name = "ext-sf-comps", spec = executor_spec }
    }
