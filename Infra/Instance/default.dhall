{ backup = None ../Backup/Type.dhall
, groups = [] : List Text
, monitoring_auth_urls = [] : List Text
, monitoring_urls = [] : List Text
, monitoring_urls_skip_cert_verify = [] : List Text
, node-exporter = True
, server = None ../Server/Type.dhall
, volumes = [] : List ../Volume/Type.dhall
}
