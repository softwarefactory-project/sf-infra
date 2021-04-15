{ connection : ../Connection/Type.dhall
, backup : Optional ../Backup/Type.dhall
, groups : List Text
, monitoring_auth_urls : List Text
, monitoring_urls : List Text
, name : Text
, node-exporter : Bool
, server : Optional ../Server/Type.dhall
, volumes : List ../Volume/Type.dhall
}
