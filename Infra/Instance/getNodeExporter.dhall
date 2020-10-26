--| Return the node-exporter attribute of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let getNodeExporter
    : Instance.Type -> Bool
    = \(instance : Instance.Type) -> instance.node-exporter

in  getNodeExporter
