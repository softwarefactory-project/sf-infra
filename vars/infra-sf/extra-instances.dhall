let Infra = ../../Infra/package.dhall

let -- | A function to create logscraper0{}.openstack.org
    mkLogscraper =
      \(idx : Natural) ->
        Infra.Instance::{
        , name = "logscraper0${Natural/show idx}.openstack.org"
        , groups = [ "openstack" ]
        , connection = Infra.Connection::{
          , ansible_user = "sf"
          , ansible_python_interpreter = "auto"
          }
        }

let -- | These machines are hosted in Openstack infrastructure.
    -- please contact Jeremy Stanley <fungi@yuggoth.org> or
    -- Clark Boylan <cboylan@sapwetik.org>
    -- Tony Breeds <tony@bakeyournoodle.com>
    logscraper-openstack =
      [ mkLogscraper 2 ]

in  logscraper-openstack
