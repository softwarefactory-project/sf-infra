--| Adds an IP to a Server record
let Ip = \(ip : Text) -> { auto_ip = None Bool, floating_ips = Some [ ip ] }

let example0 =
      let Server = { Type = ./Type.dhall, default = ./default.dhall }

      in    assert
          :         Server::{
                    , image = "centos"
                    , name = "www.softwarefactory-project.io"
                    }
                //  Ip "38.102.83.76"
            ===  Server::{
                 , image = "centos"
                 , name = "www.softwarefactory-project.io"
                 , auto_ip = None Bool
                 , floating_ips = Some [ "38.102.83.76" ]
                 }

in  Ip
