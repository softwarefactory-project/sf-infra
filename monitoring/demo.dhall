let Infra = ../Infra/package.dhall

let TestInstance =
      Infra.Instance::{
      , name = "Test"
      , connection = Infra.Connection::{ ansible_user = Some "centos" }
      , server = Some Infra.Server::{ image = "unused" }
      }

in  { Simplest =
        ./prometheus-config.dhall
          ([] : List Infra.Instance.Type)
          [ "rules-node.yaml" ]
          ([] : List (./binding.dhall).ScrapeConfig.Type)
    , OneInstance =
        ./prometheus-config.dhall
          [ TestInstance ]
          [ "rules-node.yaml" ]
          ([] : List (./binding.dhall).ScrapeConfig.Type)
    , OneInstanceUrl =
        ./prometheus-config.dhall
          [ TestInstance // { monitoring_urls = [ "https://example.com/test" ] }
          ]
          [ "rules-node.yaml" ]
          ([] : List (./binding.dhall).ScrapeConfig.Type)
    }
