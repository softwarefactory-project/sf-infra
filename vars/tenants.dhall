{- This file describe the tenants variables file needed by the playbooks:

- playbooks/configure-tenants.yaml
- playbooks/create-hosts.yaml playbooks
-}
let Infra = ../conf/package.dhall

in  { Infra =
      { RDO =
          let instances = ./infra-rdo/instances.dhall

          in  Infra.Tenant::(     { servers = Infra.getServers instances
                                  , volumes = Infra.getVolumes instances
                                  }
                              //  ./infra-rdo/networking.dhall
                              //  ./infra-rdo/images.dhall
                            )
      , SF =
          let instances = ./infra-sf/instances.dhall

          in  Infra.Tenant::(     { servers = Infra.getServers instances
                                  , volumes = Infra.getVolumes instances
                                  }
                              //  ./infra-sf/networking.dhall
                              //  ./infra-sf/images.dhall
                            )
      }
    , Nodepool =
      { RDO = Infra.Tenant::(./nodepool-rdo/networking.dhall)
      , SF = Infra.Tenant::(./nodepool-sf/networking.dhall)
      , TripleO = Infra.Tenant::(     ./nodepool-tripleo/networking.dhall
                                  //  ./nodepool-tripleo/images.dhall
                                )
      }
    }
