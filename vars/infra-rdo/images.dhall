let Infra = ../../conf/package.dhall

in  { images =
      [ Infra.OS.CentOS.`7.0`.image
      , Infra.OS.Fedora.`30`.image
      , Infra.OS.CentOS.`8.0`.image
      , Infra.OS.CentOS.`8.1`.image
      ]
    }
