let Infra = ../../conf/package.dhall

in  { images =
      [ Infra.OS.Fedora.`31`.image
      , Infra.OS.Fedora.`30`.image
      , Infra.OS.CentOS.`7.0`.image
      ]
    }
