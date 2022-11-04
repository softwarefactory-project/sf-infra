let OS = (../common.dhall).OS

in  { images =
      [ OS.CentOS.`7.0`.image
      , OS.CentOS.`8-stream`.image
      , OS.CentOS.`8.3`.image
      , OS.Fedora.`30`.image
      , OS.Fedora.`31`.image
      , OS.Fedora.`33`.image
      , OS.Fedora.`35`.image
      , OS.Fedora.`36`.image
      ]
    }
