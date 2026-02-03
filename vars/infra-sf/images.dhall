let OS = (../common.dhall).OS

in  { images =
      [ OS.Fedora.`43`.image
      , OS.CentOS.`9-stream`.image
      , OS.RHEL.`9.3`.image
      , OS.RHEL.`9.4`.image
      , OS.RHEL.`10.1`.image
      ]
    }
