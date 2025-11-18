let OS = (../common.dhall).OS

in  { images =
      [ OS.CentOS.`7.0`.image, OS.CentOS.`8.0`.image, OS.CentOS.`8.1`.image ]
    }
