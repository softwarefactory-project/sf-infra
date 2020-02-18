let OS = (../common.dhall).OS

in  { images =
      [ OS.Fedora.`31`.image, OS.Fedora.`30`.image, OS.CentOS.`7.0`.image ]
    }
