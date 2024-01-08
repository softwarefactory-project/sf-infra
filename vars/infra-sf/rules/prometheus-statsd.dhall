let Rule = ../../../Infra/Rule/package.dhall

let udp-multiplexer = +7000

let ibm-bm3 = "169.60.49.226"

let ibm-bm4 = "169.60.49.233"

let vexxhost-infra-sf-private = "192.168.242.0/24"

let vexxhost-pub1 = "38.102.83.0/24"

let vexxhost-pub2 = "38.129.56.0/24"

in    Rule.integerMap (Rule.createUdpHost vexxhost-pub1) [ udp-multiplexer ]
    # Rule.integerMap (Rule.createUdpHost vexxhost-pub2) [ udp-multiplexer ]
    # Rule.integerMap
        (Rule.createUdpHost vexxhost-infra-sf-private)
        [ udp-multiplexer ]
    # Rule.integerMap (Rule.createUdpHost ibm-bm3) [ udp-multiplexer ]
    # Rule.integerMap (Rule.createUdpHost ibm-bm4) [ udp-multiplexer ]
