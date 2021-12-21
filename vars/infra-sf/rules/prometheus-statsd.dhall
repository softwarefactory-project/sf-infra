let Rule = ../../../Infra/Rule/package.dhall

let udp-multiplexer = +7000

let public_network = "38.102.83.0/24"

let public_network_2 = "38.129.56.0/24"

let private_network = "192.168.242.0/24"

in    Rule.integerMap (Rule.createUdpHost public_network) [ udp-multiplexer ]
    # Rule.integerMap (Rule.createUdpHost public_network_2) [ udp-multiplexer ]
    # Rule.integerMap (Rule.createUdpHost private_network) [ udp-multiplexer ]
