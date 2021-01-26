let Rule = ../../../Infra/Rule/package.dhall

let statsd-exporter = +9125

let udp-multiplexer = +7000

let metric-emiter = "38.102.83.114/32"

let nodepool-builder-02 = "38.145.37.207/32"

in    Rule.integerMap
        (Rule.createUdpHost metric-emiter)
        [ statsd-exporter, udp-multiplexer ]
    # Rule.integerMap
        (Rule.createUdpHost nodepool-builder-02)
        [ statsd-exporter, udp-multiplexer ]
