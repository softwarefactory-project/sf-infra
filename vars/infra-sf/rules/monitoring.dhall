let Infra = ../../../conf/package.dhall

let private-monitoring-rules =
      Infra.Prelude.List.map
        Infra.Rule.Type
        Infra.Rule.Type
        ( \(rule : Infra.Rule.Type) ->
            rule // { remote_ip_prefix = Some "{{ prometheus_private_ip }}/32" }
        )
        ./prometheus.dhall

let public-monitoring-rules =
      Infra.Prelude.List.map
        Infra.Rule.Type
        Infra.Rule.Type
        ( \(rule : Infra.Rule.Type) ->
            rule // { remote_ip_prefix = Some "{{ prometheus_public_ip }}/32" }
        )
        private-monitoring-rules

in  private-monitoring-rules # public-monitoring-rules
