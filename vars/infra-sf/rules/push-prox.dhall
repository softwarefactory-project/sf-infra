let Infra = ../../../conf/package.dhall

in  Infra.text-to-rule-map (Infra.tcp-access-rule +8080) [ "8.43.84.199/32" ]
