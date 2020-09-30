let Rule = ../../../conf/schemas/Rule/package.dhall

in  Rule.textMap (Rule.createTcpPort +8080) [ "8.43.84.248/32" ]
