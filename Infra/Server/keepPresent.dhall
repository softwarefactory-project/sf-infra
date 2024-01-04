let Prelude = ../Prelude.dhall

let State = ./State.dhall

let isPresent
    : ./Type.dhall -> Bool
    = \(server : ./Type.dhall) ->
        merge { present = True, absent = False } server.state

let keepPresent
    : List ./Type.dhall -> List ./Type.dhall
    = Prelude.List.filter ./Type.dhall isPresent

in  keepPresent
