let Prelude = ../Prelude.dhall

let Instance = ../Instance/package.dhall

let Volume = ../Volume/package.dhall

in  \(instances : List Instance.Type) ->
      Prelude.List.concat
        Volume.Type
        ( Prelude.List.map
            Instance.Type
            (List Volume.Type)
            ( \(i : Instance.Type) ->
                Prelude.List.map
                  Volume.Type
                  Volume.Type
                  (\(v : Volume.Type) -> v // { server = i.name })
                  i.volumes
            )
            instances
        )
