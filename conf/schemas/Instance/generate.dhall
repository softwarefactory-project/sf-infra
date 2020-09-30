let Prelude = ../../Prelude.dhall

in  \(f : Natural -> ./Type.dhall) ->
    \(count : Natural) ->
      Prelude.List.generate count ./Type.dhall (\(idx : Natural) -> f (idx + 1))
