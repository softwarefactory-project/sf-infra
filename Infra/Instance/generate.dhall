--| An helper function to generate a list of Instance
let Prelude = ../Prelude.dhall

let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let generate
    : forall (f : Natural -> Instance.Type) ->
      forall (count : Natural) ->
        List Instance.Type
    = \(f : Natural -> Instance.Type) ->
      \(count : Natural) ->
        Prelude.List.generate
          count
          Instance.Type
          (\(idx : Natural) -> f (idx + 1))

let example0 =
      let createInstance =
            \(idx : Natural) -> Instance::{ name = "www-${Natural/show idx}" }

      in    assert
          :     generate createInstance 3
            ===  [ Instance::{ name = "www-1" }
                 , Instance::{ name = "www-2" }
                 , Instance::{ name = "www-3" }
                 ]

in  generate
