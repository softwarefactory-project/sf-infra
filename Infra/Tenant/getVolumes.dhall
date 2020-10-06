--| Returns the list of volumes
let Prelude = ../Prelude.dhall

let Instance = ../Instance/package.dhall

let Volume = ../Volume/package.dhall

let getVolumes
    : List Instance.Type -> List Volume.Type
    = \(instances : List Instance.Type) ->
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

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     getVolumes
                  [ Instance::{
                    , connection = Connection::{ ansible_user = "centos" }
                    , name = "www"
                    }
                  ]
            ===  Prelude.List.empty Volume.Type

let -- TODO: add example of instance with volume
    example1 =
      assert : True === True

in  getVolumes
