let Prelude = ../Prelude.dhall

let Instance/show = ./show.dhall

let mkInventory =
      \(instances : List ./Type.dhall) ->
        let Inventory =
              Prelude.Text.concatSep
                "\n"
                (Prelude.List.map ./Type.dhall Text Instance/show instances)

        in  ''
            # Inventory

            This project manages:

            ${Inventory}

            ''

in  mkInventory
