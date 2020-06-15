{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
  env:DHALL_PROMETHEUS sha256:64114b32c7daea1ef139ee0b0f885923f03adc744d767059f1a972cbeff70793
? https://raw.githubusercontent.com/TristanCacqueray/dhall-prometheus/5f288835b8a3cc9eb984cc7b2ca6f43125561d7b/package.dhall sha256:64114b32c7daea1ef139ee0b0f885923f03adc744d767059f1a972cbeff70793
