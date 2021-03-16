{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List (intercalate)
import Data.Text (Text, unpack)
import Dhall (auto, input)
import qualified Dhall.TH

-- | Generate Haskell Type from Dhall Type, see: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  [ Dhall.TH.SingleConstructor "Server" "Server" "./Infra/Server/Type.dhall"
  ]

-- | Convert a flavor name into a (cpu, mem) tuple
flavorInfo :: Text -> (Int, Int)
flavorInfo = \case
  "1vcpu_1gb" -> (1, 1)
  "1vcpu_2gb" -> (1, 2)
  "4vcpus_8gb" -> (4, 8)
  "8vcpu_16GB" -> (8, 16)
  "v1-standard-1" -> (1, 4)
  "v1-standard-2" -> (2, 8)
  "v1-standard-4" -> (4, 16)
  "v1-standard-8" -> (8, 32)
  "ci.m1.large" -> (4, 8)
  x -> error ("Unknown flavor" <> show x)

-- | Convert a server definition into a csv row
showServer :: Server -> String
showServer Server {..} = intercalate "," [unpack name, show cpu, show mem]
  where
    (cpu, mem) = maybe (error "no flavor set") flavorInfo flavor

main :: IO ()
main = do
  -- Load the servers list
  (servers :: [Server]) <- input auto "(./vars/package.dhall).servers"

  -- Create the csv lines
  csv <- pure (["Host,Cpu,Memory"] <> map showServer servers)

  -- Update the inventory
  writeFile "./doc/inventory.csv" (unlines csv)
  putStrLn "Wrote ./doc/inventory.csv"
