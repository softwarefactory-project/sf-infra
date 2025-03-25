-- | The zuul-info/inventory.yaml data type
module ZuulInventory where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

type Hostname = Text
newtype Inventory = Inventory {all :: InventoryAll}
    deriving (Show, Generic)
instance FromJSON Inventory
instance ToJSON Inventory

data InventoryAll = InventoryAll
    { hosts :: Map Hostname Host
    , vars :: Map Text Value
    }
    deriving (Show, Generic)
instance FromJSON InventoryAll
instance ToJSON InventoryAll

data Host = Host
    { ansible_host :: Text
    , nodepool :: Node
    }
    deriving (Show, Generic)
instance FromJSON Host
instance ToJSON Host

data Node = Node
    { label :: Text
    , host_id :: Maybe Text
    }
    deriving (Show, Generic)
instance FromJSON Node
instance ToJSON Node
