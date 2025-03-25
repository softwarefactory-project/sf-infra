{-# LANGUAGE DuplicateRecordFields #-}

-- | This module contains the data model for job-output.json
module JobOutput where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import ZuulInventory (Hostname)

type JobOutput = [JobPlaybook]

data JobPlaybook = JobPlaybook
    { phase :: Text
    , playbook :: Text
    , plays :: [JobPlay]
    , trusted :: Bool
    }
    deriving (Show, Generic)
instance FromJSON JobPlaybook
instance ToJSON JobPlaybook

newtype JobPlay = JobPlay {tasks :: [JobTask]} deriving (Show, Generic)
instance FromJSON JobPlay
instance ToJSON JobPlay

data JobTask = JobTask
    { hosts :: Map Hostname TaskResult
    , task :: TaskInfo
    , role :: Maybe RoleInfo
    }
    deriving (Show, Generic)
instance FromJSON JobTask
instance ToJSON JobTask

data RoleInfo = RoleInfo
    { name :: Text
    , path :: Text
    }
    deriving (Show, Generic)
instance FromJSON RoleInfo
instance ToJSON RoleInfo

newtype TaskInfo = TaskInfo {name :: Text} deriving (Show, Generic)
instance FromJSON TaskInfo
instance ToJSON TaskInfo

data TaskResult = TaskResult
    { action :: Text
    , changed :: Bool
    , failed :: Maybe Bool
    , msg :: Maybe Text
    , stdout :: Maybe Text
    }
    deriving (Show, Generic)
instance FromJSON TaskResult
instance ToJSON TaskResult
