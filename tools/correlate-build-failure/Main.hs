{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as LText
import Data.Vector qualified as V
import Data.Yaml qualified as YAML
import GHC.Generics (Generic)
import Gerrit qualified
import Gerrit.Data.Change qualified as Gerrit
import Main.Utf8 (withUtf8)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import System.Cached.JSON qualified as Cache
import System.Environment (getEnv)

main :: IO ()
main = withUtf8 $ do
    -- Collect build results
    changeNR <- read <$> getEnv "GERRIT_CHANGE"
    results <- Gerrit.withClient "https://softwarefactory-project.io/r/" Nothing $ getBuildResultFromGerrit changeNR
    -- Collect build log_url
    builds <- traverse getBuild results
    -- Collect hostids
    nodeIDs <- traverse getNodeIDS builds
    -- Collect failed task name
    tasks <- traverse getFailedTasks builds
    -- Make summary
    let infos = zipWith3 BuildInfo results nodeIDs tasks
    mapM_ (Text.putStrLn . formatResult) infos
    Text.putStrLn $ summaryPerHost infos

data BuildResult = BuildResult {url :: Text, status :: Text} deriving (Show)

getBuildResultFromGerrit :: Int -> Gerrit.GerritClient -> IO [BuildResult]
getBuildResultFromGerrit changeNR client = do
    mChange <- Gerrit.getChange changeNR client
    case mChange of
        Nothing -> putStrLn ("Unknown change: " <> show changeNR) >> pure []
        Just c -> pure $ concatMap readBuildResult c.messages

readBuildResult :: Gerrit.GerritChangeMessage -> [BuildResult]
readBuildResult m = mapMaybe readMessageLine $ Text.lines m.mMessage
  where
    readMessageLine l = case Text.words l of
        ["-", _job, url, ":", status, "in", _m, _s] -> Just (BuildResult{url, status})
        _ -> Nothing

data Build = Build {log_url :: Maybe Text, uuid, result :: Text} deriving (Show, Generic)
instance Aeson.FromJSON Build
instance Aeson.ToJSON Build

getBuild :: BuildResult -> IO Build
getBuild br = Cache.getCachedJSON cacheName name url 1_000_000
  where
    name = Text.unpack $ "build-" <> Text.takeWhileEnd (/= '/') br.url
    url = Text.unpack $ Text.replace "/t/" "/api/tenant/" br.url

newtype NodeIDs = NodeIDs (Map.Map Text (Maybe Text)) deriving (Show)
instance Aeson.ToJSON NodeIDs where
    toJSON (NodeIDs m) = Aeson.object ["all" .= Aeson.object ["hosts" .= fmap toNode m]]
      where
        toNode n = Aeson.object ["nodepool" .= Aeson.object ["host_id" .= n]]
instance Aeson.FromJSON NodeIDs where
    parseJSON = Aeson.withObject "Inventory" $ \v -> do
        all' <- v .: "all"
        (hosts :: Map.Map Text Aeson.Object) <- all' .: "hosts"
        nodes <- mapM getNodeIDs hosts
        pure $ NodeIDs nodes
      where
        getNodeIDs n = do
            node <- n .: "nodepool"
            node .: "host_id"

getNodeIDS :: Build -> IO NodeIDs
getNodeIDS (Build (Just log_url) uuid _) = Cache.getCachedJSONQuery cacheName name (getYAML url) 1_000_000
  where
    name = Text.unpack $ "inventory-" <> uuid
    url = Text.unpack $ log_url <> "zuul-info/inventory.yaml"
getNodeIDS _ = pure $ NodeIDs mempty

data FailedTask = FailedTask {host :: Text, task :: Text, output :: Text} deriving (Show, Generic)
instance Aeson.ToJSON FailedTask
newtype FailedTasks = FailedTasks [FailedTask] deriving (Show)
instance Aeson.ToJSON FailedTasks where
    toJSON (FailedTasks v) = Aeson.toJSON $ map toPlay v
      where
        toPlay t = Aeson.object ["plays" .= [Aeson.object ["tasks" .= [toTask t]]]]
        toTask t =
            Aeson.object
                [ "hosts" .= Aeson.object [Key.fromText t.host .= Aeson.object ["failed" .= True, "msg" .= t.output]]
                , "task" .= Aeson.object ["name" .= t.task]
                ]
instance Aeson.FromJSON FailedTasks where
    parseJSON = Aeson.withArray "Results" $ \xs ->
        FailedTasks . concat <$> mapM getPlays (V.toList xs)
      where
        getPlays = Aeson.withObject "Play" $ \v -> do
            (plays :: [Aeson.Object]) <- v .: "plays"
            concat <$> mapM getPlayTasks plays
        getPlayTasks p = do
            (tasks :: [Aeson.Object]) <- p .: "tasks"
            concat <$> mapM getTasks tasks
        getTasks t = do
            (mHosts :: Maybe (Map.Map Text Aeson.Object)) <- t Aeson..:? "hosts"
            case mHosts of
                Nothing -> pure []
                Just hosts -> do
                    failed <- filterM isFailed $ Map.toList hosts
                    case failed of
                        [] -> pure []
                        xs -> do
                            task <- t .: "task"
                            name <- task .: "name"
                            concat <$> mapM (getTasksOutput name) xs
        isFailed (_host, obj) = fromMaybe False <$> obj Aeson..:? "failed"
        getTasksOutput task (host, obj) = do
            stdout <- obj Aeson..:? "stdout"
            output <- case stdout of
                Just s -> pure s
                Nothing -> obj .: "msg"
            pure [FailedTask{host, task, output}]

getFailedTasks :: Build -> IO FailedTasks
getFailedTasks (Build (Just log_url) uuid result)
    | result /= "SUCCESS" = Cache.getCachedJSON cacheName name url 1_000_000
  where
    name = Text.unpack $ "tasks-" <> uuid
    url = Text.unpack $ log_url <> "job-output.json"
getFailedTasks _ = pure $ FailedTasks mempty

formatResult :: BuildInfo -> Text
formatResult (BuildInfo br (NodeIDs nodeIDs) (FailedTasks tasks)) = Text.unwords [br.url, br.status, encode nodeIDs, encode shortTasks]
  where
    shortTasks = map (\ft -> ft{output = lastLines 20 ft.output}) tasks
    lastLines count = Text.unlines . reverse . take count . reverse . Text.lines
    encode :: (Aeson.ToJSON a) => a -> Text
    encode = LText.toStrict . LText.decodeUtf8 . Aeson.encode

getYAML :: (Aeson.FromJSON a) => String -> IO a
getYAML url = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    request <- HTTP.parseRequest url
    response <- HTTP.httpLbs request manager
    YAML.decodeThrow $ LBS.toStrict response.responseBody

data BuildInfo = BuildInfo BuildResult NodeIDs FailedTasks

summaryPerHost :: [BuildInfo] -> Text
summaryPerHost xs = Text.unlines $ map perHost hosts
  where
    perHost h = Text.unwords [h, "success:" <> tshow successCount, "failed:" <> tshow failedCount, tshow $ Map.toList tasksNames]
      where
        builds = filter (\(BuildInfo _ (NodeIDs nodes) _) -> Just h `elem` Map.elems nodes) xs
        successCount = length $ filter (\(BuildInfo br _ _) -> br.status == "SUCCESS") builds
        failedTasks = concatMap (\(BuildInfo _ _ (FailedTasks ts)) -> ts) builds
        failedCount = length builds - successCount
        tasksNames = foldl' (\x a -> Map.insertWith (+) a.task (1 :: Int) x) Map.empty failedTasks
    hosts = Set.toList $ Set.fromList $ concatMap (catMaybes . getHost) xs
      where
        getHost (BuildInfo _ (NodeIDs nodes) _) = snd <$> Map.toList nodes

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

cacheName :: String
cacheName = "zuul-correlate-builds"
