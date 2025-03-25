{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson qualified as Aeson
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
import Data.Yaml qualified as YAML
import GHC.Generics (Generic)
import Gerrit qualified
import Gerrit.Data.Change qualified as Gerrit
import Main.Utf8 (withUtf8)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import System.Cached.JSON qualified as Cache
import System.Environment (getEnv)

import JobOutput
import ZuulInventory

main :: IO ()
main = withUtf8 $ do
    -- Collect build results
    changeNR <- read <$> getEnv "GERRIT_CHANGE"
    results <- Gerrit.withClient "https://softwarefactory-project.io/r/" Nothing $ getBuildResultFromGerrit changeNR
    -- Collect build log_url
    builds <- traverse getBuild results
    -- Collect kernel versions (TODO: adapt this for non minikube jobs)
    kernels <- traverse getKernelVersion builds
    -- Collect hostids
    nodeIDs <- traverse getNodeIDS builds
    -- Collect failed task name
    tasks <- traverse getFailedTasks builds
    -- Make summary
    let infos = zipWith3 BuildInfo results (zip nodeIDs kernels) tasks
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

getKernelVersion :: Build -> IO (Maybe KernelVersion)
getKernelVersion (Build (Just log_url) uuid _) = Cache.getCachedJSONQuery cacheName name (readKernelVersion <$> getText url) 1_000_000
  where
    name = Text.unpack $ "kernel-" <> uuid
    url = Text.unpack $ log_url <> "minikube.logs"
getKernelVersion _ = pure Nothing

newtype KernelVersion = KernelVersion Text deriving newtype (Show, Eq, Aeson.ToJSON, Aeson.FromJSON)
readKernelVersion :: Text -> Maybe KernelVersion
readKernelVersion = go . Text.lines
  where
    go [] = Nothing
    go (x : rest)
        | "  Kernel Version:" `Text.isPrefixOf` x = Just $ KernelVersion $ last $ Text.words x
        | otherwise = go rest

newtype NodeIDs = NodeIDs (Map.Map Hostname (Maybe Text)) deriving (Show)

decodeNodeIDs :: Inventory -> NodeIDs
decodeNodeIDs i = NodeIDs $ fmap (\h -> h.nodepool.host_id) i.all.hosts

getNodeIDS :: Build -> IO NodeIDs
getNodeIDS (Build (Just log_url) uuid _) = decodeNodeIDs <$> Cache.getCachedJSONQuery cacheName name (getYAML url) 1_000_000
  where
    name = Text.unpack $ "inventory-" <> uuid
    url = Text.unpack $ log_url <> "zuul-info/inventory.yaml"
getNodeIDS _ = pure $ NodeIDs mempty

data FailedTask = FailedTask {host, name :: Text, output :: Maybe Text} deriving (Show, Generic)
instance Aeson.ToJSON FailedTask

decodeFailedTasks :: [JobPlaybook] -> [FailedTask]
decodeFailedTasks = concatMap getPlays
  where
    getPlays :: JobPlaybook -> [FailedTask]
    getPlays jp = concatMap getPlayTasks jp.plays
    getPlayTasks :: JobPlay -> [FailedTask]
    getPlayTasks jp = concatMap getTasks jp.tasks
    getTasks :: JobTask -> [FailedTask]
    getTasks jt = map toFailedTask $ filter isFailed $ Map.toList jt.hosts
      where
        name = jt.task.name
        toFailedTask :: (Hostname, TaskResult) -> FailedTask
        toFailedTask (host, tr) = FailedTask host name (tr.stdout <|> tr.msg)

    isFailed (_host, tr) = fromMaybe False tr.failed

getFailedTasks :: Build -> IO [FailedTask]
getFailedTasks (Build (Just log_url) uuid result)
    | result /= "SUCCESS" = decodeFailedTasks <$> Cache.getCachedJSON cacheName name url 1_000_000
  where
    name = Text.unpack $ "tasks-" <> uuid
    url = Text.unpack $ log_url <> "job-output.json"
getFailedTasks _ = pure []

formatResult :: BuildInfo -> Text
formatResult (BuildInfo br (NodeIDs nodeIDs, kernel) tasks) =
    Text.unwords [br.url, br.status, encode kernel, encode nodeIDs, encode shortTasks]
  where
    shortTasks = map (\ft -> ft{output = lastLines 20 <$> ft.output}) tasks
    lastLines count = Text.unlines . reverse . take count . reverse . Text.lines
    encode :: (Aeson.ToJSON a) => a -> Text
    encode = LText.toStrict . LText.decodeUtf8 . Aeson.encode

getRAW :: String -> IO LBS.ByteString
getRAW url = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    request <- HTTP.parseRequest url
    response <- HTTP.httpLbs request manager
    pure response.responseBody

getText :: String -> IO Text
getText url = LText.toStrict . LText.decodeUtf8 <$> getRAW url

getYAML :: (Aeson.FromJSON a) => String -> IO a
getYAML url = YAML.decodeThrow . LBS.toStrict =<< getRAW url

data BuildInfo = BuildInfo BuildResult (NodeIDs, Maybe KernelVersion) [FailedTask]

summaryPerHost :: [BuildInfo] -> Text
summaryPerHost xs = Text.unlines $ map perHost hosts
  where
    perHost h = Text.unwords [h, "success:" <> tshow successCount, "failed:" <> tshow failedCount, tshow $ Map.toList tasksNames]
      where
        builds = filter (\(BuildInfo _ (NodeIDs nodes, _) _) -> Just h `elem` Map.elems nodes) xs
        successCount = length $ filter (\(BuildInfo br _ _) -> br.status == "SUCCESS") builds
        failedTasks = concatMap (\(BuildInfo _ _ ts) -> ts) builds
        failedCount = length builds - successCount
        tasksNames = foldl' (\x task -> Map.insertWith (+) task.name (1 :: Int) x) Map.empty failedTasks
    hosts = Set.toList $ Set.fromList $ concatMap (catMaybes . getHost) xs
      where
        getHost (BuildInfo _ (NodeIDs nodes, _) _) = snd <$> Map.toList nodes

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

cacheName :: String
cacheName = "zuul-correlate-builds"
