#!/usr/bin/env stack
-- stack script --package network --package bytestring
--
-- Install stack using:
-- sudo dnf copr enable -y petersen/stack2 && sudo dnf install -y stack && sudo stack upgrade
--
-- Start a REPL using:
-- stack repl --package network --package bytestring
-- Prelude> :load udp-multiplexer
-- [1 of 1] Compiling Main             ( udp-multiplexer.hs, interpreted )
-- Main> :type ...
--
-- Build a binary using:
-- stack ghc --package network --package bytestring -- -Wall -O2 -threaded ./udp-multiplexer.hs

import Control.Exception (SomeException, handle)
import Control.Monad (forever, forM)
import Network.Socket hiding (recv)
import Data.ByteString.Internal (ByteString)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)

type Endpoint = (String, Int)

handler :: Endpoint -> IO () -> IO ()
handler (host, port) = handle handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = print $ host <> ":" <> show port <> " " <> show e

runUDPServerForever :: Int -> [Endpoint] -> IO ()
runUDPServerForever listenPort endpoints = do
  addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just $ show listenPort)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  sockets <- mapM (\(host, port) -> client host port) endpoints
  let endpointsAndSocket = zip endpoints sockets
  bind sock (addrAddress serveraddr)
  print $ "Listening on localhost:" <> show listenPort
  forever $ do
    pkt <- recv sock 4096
    forM endpointsAndSocket (sendPacket pkt)
  where
    sendPacket :: ByteString -> (Endpoint, Socket) -> IO ()
    sendPacket pkt (dest, skt) = handler dest $ sendAll skt pkt

client :: String -> Int -> IO Socket
client i p = do
  addrinfos <- getAddrInfo Nothing (Just i) (Just $ show p)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [listenPort, dest1Host, dest1Port, dest2Host, dest2Port] ->
      runUDPServerForever
        (read listenPort)
        [ (dest1Host, (read dest1Port))
        , (dest2Host, (read dest2Port))
        ]
    ["--help"] -> print "usage: listen-port [dest-host dest-port]"
    _ -> print "Not enough arguments"
