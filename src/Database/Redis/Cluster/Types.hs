{-# LANGUAGE OverloadedStrings #-}
module Database.Redis.Cluster.Types (
Slot,
NodeId,
FailoverOptions(..),
Count,
Info(..),
Key,
NodeInfo(..),
LinkState(..),
NodeSlot(..),
ResetOptions(..),
Epoch,
SetSlotSubcommand(..),
SlotMap,
) where

import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Monoid ((<>))
import Database.Redis (HostName)
import Network.Socket (PortNumber)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Database.Redis as Redis

type Slot = Integer

type NodeId = ByteString

data FailoverOptions = Force | Takeover

type Count = Int

data Info = Info -- FIXME IMPLEMENT PARSING

type Key = ByteString

data NodeInfo = NodeInfo {
  nodeId :: NodeId,
  hostName :: HostName,
  port :: PortNumber,
  flags :: (),
  pingSent :: Integer,
  pongRecv :: Integer,
  configEpoch :: Epoch,
  linkState :: LinkState,
  slots :: [NodeSlot]
} -- FIXME IMPLEMENT PARSING

instance Redis.RedisResult NodeInfo where
  decode r@(Redis.SingleLine line) = case Char8.words line of
    (nodeId : hostNamePort : flags' : pingSent' : pongRecv' : epoch : linkState' : slots') ->
      let [hostName, port'] = Char8.split ':' hostNamePort
      in Right $ NodeInfo {
        nodeId = nodeId,
        hostName = Char8.unpack hostName,
        port = read $ Char8.unpack port',
        flags = (), -- FIXME PARSE FLAGS
        -- FIXME ADD related node
        pingSent = readInteger pingSent',
        pongRecv = readInteger pongRecv',
        configEpoch = readInteger epoch,
        linkState = readLinkState linkState',
        slots = fmap readNodeSlot slots'
      }
    _ -> Left r
    where
      readInteger :: ByteString -> Integer
      readInteger = undefined

      readLinkState :: ByteString -> LinkState
      readLinkState "connected" = Connected
      readLinkState "disconnected" = Disconnected
      readLinkState x = error $ "Undefined LinkState: " <> (show x)

      readNodeSlot :: ByteString -> NodeSlot
      readNodeSlot x = case Char8.split '-' x of
        [start, end] -> (SlotRange `on` readInteger) start end
        [slot] -> SingleSlot $ readInteger slot
  decode r = Left r

data LinkState = Connected | Disconnected

data NodeSlot = SingleSlot Slot | SlotRange Slot Slot

{-
07c37dfeb235213a872192d90877d0cd55635b91 127.0.0.1:30004 slave         e7d1eecce10fd6bb5eb35b9f99a514335d9ba9ca 0 1426238317239 4 connected
67ed2db8d677e59ec4a4cefb06858cf2a1a89fa1 127.0.0.1:30002 master        -                                        0 1426238316232 2 connected 5461-10922
292f8b365bb7edb5e285caf0b7e6ddc7265d2f4f 127.0.0.1:30003 master        -                                        0 1426238318243 3 connected 10923-16383
6ec23923021cf3ffec47632106199cb7f496ce01 127.0.0.1:30005 slave         67ed2db8d677e59ec4a4cefb06858cf2a1a89fa1 0 1426238316232 5 connected
824fe116063bc5fcf9f4ffd895bc17aee7731ac3 127.0.0.1:30006 slave         292f8b365bb7edb5e285caf0b7e6ddc7265d2f4f 0 1426238317741 6 connected
e7d1eecce10fd6bb5eb35b9f99a514335d9ba9ca 127.0.0.1:30001 myself,master -                                        0             0 1 connected 0-5460
-}

data ResetOptions = Soft | Hard

type Epoch = Integer

data SetSlotSubcommand = Importing NodeId | Migrating NodeId | Stable | Node NodeId

data SlotMap = SlotMap -- FIXME IMPLEMENT SlotMap Redis.RedisResult instance
