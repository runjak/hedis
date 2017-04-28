{-# LANGUAGE OverloadedStrings #-}
module Database.Redis.Cluster.Types (
Slot,
NodeId,
FailoverOptions(..),
Count,
Info(..),
Key,
NodeInfo(..),
NodeInfoFlag(..),
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
import qualified Data.Maybe as Maybe
import qualified Database.Redis as Redis

type Slot = Integer

type NodeId = ByteString

data FailoverOptions = Force | Takeover

type Count = Int

data Info = Info -- FIXME IMPLEMENT PARSING

type Key = ByteString

data NodeInfo = NodeInfo {
  nodeId       :: NodeId,
  hostName     :: HostName,
  port         :: PortNumber,
  flags        :: [NodeInfoFlag],
  masterNodeId :: Maybe NodeId,
  pingSent     :: Integer,
  pongRecv     :: Integer,
  configEpoch  :: Epoch,
  linkState    :: LinkState,
  slots        :: [NodeSlot]
} deriving (Show, Eq, Ord)

data NodeInfoFlag =
    Myself    -- | The node you are contacting.
  | Master    -- | Node is a master.
  | Slave     -- | Node is a slave.
  | Pfail     -- | Node is in PFAIL state. Not reachable for the node you a re contacting, but still logically reachable (not in FAIL state).
  | Fail      -- | Node is in FAIL state. It was not reachable for multiple nodes that promoted the PFAIL state to FAIL.
  | Handshake -- | Untrusted node, we are handshaking.
  | NoAddr    -- | No address known for this node.
  | NoFlags   -- | No flags at all.
  deriving (Show, Eq, Ord)

instance Redis.RedisResult NodeInfo where
  decode r@(Redis.SingleLine line) = case Char8.words line of
    (nodeId : hostNamePort : flags' : masterNodeId' : pingSent' : pongRecv' : epoch : linkState' : slots') ->
      let [hostName, port'] = Char8.split ':' hostNamePort
      in Right $ NodeInfo {
        nodeId       = nodeId
      , hostName     = Char8.unpack hostName
      , port         = read $ Char8.unpack port'
      , flags        = readNodeFlags flags'
      , masterNodeId = readMasterNodeId masterNodeId'
      , pingSent     = readInteger pingSent'
      , pongRecv     = readInteger pongRecv'
      , configEpoch  = readInteger epoch
      , linkState    = readLinkState linkState'
      , slots        = fmap readNodeSlot slots'
      }
    _ -> Left r
    where
      readInteger :: ByteString -> Integer
      readInteger = fst . Maybe.fromJust . Char8.readInteger

      readLinkState :: ByteString -> LinkState
      readLinkState "connected"    = Connected
      readLinkState "disconnected" = Disconnected
      readLinkState x = error $ "Undefined LinkState: " <> (show x)

      readNodeSlot :: ByteString -> NodeSlot
      readNodeSlot x = case Char8.split '-' x of
        [start, end] -> (SlotRange `on` readInteger) start end
        [slot]       -> SingleSlot $ readInteger slot

      readMasterNodeId :: ByteString -> Maybe NodeId
      readMasterNodeId "-"    = Nothing
      readMasterNodeId nodeId = Just nodeId

      readNodeFlags :: ByteString -> [NodeInfoFlag]
      readNodeFlags = Maybe.catMaybes . fmap go . Char8.split ','
        where
          go :: ByteString -> Maybe NodeInfoFlag
          go "myself"    = Just Myself
          go "master"    = Just Master
          go "slave"     = Just Slave
          go "fail?"     = Just Pfail
          go "fail"      = Just Fail
          go "handshake" = Just Handshake
          go "noaddr"    = Just NoAddr
          go "noflags"   = Just NoFlags
          go _           = Nothing
  decode r = Left r

data LinkState =
    Connected
  | Disconnected
  deriving (Show, Eq, Ord)

data NodeSlot =
    SingleSlot Slot
  | SlotRange Slot Slot
  deriving (Show, Eq, Ord)

testData :: Redis.Reply
testData = Redis.MultiBulk . Just $ fmap Redis.SingleLine [
    "07c37dfeb235213a872192d90877d0cd55635b91 127.0.0.1:30004 slave         e7d1eecce10fd6bb5eb35b9f99a514335d9ba9ca 0 1426238317239 4 connected"
  , "67ed2db8d677e59ec4a4cefb06858cf2a1a89fa1 127.0.0.1:30002 master        -                                        0 1426238316232 2 connected 5461-10922"
  , "292f8b365bb7edb5e285caf0b7e6ddc7265d2f4f 127.0.0.1:30003 master        -                                        0 1426238318243 3 connected 10923-16383"
  , "6ec23923021cf3ffec47632106199cb7f496ce01 127.0.0.1:30005 slave         67ed2db8d677e59ec4a4cefb06858cf2a1a89fa1 0 1426238316232 5 connected"
  , "824fe116063bc5fcf9f4ffd895bc17aee7731ac3 127.0.0.1:30006 slave         292f8b365bb7edb5e285caf0b7e6ddc7265d2f4f 0 1426238317741 6 connected"
  , "e7d1eecce10fd6bb5eb35b9f99a514335d9ba9ca 127.0.0.1:30001 myself,master -                                        0             0 1 connected 0-5460"
  ]

testDecode :: Either Redis.Reply [NodeInfo]
testDecode = Redis.decode testData

data ResetOptions = Soft | Hard

type Epoch = Integer

data SetSlotSubcommand = Importing NodeId | Migrating NodeId | Stable | Node NodeId

data SlotMap = SlotMap -- FIXME IMPLEMENT SlotMap Redis.RedisResult instance
