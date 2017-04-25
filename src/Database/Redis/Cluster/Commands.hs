{-# LANGUAGE OverloadedStrings #-}
module Database.Redis.Cluster.Commands (
hashSlots,
Slot,
addSlots,
NodeId,
countFailureReports,
countKeysInSlot,
delSlots,
FailoverOptions,
failover,
forget,
Count,
getKeysInSlot,
Info,
info,
keyslot,
meet, meet',
NodeInfo,
nodes,
replicate,
reset,
saveConfig,
Epoch,
setConfigEpoch,
SetSlotSubcommand,
setSlot,
slaves,
slots,
readonly,
readwrite
) where

import Data.ByteString (ByteString)
import Database.Redis (RedisCtx, Reply, Status, HostName, PortID)
import Network.Socket (PortNumber)
import Prelude hiding (replicate)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Maybe as Maybe
import qualified Database.Redis as Redis

hashSlots :: Int
hashSlots = 2^14

type Slot = Int

addSlots :: (RedisCtx m f) => [Slot] -> m (f Status)
addSlots = undefined -- FIXME IMPLEMENT

type NodeId = Int

countFailureReports :: (RedisCtx m f) => NodeId -> m (f Int)
countFailureReports = undefined -- FIXME IMPLEMENT

countKeysInSlot :: (RedisCtx m f) => Slot -> m (f Int)
countKeysInSlot = undefined -- FIXME IMPLEMENT

delSlots :: (RedisCtx m f) => [Slot] -> m (f Status)
delSlots = undefined -- FIXME IMPLEMENT

data FailoverOptions = Force | Takeover

failover :: (RedisCtx m f) =>  Maybe FailoverOptions -> m (f Status)
failover = undefined -- FIXME IMPLEMENT

forget :: (RedisCtx m f) => NodeId -> m (f Status)
forget = undefined -- FIXME IMPLEMENT

type Count = Int

getKeysInSlot :: (RedisCtx m f) => Slot -> Count -> m (f [ByteString])
getKeysInSlot = undefined -- FIXME IMPLEMENT

data Info = Info -- FIXME IMPLEMENT PARSING

info :: (RedisCtx m f) => m (f Info)
info = undefined -- FIXME IMPLEMENT

keyslot :: (RedisCtx m f) => m (f Slot)
keyslot = undefined -- FIXME IMPLEMENT

meet :: (RedisCtx m f) => HostName -> PortNumber -> m (f Status)
meet host port = do
  let host' = Char8.pack host
      port' = Char8.pack $ show port
  Redis.sendRequest ["CLUSTER", "MEET", host', port']

-- | Wrap meet for PortID instead of PortNumber.
meet' :: (RedisCtx m f) => HostName -> PortID -> m (f Status)
meet' host port =
  let port' = toClusterPortNumber port
  in maybe (fail "Invalid PortID given.") (meet host) port'
  where
    toClusterPortNumber :: PortID -> Maybe PortNumber
    toClusterPortNumber (Redis.Service _)    = Nothing
    toClusterPortNumber (Redis.UnixSocket _) = Nothing
    toClusterPortNumber (Redis.PortNumber p) = Just $ 10000 + p

data NodeInfo = NodeInfo -- FIXME IMPLEMENT PARSING

nodes :: (RedisCtx m f) => m (f [NodeInfo])
nodes = undefined -- FIXME IMPLEMENT

replicate :: (RedisCtx m f) => NodeId -> m (f Status)
replicate = undefined -- FIXME IMPLEMENT

data ResetOptions = Soft | Hard

reset :: (RedisCtx m f) => ResetOptions -> m (f Status)
reset = undefined -- FIXME IMPLEMENT

saveConfig :: (RedisCtx m f) => m (f Status)
saveConfig = undefined -- FIXME IMPLEMENT

type Epoch = Int

setConfigEpoch :: (RedisCtx m f) => Epoch -> m (f Status)
setConfigEpoch = undefined -- FIXME IMPLEMENT

data SetSlotSubcommand = Importing NodeId | Migrating NodeId | Stable | Node NodeId

setSlot :: (RedisCtx m f) => Slot -> SetSlotSubcommand -> m (f Status)
setSlot = undefined -- FIXME IMPLEMENT

slaves :: (RedisCtx m f) => NodeId -> m (f [NodeInfo])
slaves = undefined -- FIXME IMPLEMENT

data SlotMap = SlotMap -- FIXME IMPLEMENT SlotMap

slots :: (RedisCtx m f) => m (f SlotMap)
slots = undefined -- FIXME IMPLEMENT

readonly :: (RedisCtx m f) => m (f Status)
readonly = undefined -- FIXME IMPLEMENT

readwrite :: (RedisCtx m f) => m (f Status)
readwrite = undefined -- FIXME IMPLEMENT
