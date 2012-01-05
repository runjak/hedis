{-# LANGUAGE OverloadedStrings, CPP #-}

module Database.Redis (
    
    -- * The Redis Monad
    Redis(), runRedis,
    
    -- * Connection
    RedisConn, connect, disconnect,
    HostName,PortID(..),
    
    -- * Low-Level Requests and Replies
    Reply(..),
    sendRequest,
    -- |'sendRequest' can be used to implement one of the unimplemented 
    --  commands, as shown below.
    --
    -- @
    -- -- |Redis DEBUG OBJECT command
    -- debugObject :: (RedisArg key, RedisResult a) => key -> Redis a
    -- debugObject key = sendRequest [\"DEBUG\", \"OBJECT\", encode key]
    -- @
    --
    
    -- * PubSub
    module Database.Redis.PubSub,
    module Database.Redis.Types,
    -- * Commands
	module Database.Redis.Commands

) where

import Database.Redis.Internal
import Database.Redis.PubSub
import Database.Redis.Reply
import Database.Redis.Types

import Database.Redis.Commands
