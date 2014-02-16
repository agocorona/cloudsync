{-# LANGUAGE TemplateHaskell,DeriveDataTypeable, RecordWildCards, DeriveGeneric
 , UndecidableInstances , FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.TCache
import Data.TCache.Defs
import Data.TCache.IndexQuery
import Control.Workflow.Stat
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)

import System.Environment
--import Network.Socket hiding (shutdown)

import Data.Binary
import Control.Concurrent
import Data.Typeable
import GHC.Generics
import CloudSync
import Text.Printf
import System.Time
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.RefSerialize
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Unsafe.Coerce


instance Read Stamp where
   readsPrec n s= undefined

instance Indexable Stamp where key = const "stamp"

instance  Serialize Stamp where
    showp= showpText
    readp= readpText

instance Serializable Stamp where
    serialize= pack . show
    deserialize= read . unpack

server ::  Process ()
server = do
  mynode <- getSelfNode
  server' mynode
  where
  server' mynode= do
      e <- expect
      say $ printf "message received from %s" (show . nodeid  . stamp $ event e)

      liftIO $ processEvent mynode ( e :: Event Stamp)
      server' mynode

remotable [ 'server ]


test1 serv= do
  myid <- getSelfNode

  loop myid
  where
  loop myid = do
      Nodes ns <- atomic $ readDBRef refNodes `onNothing` return (Nodes M.empty)
      let peers= M.keys ns
      connectNodes serv ns peers
      let mps = map cpid $ M.elems ns
          ps = catMaybes mps
      if not . null $ filter isNothing mps then say $ "NOT CONNECTED "++ ( show $ filter isNothing mps)  else do
       forM_ ps $ \to -> do
          time <- liftIO $ getClockTime
          let stamp= Stamp time myid
          send to Event{consolidated= stamp
                       ,event= CEvent stamp  stamp}    !> "send"

      liftIO $ threadDelay 5000000

      loop myid

main :: IO ()
main = do

   args <- getArgs
   case args of
     [host, port] -> do
       backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
       forkIO $ startMaster backend (initSync ( $(mkStaticClosure 'server))  test1)
       startSlave backend



--     ["slave",host, port] -> do
--       backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
--       startSlave backend

--main1 = do
-- [port] <- getArgs
---- index wfName
-- backend <- initializeBackend "localhost" port
--                              (Main.__remoteTable initRemoteTable)
-- node <- newLocalNode backend
--
-- runProcess node (initSync ( $(mkStaticClosure 'server)) backend)
-- liftIO $ threadDelay 100000000
---- runProcess node test1
-- print "END main"





--test= do
--  forkIO $ loop 2000000 2
--  loop 1000000 1
--
--loop t n=do
--  time <- liftIO  getClockTime
--  liftIO $ threadDelay t
--  let stamp= Stamp time n
--  processEvent $ Event{consolidated= stamp
--                      ,event= CEvent stamp  stamp} !> "send"
--
--  loop t n

