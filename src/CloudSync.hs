-----------------------------------------------------------------------------
--
-- Module      :  CloudSync
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE  TemplateHaskell, RecordWildCards, DeriveDataTypeable, RankNTypes
    , DeriveGeneric, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}
module CloudSync where

import  qualified Data.HashTable.IO as H
import Data.TCache
import Data.TCache.DefaultPersistence
import qualified Data.Map as M
import Data.IORef
import Control.Concurrent.MVar
import Data.Typeable
import Control.Monad
import Control.Monad.Primitive
import System.IO.Unsafe
import Data.List(span,insert)
import Data.ByteString.Lazy.Char8(pack,unpack)
import EventSourcing
import Control.Workflow
import Control.Distributed.Process
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)
import System.Time

import Data.Maybe
import Data.List
import Data.Char
import Data.Binary
import GHC.Generics(Generic)
import System.Time(ClockTime(..))
import Data.RefSerialize
import Data.Monoid
import Text.Printf
import Debug.Trace

(!>)= flip trace

data Stamp = Stamp{stime ::ClockTime , nodeid ::NodeId} deriving (Show, Eq, Ord,Generic,Typeable)

stamp0 nid= Stamp (TOD 0 0) nid
instance Binary Stamp



atomic= liftIO . atomically

data Condata= Condata {cstamp :: Stamp, cpid :: Maybe ProcessId} deriving (Show,Typeable,Generic)
newtype Nodes= Nodes (M.Map NodeId Condata)  deriving (Typeable,Show,Generic)
refNodes :: DBRef Nodes
refNodes= getDBRef keyNodes
keyNodes= "Nodes"

--getServer :: (NodeId,Condata) -> Process(NodeId,Condata)
--getServer (node,cdata)= do
--    nid <- getSelfNode    !> "getServer called"
--    atomic $ do
--      Nodes ts <- readDBRef refNodes `onNothing` return (Nodes M.empty)
--      writeDBRef refNodes . Nodes $ M.union (M.singleton node cdata) ts
--      return (nid,fromJust $ M.lookup nid ts )
--
--
--remotable [ 'getServer ]

--connectNode :: (NodeId,Condata) -> Process(NodeId,Condata)

-- do
--        (nid',cdata') <- call $(functionTDict 'getServer ) nid  ($(mkClosure 'getServer) () )
--                          !> ("CALL connectNode " ++ show nid)
--        return (nid,cdata) !> "return CALL"



--instance Read NodeId where
--  readsPrec n s= if "nid://" `isPrefixOf` s
--    then
--        let (s',s'')= break (not . isSpace) $ drop 6 s
--        in [(NodeId (read s'), tail s'')]
--    else error "read Nodeid"

--instance Binary a => Serializable a where
--   serialize = encode
--   deserialize = decode



deriving instance Generic ClockTime

instance Binary ClockTime



data CEvent a = CEvent{stamp :: Stamp, payload :: a}
  deriving (Show, Typeable,Generic,Eq)

instance Eq a => Ord (CEvent a) where
  compare (CEvent t _) (CEvent t' _)= compare t t'

instance Binary a => Binary (CEvent a)

class Processable a where
   process :: a -> IO ()

data EventList a= EventList{ events:: [CEvent a]}

eventCache :: forall a.H.BasicHashTable  String ([CEvent a],DBRef Stat)
eventCache= unsafePerformIO $   H.new


instance Binary Condata where
   put (Condata s _)= put s
   get = get >>= \s -> return (Condata s Nothing)

instance Binary Nodes

instance Indexable Nodes where key= const keyNodes

instance Serializable Nodes where
   serialize = encode
   deserialize = decode


data Event a= Event{consolidated:: Stamp, event :: CEvent a} deriving (Typeable, Generic,Show)

instance Binary a => Binary (Event a)


processEvent
  :: (Ord a, Typeable a,Show a,
      Indexable  a,
      Data.RefSerialize.Serialize a) =>
     NodeId -> Event a -> IO ()

processEvent mynode e@Event{..}= do
   t <- getConsolidated mynode e                                            --   !> ("processEvent" ++ show (payload event))
   let k= key $ payload  event
   mr <- H.lookup  eventCache  k                                             --   !> "lookup"
   ((tosave, es'),rst) <- case mr of
     Nothing -> do
       rst <- exec1nc k  getWFStat
       return (([],[event]),rst)                                                  -- !> "NOTHING processevent"
     Just (es,rst) -> return (span (< CEvent{stamp=t}) $ insert event  es, rst)   -- !> "Just processEvent"

   when (not $ null tosave) $ logEvents rst (map payload tosave) >> return ()     -- !> "logevent"
   when (not $ null es') $ H.insert eventCache k (es',rst)                         -- !> "insert eventCache"

logEvents rst = mapM (logEvent rst)                                               -- !> "LogEvents"

getConsolidated mynode Event{..} = atomically $ do
    Nodes ts <- readDBRef refNodes `onNothing` (return (Nodes M.empty))
    let node = nodeid consolidated
        Condata _ mpid =  fromMaybe  (Condata undefined Nothing) $ M.lookup node ts
        ts'= M.insert node (Condata consolidated mpid) ts                          !> "insert consolidated"
    writeDBRef refNodes $ Nodes ts'
    let t=  minimum . filter (\st -> stime st /= TOD 0 0) . map cstamp $ M.elems ts'
    let cdata = fromMaybe  (Condata undefined Nothing) $ M.lookup mynode ts'
    writeDBRef refNodes . Nodes $ M.insert mynode cdata{cstamp= t} ts'             !> "write refconsolidated"
    return t !>  ("consolidated="++show t)

--
--initSync backend serv = do
--  mynode <- getSelfNode
--  mypid  <- spawn mynode serv
--  register "CloudSync" mypid
--
--  peers0 <- liftIO $ findPeers backend 1000000
--  let peers = filter (/= mynode) peers0


initSync serv test peers = do
  mynode <- getSelfNode

--  let peers = filter (/= mynode) peers0  !> ("mynode=" ++ show mynode)
  say ("peers are " ++ show peers)
--  Nodes ns <- atomic $ readDBRef refNodes `onNothing` return (Nodes M.empty)
--  let cdata = fromMaybe (Condata (stamp0 mynode) (Just mypid)) $ M.lookup mynode ns
--      ns' = M.insert mynode cdata{cpid=Just mypid} ns



  Nodes ns <- atomic $ readDBRef refNodes `onNothing` return (Nodes M.empty)
  list <- connectNodes serv ns peers

  test serv

connectNodes server ns peers = forM peers $ connectNode server ns

connectNode server ns node= do
    case fromMaybe (Condata (stamp0 node) Nothing) $ M.lookup node ns of
      Condata _ (Just _) -> return ()
      cdata -> do
       pid  <- spawn node server  !> ("connectNode="++ show node)
       atomic $ writeDBRef refNodes . Nodes $ M.insert node cdata{cpid= Just pid} ns

