{-# LANGUAGE TemplateHaskell #-}
 import System.Environment (getArgs)
 import Control.Distributed.Process
 import Control.Distributed.Process.Closure
 import Control.Distributed.Process.Backend.SimpleLocalnet
 import Control.Distributed.Process.Node (initRemoteTable)
 import Control.Concurrent

 isPrime :: Integer -> Process Bool
 isPrime n = return . (n `elem`) . takeWhile (<= n) . sieve $ [2..]
   where
     sieve :: [Integer] -> [Integer]
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

 remotable ['isPrime]

 master :: [NodeId] -> Process ()
 master [] = liftIO $ putStrLn "no slaves"
 master (slave:_) = do
   isPrime79 <- call $(functionTDict 'isPrime) slave ($(mkClosure 'isPrime) (79 :: Integer))
   liftIO $ print isPrime79

 main :: IO ()
 main = do
   threadDelay 10000000
   args <- getArgs
   case args of
     [host, port] -> do
       backend <- initializeBackend host port rtable
       forkIO $ startMaster backend master
       startSlave backend
   where
     rtable :: RemoteTable
     rtable = __remoteTable initRemoteTable
