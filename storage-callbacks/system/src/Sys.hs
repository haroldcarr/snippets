{-# LANGUAGE OverloadedStrings #-}

module Sys where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.ByteString         (ByteString)
import           Data.ByteString.Char8   (putStrLn)
import           Prelude                 hiding (putStrLn)

type CmdRequest  = ByteString
type CmdResponse = ByteString

data State m a = State
  {
    _nextValue     :: MVar a
  , _applyLogEntry :: a -> CmdRequest -> m (a, CmdResponse)
  }

mkState :: MonadIO m
        => a
        -> ((String -> IO ()) -> a -> CmdRequest -> IO (a, CmdResponse))
        -> IO (State m a)
mkState nextValue applyFn = do
  mv <- newMVar nextValue
  return State { _nextValue     = mv
               , _applyLogEntry = liftIO2 (applyFn (\_ -> return ()))
               }

run :: State IO a
    -> CmdRequest -> IO CmdResponse
run s c = do
  a              <- takeMVar (_nextValue s)
  (a', response) <- _applyLogEntry s a c
  _              <- putMVar (_nextValue s) a'
  return response

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f a = liftIO . f a
