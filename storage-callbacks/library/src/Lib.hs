{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.ByteString         (ByteString)
import           Data.ByteString.Char8   (putStrLn)
import           GHC.Generics            hiding (from)
import           Prelude                 hiding (putStrLn)

data State m a = State
  {
    _nextValue     :: MVar a
  , _applyLogEntry :: a -> CmdRequest -> m (a, CmdResponse)
  }

newtype CmdRequest = CmdRequest { unCmdRequest :: ByteString }
  deriving (Show, Eq)

newtype CmdResponse = CmdResponse { unCmdResponse :: ByteString }
  deriving (Show, Eq)

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
