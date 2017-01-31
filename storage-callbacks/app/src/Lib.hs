{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.ByteString        as BS hiding (putStrLn)
import           Data.ByteString.Char8  (putStrLn)
import           Prelude                hiding (putStrLn)
import           TheSpec

data AppState
  = AppState ByteString
  deriving (Eq, Show)

appState :: AppState
appState = AppState "harold"

appApplyFn :: (String -> IO ()) -> AppState -> Command -> IO (AppState, CommandResult)
appApplyFn _ (AppState s) c = do
  putStrLn (unCommandEntry (_cmdEntry c))
  return (AppState "next", CommandResult (BS.append s "world"))

appSpec :: MonadIO m => Spec m AppState
appSpec = mkSpec appApplyFn

