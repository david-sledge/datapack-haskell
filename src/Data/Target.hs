{-# LANGUAGE
  DeriveFunctor,
  DeriveGeneric,
  DerivingStrategies,
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  ImportQualifiedPost,
  MultiParamTypeClasses,
  StandaloneKindSignatures,
  UndecidableInstances
#-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Data.Target (
  DataTarget(..),
  ) where

import Prelude
import Data.ListLike (ListLike, ListLikeIO)
import Data.ListLike qualified as LL
import Data.ListLike.IO (hPutStr)
import GHC.Exts (Item)
import System.IO (Handle)

class DataTarget t d m where
  -- | Put data in stream target
  giveData
    :: d  -- ^ Data to put into the stream target
    -> t  -- ^ Stream target
    -> m t  -- ^ Modified stream target

instance (ListLike ll (Item ll), Applicative m) => DataTarget ll ll m where
  giveData ll t = pure $ LL.append t ll

instance ListLikeIO ll (Item ll) => DataTarget Handle ll IO where
  giveData ll h = do
    hPutStr h ll
    pure h
