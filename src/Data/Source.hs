{-# LANGUAGE
  DeriveFunctor,
  DeriveGeneric,
  DerivingStrategies,
  FlexibleContexts,
  FlexibleInstances,
  FunctionalDependencies,
  GeneralizedNewtypeDeriving,
  ImportQualifiedPost,
  MultiParamTypeClasses,
  StandaloneKindSignatures,
  TupleSections,
  UndecidableInstances
#-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Data.Source (
  DataSource(..),
  DataSourceError(..),
  takeFromSource,
  ) where

import Prelude hiding (splitAt)
import Control.Monad.Except (MonadError, throwError, modifyError)
import Data.ListLike (ListLike(genericLength))
import GHC.Exts (Item)
import GHC.Generics (Generic, Generic1)
import Data.ByteString.Lazy (ByteString, splitAt, hGet)
import Data.Int (Int64)
import System.IO (Handle)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))

class Integral n => DataSource s d n m where
  takeData :: n -> d -> s -> m (d, s)

data DataSourceError e s b n =
  Source2 e |
  MoreData2 s b n
  deriving stock ( Eq
                 , Ord
                 , Read
                 , Show
                 , Generic
                 , Generic1
                 , Functor
                 )

takeFromSource :: (
    ListLike full (Item full), MonadError (DataSourceError e s full b) m,
    DataSource s full b (ExceptT e m)) =>
  b -> full -> s -> m (full, s)
takeFromSource n d s = do
  r@(d', s') <- modifyError Source2 (takeData n d s)
  if n > genericLength d' - genericLength d
    then throwError . MoreData2 s' d' $ n - genericLength d' + genericLength d
    else pure r

instance Applicative m => DataSource ByteString ByteString Int64 m where
  takeData n d s =
    let (d', s') = splitAt n s in
    pure (d <> d', s')

instance MonadIO m => DataSource Handle ByteString Int m where
  takeData n d h = do
    d' <- liftIO $ hGet h n
    pure (d <> d', h)
