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

module Data.Source (
  DataSource(..),
  DataSourceError(..),
  LL(..),
  SourcePos(..),
  takeFromSource,
  ) where

import Prelude
import Control.Monad.Except (MonadError, throwError)
import Data.ListLike (ListLike(genericLength, genericSplitAt))
import Data.ListLike qualified as LL
import GHC.Exts (Item)
import GHC.Generics (Generic, Generic1)
import Data.Kind (Type, Constraint)

type DataSource :: Type -> Type -> (Type -> Type) -> Constraint
class DataSource s d m | s -> d where
  -- | Get data from stream source
  getData
    :: s  -- ^ Stream source
    -> m d  -- ^ The data from the stream source

  -- | Put data in stream source
  putData
    :: d  -- ^ Data to put into the stream source
    -> s  -- ^ Stream source
    -> m s  -- ^ Modified stream source

  -- | Event handler for when data is extracted from the source
  onTake
    :: d  -- ^ Extracted data
    -> s  -- ^ Post extraction source
    -> m s  -- ^ Resulting source

type DataSourceError :: Type -> Type -> Type
data DataSourceError e a =
  MoreData a |
  Source e
  deriving stock ( Eq
                 , Ord
                 , Read
                 , Show
                 , Generic
                 , Generic1
                 , Functor
                 )

takeFromSource :: (ListLike d (Item d), MonadError (DataSourceError e a) m, Integral a, DataSource s d m)
  => a -> s -> m (d, s)
takeFromSource i s =
  if i == 0
  then pure (LL.empty, s)
  else do
    d <- getData s
    if i > genericLength d
      then throwError . MoreData $ i - genericLength d
      else case genericSplitAt i d of
        (front, back) -> (front, ) <$> (putData back s >>= onTake front)

type LL :: Type -> Type
newtype LL ll = LL ll
  deriving stock ( Eq
                 , Ord
                 , Read
                 , Show
                 , Bounded
                 , Functor
                 , Generic
                 , Generic1
                 ) deriving newtype (Num)

instance (ListLike ll (Item ll), Applicative m) => DataSource (LL ll) ll m where
  getData (LL ll) = pure ll

  putData ll _ = pure $ LL ll

  onTake _ = pure

type SourcePos :: Type -> Type -> Type
data SourcePos s a = SourcePos
  { dataSource :: s
  , pos :: a
  }
  deriving stock ( Eq
                 , Ord
                 , Read
                 , Show
                 , Generic
                 , Generic1
                 , Functor
                 )

instance (ListLike ll (Item ll), Applicative m, Num a) => DataSource (SourcePos ll a) ll m where
  getData (SourcePos ll _) = pure ll

  putData ll (SourcePos _ posi) = pure $ SourcePos ll posi

  onTake d (SourcePos ll posi) = pure . SourcePos ll $ posi + LL.genericLength d
