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

module Data.Target (
  DataTarget(..),
  LL(..),
  TargetPos(..),
  ) where

import Prelude
import Data.ListLike (ListLike(genericLength), ListLikeIO)
import Data.ListLike qualified as LL
import Data.ListLike.IO (hPutStr)
import GHC.Exts (Item)
import GHC.Generics (Generic, Generic1)
import Data.Kind (Type, Constraint)
import System.IO (Handle)

type DataTarget :: Type -> Type -> (Type -> Type) -> Constraint
class DataTarget t d m where
  -- | Put data in stream target
  giveData
    :: d  -- ^ Data to put into the stream target
    -> t  -- ^ Stream target
    -> m t  -- ^ Modified stream target

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

instance (ListLike ll (Item ll), Applicative m) => DataTarget (LL ll) ll m where
  giveData ll (LL t) = pure . LL $ LL.append t ll

type TargetPos :: Type -> Type -> Type
data TargetPos t a = TargetPos
  { dataTarget :: t
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

instance (ListLike ll (Item ll), Applicative m, Num a) => DataTarget (TargetPos ll a) ll m where
  giveData ll (TargetPos t posi) = pure $ TargetPos (LL.append t ll) $ posi + genericLength t

instance ListLikeIO ll (Item ll) => DataTarget Handle ll IO where
  giveData ll h = do
    hPutStr h ll
    pure h
