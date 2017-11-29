{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Exception.Safe
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State
import Data.Word
import qualified Data.ByteString as C
import Test.HUnit

import Data.DataPack
import Data.DataPack.Get

data StreamWithPosition = StreamWithPosition C.ByteString Int
  deriving (Show, Eq, Ord)

data EndOfStreamException = EndOfStreamException
  deriving (Show, Typeable, Eq, Ord)

instance Exception EndOfStreamException

instance (Monad m, MonadThrow m) => Stream StreamWithPosition m Int where
  take n (StreamWithPosition bstr pos) =
    if n > C.length bstr
    then throwM EndOfStreamException
    else
      let (body, tail) = C.splitAt n bstr in
      pure (body, StreamWithPosition tail $ pos + n)
  getPos (StreamWithPosition bstr pos) = pure pos

testWrapper label exec assertion = TestLabel label (TestCase (
  case exec of
    Right a -> assertion a
    Left e -> putStr "\n\t" *> print e
  ))

testReadNil = testWrapper "testReadNil" (
    runIdentityT . runStateT (runStateT readValue [])
    $ StreamWithPosition (C.pack [nilByte]) 0
    :: Either SomeException ((PackType, [StateStack]), StreamWithPosition))
  $ assertEqual "testReadNil" ((Nil,[]),StreamWithPosition C.empty 1)

-- testValidateNonText_ ss = print
--   (runIdentityT . runStateT (runStateT validateNonText ss)
--     $ StreamWithPosition C.empty 0
--     :: Either SomeException (((), [StateStack]), StreamWithPosition))

testValidateNonText_ ss label ss' = testWrapper label (
  runIdentityT . runStateT (runStateT validateNonText ss)
  $ StreamWithPosition C.empty 0
  :: Either SomeException (((), [StateStack]), StreamWithPosition))
  $ assertEqual "testValidateNonText" (((),ss'),StreamWithPosition C.empty 0)


testValidateNonText =
  [ testValidateNonText_ [] "non text with empty stack" []
  , testValidateNonText_ [Arr] "non text with Arr state" [Arr]
  , testValidateNonText_ [Mp] "non text with Mp state" [Mp]
  , testValidateNonText_ [Obj] "non text with Obj state" [Obj]
  , testValidateNonText_ [ColInit] "non text with ColInit state" [ColInit]
  , testValidateNonText_ [ClsNm] "non text with ClsNm state" [ClsNm]
  , testValidateNonText_ [EntryValue] "non text with EntryValue state" [EntryValue]
  , testValidateNonText_ [LocalName] "non text with LocalName state" [LocalName]
  , testValidateNonText_ [ColInit, Obj] "non text with [ColInit, Obj] stack" [ColInit, Obj]
  , testValidateNonText_ [ColInit, Arr] "non text with [ColInit, Arr] stack" [ColInit]
  , testValidateNonText_ [ColInit, Mp] "non text with [ColInit, Mp] stack" [ColInit, Mp]
  , testValidateNonText_ [ColInit, ColInit] "non text with [ColInit, ColInit] stack" [ColInit, ColInit]
  , testValidateNonText_ [ColInit, ClsNm] "non text with [ColInit, ClsNm] stack" [ColInit, ClsNm]
  , testValidateNonText_ [ColInit, EntryValue] "non text with [ColInit, EntryValue] stack" [ColInit, EntryValue]
  , testValidateNonText_ [ColInit, LocalName] "non text with [ColInit, LocalName] stack" [ColInit, LocalName]
  ]

main = runTestTT $ TestList (testReadNil:testValidateNonText)
