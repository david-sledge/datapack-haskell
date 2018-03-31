{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Exception.Safe
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State
import Data.Function (on)
import Data.Word
import qualified Data.ByteString as C
import Test.HUnit

import Data.DataPack
import Data.DataPack.Get

instance Eq SomeException where
    (==) = (==) `on` displayException

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

testWrapper label exec assertion = TestLabel label . TestCase $ assertion exec

testReadNil = testWrapper "testReadNil" (
    runIdentityT . runStateT (runStateT readValue [])
    $ StreamWithPosition (C.pack [nilByte]) 0
    :: Either SomeException ((PackType, [StateStack]), StreamWithPosition))
  $ assertEqual "testReadNil" $ Right ((Nil,[]),StreamWithPosition C.empty 1)

testValidateNonText_ ss label a = testWrapper label (
  runIdentityT . runStateT (runStateT validateNonText ss)
  $ StreamWithPosition C.empty 0
  :: Either SomeException (((), [StateStack]), StreamWithPosition))
  $ assertEqual "testValidateNonText" a


testValidateNonText =
  [ testValidateNonText_ [] "non text with empty stack" $ Right (((),[]),StreamWithPosition C.empty 0)
  , testValidateNonText_ [Arr] "non text with Arr state" $ Right (((),[Arr]),StreamWithPosition C.empty 0)
  , testValidateNonText_ [Mp] "non text with Mp state" $ Right (((),[Mp]),StreamWithPosition C.empty 0)
  , testValidateNonText_ [Obj] "non text with Obj state" . Left . SomeException . FormatException . OptionException CollectionEndException $ OptionException NamespaceException LocalNameException
  , testValidateNonText_ [ColInit] "non text with ColInit state" . Left . SomeException $ FormatException InvalidStateException
  , testValidateNonText_ [ClsNm] "non text with ClsNm state" . Left . SomeException . FormatException $ OptionException NamespaceException LocalNameException
  , testValidateNonText_ [EntryValue] "non text with EntryValue state" $ Right (((),[EntryValue]),StreamWithPosition C.empty 0)
  , testValidateNonText_ [LocalName] "non text with LocalName state" . Left . SomeException $ FormatException LocalNameException
  , testValidateNonText_ [ColInit, Obj] "non text with [ColInit, Obj] stack" . Left . SomeException . FormatException . OptionException (OptionException ClassnameException CollectionEndException) $ OptionException NamespaceException LocalNameException
  , testValidateNonText_ [ColInit, Arr] "non text with [ColInit, Arr] stack" $ Right (((),[Arr]),StreamWithPosition C.empty 0)
  , testValidateNonText_ [ColInit, Mp] "non text with [ColInit, Mp] stack" . Left . SomeException $ FormatException InvalidStateException
  , testValidateNonText_ [ColInit, ColInit] "non text with [ColInit, ColInit] stack" . Left . SomeException $ FormatException InvalidStateException
  , testValidateNonText_ [ColInit, ClsNm] "non text with [ColInit, ClsNm] stack" . Left . SomeException $ FormatException InvalidStateException
  , testValidateNonText_ [ColInit, EntryValue] "non text with [ColInit, EntryValue] stack" . Left . SomeException $ FormatException InvalidStateException
  , testValidateNonText_ [ColInit, LocalName] "non text with [ColInit, LocalName] stack" . Left . SomeException $ FormatException InvalidStateException
  ]

main = runTestTT $ TestList (testReadNil:testValidateNonText)
