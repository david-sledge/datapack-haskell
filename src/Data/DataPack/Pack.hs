{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.DataPack.Pack
-- Copyright : David M. Sledge 2017
-- License   : BSD3
--
-- Maintainer:  <lastname><firstinitial>@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- DataPack Serializer using @Data.Binary@
--
--------------------------------------------------------------------

module Data.DataPack.Pack (
  LocalName,
  NamespaceName,
  PackError(..),
  Value,

  pack,
  pkNil,
  pkLnBin,
  pkLnTxt,
  pkLnStr,
  pkNsTxt,
  pkNsStr,
  pkBin,
  pkTxt,
  pkStr,
  pkInt,
  pkFloat,
  pkDouble,
  pkBool,
  pkFalse,
  pkTrue,
  pkDict,
  pkSeq,
  pkUSeq,
  pkObj,
  pkUObj,
  (#.),
  (##),
  (#~),
  (~.),
  (~#),
  (~~),
  (|!),
  (.!),
  (#!),
  (~!),
  (|/),
  (./),
  (#/),
  (~/),
  (.:),
  (#:),
  (~:),
) where

import Prelude
import Data.Binary.IEEE754 ( doubleToWord, floatToWord )
import Data.Bits ( Bits((.|.), shiftR) )
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Target ( DataTarget(..) )
import Data.Text.Lazy ( Text )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.DataPack
    ( bin16Byte,
      bin32Byte,
      bin8Byte,
      classNameByte,
      collectionEndByte,
      dictionaryByte,
      doubleByte,
      falseByte,
      fixbinMask,
      fixnsMask,
      fixstrMask,
      floatByte,
      int16Byte,
      int32Byte,
      int64Byte,
      int8Byte,
      nilByte,
      ns16Byte,
      ns32Byte,
      ns8Byte,
      objectByte,
      sequenceByte,
      str16Byte,
      str32Byte,
      str8Byte,
      trueByte,
      uint16Byte,
      uint32Byte,
      uint64Byte,
      uint8Byte )
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad (foldM, (>=>))

infix 4 ##, #., #~, ~#, ~., ~~, .:, #:, ~:
infix 3 |!, .!, #!, ~!, |/, ./, #/, ~/

data PackError e =
  TooBig ByteString |
  TargetError e
  deriving stock (Show, Ord, Eq, Read)

_givePack :: (
    MonadTrans t, Monad m, DataTarget b d (ExceptT e m),
    MonadError (PackError e, b) (t m)) =>
  d -> b -> t m b
_givePack dat t = do
  res <- lift . runExceptT $ giveData dat t
  case res of
    Left e -> throwError (TargetError e, t)
    Right t' -> pure t'

_fromWord :: (Bits a, Integral a, Ord t, Num t) =>
  a -> t -> ByteString -> ByteString
_fromWord word n byteString =
  let n' = n - 1 in
  if n > 0
  then _fromWord (shiftR word 8) n' $
    B.cons (fromIntegral word) byteString
  else byteString

_packPrefixedBytes :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Bits a1, Integral a1, Num a2, Ord a2,
    Monad m) =>
  Word8 -> a1 -> a2 -> ByteString -> b -> t m b
_packPrefixedBytes byte = (((_givePack . B.cons byte) .) .) . _fromWord

_packDataString :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  ByteString -> Word8 -> Word8 -> Word8 -> Word8 -> b -> t m b
_packDataString byteString fixbyte byte8 byte16 byte32 =
  case B.length byteString of
  len | len <= 0x1f ->
        _givePack $ B.cons (fixbyte .|. (fromIntegral len::Word8)) byteString
      | len <= 0xff ->
        _packPrefixedBytes byte8 (fromIntegral len::Word8) (1 :: Int8) byteString
      | len <= 0xffff ->
        _packPrefixedBytes byte16 (fromIntegral len::Word16) (2 :: Int8) byteString
      | len <= 0xffffffff ->
        _packPrefixedBytes byte32 (fromIntegral len::Word32) (4 :: Int8) byteString
  _ -> throwError . (TooBig byteString, )

_pkBin :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  ByteString -> b -> t m b
_pkBin bin = _packDataString bin fixbinMask bin8Byte bin16Byte bin32Byte

_packUtf8 :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  ByteString -> b -> t m b
_packUtf8 bin = _packDataString bin fixstrMask str8Byte str16Byte str32Byte

_pkTxt :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  Text -> b -> t m b
_pkTxt = _packUtf8 . encodeUtf8

_pkStr :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  String -> b -> t m b
_pkStr = _packUtf8 . fromString

newtype LocalName e m a = LocalName (a -> ExceptT (PackError e, a) m a)

pkLnBin :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  ByteString -> LocalName e m b
pkLnBin = LocalName . _pkBin

pkLnTxt :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  Text -> LocalName e m b
pkLnTxt = LocalName . _pkTxt

pkLnStr :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  String -> LocalName e m b
pkLnStr = LocalName . _pkStr

_packNs :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  ByteString -> b -> t m b
_packNs bin = _packDataString bin fixnsMask ns8Byte ns16Byte ns32Byte

_packNsText :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  Text -> b -> t m b
_packNsText = _packNs . encodeUtf8

_packNsString :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  String -> b -> t m b
_packNsString = _packNs . fromString

newtype NamespaceName e m a = NamespaceName (a -> ExceptT (PackError e, a) m a)

pkNsTxt :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  Text -> NamespaceName e m b
pkNsTxt = NamespaceName . _packNsText

pkNsStr :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  String -> NamespaceName e m b
pkNsStr = NamespaceName . _packNsString

(##) :: (
    DataTarget b1 ByteString (ExceptT e1 m1),
    DataTarget b2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  Text -> Text -> Maybe (Maybe (NamespaceName e1 m1 b1), LocalName e2 m2 b2)
textNS ## text = Just (Just $ pkNsTxt textNS, pkLnTxt text)

(#.) :: (
    DataTarget b1 ByteString (ExceptT e1 m1),
    DataTarget b2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  Text -> ByteString ->
  Maybe (Maybe (NamespaceName e1 m1 b1), LocalName e2 m2 b2)
textNS #. bin = Just (Just $ pkNsTxt textNS, pkLnBin bin)

(#~) :: (
    DataTarget b1 ByteString (ExceptT e1 m1),
    DataTarget b2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  Text -> String -> Maybe (Maybe (NamespaceName e1 m1 b1), LocalName e2 m2 b2)
textNS #~ str = Just (Just $ pkNsTxt textNS, pkLnStr str)

(~.) :: (
    DataTarget b1 ByteString (ExceptT e1 m1),
    DataTarget b2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  String -> ByteString ->
  Maybe (Maybe (NamespaceName e1 m1 b1), LocalName e2 m2 b2)
strNS ~. bin = Just (Just $ pkNsStr strNS, pkLnBin bin)

(~#) :: (
    DataTarget b1 ByteString (ExceptT e1 m1),
    DataTarget b2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  String -> Text -> Maybe (Maybe (NamespaceName e1 m1 b1), LocalName e2 m2 b2)
strNS ~# text = Just (Just $ pkNsStr strNS, pkLnTxt text)

(~~) :: (
    DataTarget b1 ByteString (ExceptT e1 m1),
    DataTarget b2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  String -> String -> Maybe (Maybe (NamespaceName e1 m1 b1), LocalName e2 m2 b2)
strNS ~~ str = Just (Just $ pkNsStr strNS, pkLnStr str)

_packNumberBytes :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Bits a1, Integral a1, Num a2, Ord a2,
    Monad m) =>
  Word8 -> a1 -> a2 -> b -> t m b
_packNumberBytes byte word numBytes = _packPrefixedBytes byte word numBytes B.empty

newtype Value e m a = Value (a -> ExceptT (PackError e, a) m a)

pkNil :: Maybe a
pkNil = Nothing

pkBin :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  ByteString -> Maybe (Value e m b)
pkBin = Just . Value . _pkBin

pkTxt :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  Text -> Maybe (Value e m b)
pkTxt = Just . Value . _pkTxt

pkStr :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  String -> Maybe (Value e m b)
pkStr = Just . Value . _pkStr

pkInt :: (Integral a1, Monad m, DataTarget a2 ByteString (ExceptT e m)) =>
  a1 -> Maybe (Value e m a2)
pkInt word = Just . Value $
  case word of
  _ | 0 <= word && (0x7fffffffffffffff::Word64) < fromIntegral word ->
    _packNumberBytes uint64Byte (fromIntegral word::Word64) (8 :: Int8)
  _ -> case fromIntegral word::Int64 of
    int64 | int64 < -0x7fffffff - 1 || 0xffffffff < int64 ->
      _packNumberBytes int64Byte int64 (8 :: Int8)
          | 0x7fffffff < int64 ->
      _packNumberBytes uint32Byte (fromIntegral word::Word32) (4 :: Int8)
          | int64 < -0x7fff - 1 || 0xffff < int64 ->
      _packNumberBytes int32Byte (fromIntegral word::Int32) (4 :: Int8)
          | 0x7fff < int64 ->
      _packNumberBytes uint16Byte (fromIntegral word::Word16) (2 :: Int8)
          | int64 < -0x7f - 1 || 0xff < int64 ->
      _packNumberBytes int16Byte (fromIntegral word::Int16) (2 :: Int8)
          | 0x7f < int64 ->
      _packNumberBytes uint8Byte (fromIntegral word::Word8) (1 :: Int8)
          | int64 < -0x3f - 1 || 0x3f < int64 ->
      _packNumberBytes int8Byte (fromIntegral word::Int8) (1 :: Int8)
    _ -> _givePack $ _fromWord (fromIntegral word::Int8) (1 :: Int8) B.empty

pkFloat :: (DataTarget a ByteString (ExceptT e m), Monad m) =>
  Float -> Maybe (Value e m a)
pkFloat float = Just . Value $ _packNumberBytes floatByte
  (floatToWord float) (4 :: Int8)

pkDouble :: (DataTarget a ByteString (ExceptT e m), Monad m) =>
  Double -> Maybe (Value e m a)
pkDouble double = Just . Value $ _packNumberBytes doubleByte
  (doubleToWord double) (8 :: Int8)

_packSingleByte :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) =>
  Word8 -> b -> t m b
_packSingleByte byte = _givePack . B.cons byte $ B.empty

pkBool :: (DataTarget a ByteString (ExceptT e m), Monad m) =>
  Bool -> Maybe (Value e m a)
pkBool bool = Just . Value . _packSingleByte $ if bool then trueByte else falseByte

pkFalse, pkTrue :: (DataTarget a ByteString (ExceptT e m), Monad m) =>
  Maybe (Value e m a)
pkFalse = pkBool False

pkTrue = pkBool True

_pkNilByte, _packCollectionEnd :: (
    MonadTrans t, DataTarget b ByteString (ExceptT e m),
    MonadError (PackError e, b) (t m), Monad m) => b -> t m b
_pkNilByte = _packSingleByte nilByte

_packCollectionEnd = _packSingleByte collectionEndByte

_packMaybeValue :: (DataTarget a ByteString (ExceptT e m), Monad m) =>
  Maybe (Value e m a) -> a -> ExceptT (PackError e, a) m a
_packMaybeValue mValue =
  case mValue of
    Just (Value m) -> m
    _ -> _pkNilByte

pkDict :: (Foldable t, Monad m, DataTarget a ByteString (ExceptT e m)) =>
  t (Maybe (Value e m a), Maybe (Value e m a)) -> Maybe (Value e m a)
pkDict entries = Just . Value $
  _packSingleByte dictionaryByte >=>
  flip (foldM (\ tAcc (key, value) -> _packMaybeValue key tAcc >>= _packMaybeValue value)) entries >=>
  _packCollectionEnd

_packQualifiedName :: Monad m =>
  (Maybe (NamespaceName e m c), LocalName e m c) -> c ->
  ExceptT (PackError e, c) m c
_packQualifiedName (mNamespaceName, LocalName localName) =
  (case mNamespaceName of
    Just (NamespaceName m) -> m
    _ -> pure) >=> localName

_packMaybeClassName :: (DataTarget b ByteString (ExceptT e m), Monad m) =>
  Maybe (Maybe (NamespaceName e m b), LocalName e m b) -> b ->
  ExceptT (PackError e, b) m b
_packMaybeClassName mClassName =
  case mClassName of
    Nothing -> pure
    Just mQualfiedName ->
      _packSingleByte classNameByte >=> _packQualifiedName mQualfiedName

pkSeq :: (Foldable t, Monad m, DataTarget b ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName e m b), LocalName e m b) ->
  t (Maybe (Value e m b)) -> Maybe (Value e m b)
pkSeq className values = Just . Value $
  _packSingleByte sequenceByte >=>
  _packMaybeClassName className >=>
  flip (foldM $ flip _packMaybeValue) values >=>
  _packCollectionEnd

(|!) :: (Foldable t, Monad m, DataTarget a ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName e m a), LocalName e m a) ->
  t (Maybe (Value e m a)) -> Maybe (Value e m a)
(|!) = pkSeq

pkUSeq :: (Foldable t, Monad m, DataTarget a ByteString (ExceptT e m)) =>
  t (Maybe (Value e m a)) -> Maybe (Value e m a)
pkUSeq = pkSeq Nothing

(.!) :: (Foldable t, DataTarget a ByteString (ExceptT e m), Monad m) =>
  ByteString -> t (Maybe (Value e m a)) -> Maybe (Value e m a)
bin .! values = Just (Nothing, pkLnBin bin) |! values

(#!) :: (Foldable t, DataTarget a ByteString (ExceptT e m), Monad m) =>
  Text -> t (Maybe (Value e m a)) -> Maybe (Value e m a)
text #! values = Just (Nothing, pkLnTxt text) |! values

(~!) :: (Foldable t, DataTarget a ByteString (ExceptT e m), Monad m) =>
  String -> t (Maybe (Value e m a)) -> Maybe (Value e m a)
str ~! values = Just (Nothing, pkLnStr str) |! values

pkObj :: (Monad m, Foldable t, DataTarget a ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName e m a), LocalName e m a) ->
  t (Maybe (Maybe (NamespaceName e m a), LocalName e m a), Maybe (Value e m a)) ->
  Maybe (Value e m a)
pkObj className properties = Just . Value $
  _packSingleByte objectByte >=>
  _packMaybeClassName className >=>
  flip (foldM (\ tAcc (mQualifiedName, value) ->
    (case mQualifiedName of
      Nothing -> _packSingleByte nilByte
      Just mQualfiedName -> _packQualifiedName mQualfiedName) tAcc >>= _packMaybeValue value
    )) properties >=>
  _packCollectionEnd

(|/) :: (Monad m, Foldable t, DataTarget a ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName e m a), LocalName e m a) ->
  t (Maybe (Maybe (NamespaceName e m a), LocalName e m a), Maybe (Value e m a)) ->
  Maybe (Value e m a)
(|/) = pkObj

pkUObj :: (Monad m, Foldable t, DataTarget a ByteString (ExceptT e m)) =>
  t (Maybe (Maybe (NamespaceName e m a), LocalName e m a), Maybe (Value e m a)) ->
  Maybe (Value e m a)
pkUObj = pkObj Nothing

(./) :: (Foldable t, DataTarget a ByteString (ExceptT e m), Monad m) =>
  ByteString ->
  t (Maybe (Maybe (NamespaceName e m a), LocalName e m a), Maybe (Value e m a)) ->
  Maybe (Value e m a)
bin ./ values = Just (Nothing, pkLnBin bin) |/ values

(#/) :: (Foldable t, DataTarget a ByteString (ExceptT e m), Monad m) =>
  Text ->
  t (Maybe (Maybe (NamespaceName e m a), LocalName e m a), Maybe (Value e m a)) ->
  Maybe (Value e m a)
text #/ values = Just (Nothing, pkLnTxt text) |/ values

(~/) :: (Foldable t, DataTarget a ByteString (ExceptT e m), Monad m) =>
  String ->
  t (Maybe (Maybe (NamespaceName e m a), LocalName e m a), Maybe (Value e m a)) ->
  Maybe (Value e m a)
str ~/ values = Just (Nothing, pkLnStr str) |/ values

(.:) :: (DataTarget b1 ByteString (ExceptT e m), Monad m) =>
  ByteString -> b2 -> (Maybe (Maybe a, LocalName e m b1), b2)
bin .: value = (Just (Nothing, pkLnBin bin), value)

(#:) :: (DataTarget b1 ByteString (ExceptT e m), Monad m) =>
  Text -> b2 -> (Maybe (Maybe a, LocalName e m b1), b2)
text #: value = (Just (Nothing, pkLnTxt text), value)

(~:) :: (DataTarget b1 ByteString (ExceptT e m), Monad m) =>
  String -> b2 -> (Maybe (Maybe a, LocalName e m b1), b2)
str ~: value = (Just (Nothing, pkLnStr str), value)

pack :: (DataTarget a ByteString (ExceptT e m), Monad m) =>
  Maybe (Value e m a) -> a -> m (Either (PackError e, a) a)
pack = (runExceptT .) . _packMaybeValue
