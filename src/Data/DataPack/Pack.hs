{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

  runPack,
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
import Control.Monad (foldM, (>=>))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Trans.Class (lift, MonadTrans)
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

infix 4 ##, #., #~, ~#, ~., ~~, .:, #:, ~:
infix 3 |!, .!, #!, ~!, |/, ./, #/, ~/

data PackError e =
  TooBig ByteString |
  TargetError e
  deriving stock (Show, Ord, Eq, Read)

_givePack :: (
    MonadTrans mt, Monad m, DataTarget t d (ExceptT e m),
    MonadError (PackError e, t) (mt m)) =>
  d -> t -> mt m t
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
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Bits a1, Integral a1, Num a2, Ord a2,
    Monad m) =>
  Word8 -> a1 -> a2 -> ByteString -> t -> mt m t
_packPrefixedBytes byte = (((_givePack . B.cons byte) .) .) . _fromWord

_packDataString :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  ByteString -> Word8 -> Word8 -> Word8 -> Word8 -> t -> mt m t
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
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  ByteString -> t -> mt m t
_pkBin bin = _packDataString bin fixbinMask bin8Byte bin16Byte bin32Byte

_packUtf8 :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  ByteString -> t -> mt m t
_packUtf8 bin = _packDataString bin fixstrMask str8Byte str16Byte str32Byte

_pkTxt :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  Text -> t -> mt m t
_pkTxt = _packUtf8 . encodeUtf8

_pkStr :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  String -> t -> mt m t
_pkStr = _packUtf8 . fromString

newtype LocalName m a = LocalName (a -> m a)
-- newtype LocalName e c m a = LocalName (a -> ExceptT (PackError e, a) m a)

pkLnBin :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  ByteString -> LocalName (ExceptT (PackError e, t) m) t
pkLnBin = LocalName . _pkBin

pkLnTxt :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Text -> LocalName (ExceptT (PackError e, t) m) t
pkLnTxt = LocalName . _pkTxt

pkLnStr :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  String -> LocalName (ExceptT (PackError e, t) m) t
pkLnStr = LocalName . _pkStr

_packNs :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  ByteString -> t -> mt m t
_packNs bin = _packDataString bin fixnsMask ns8Byte ns16Byte ns32Byte

_packNsText :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  Text -> t -> mt m t
_packNsText = _packNs . encodeUtf8

_packNsString :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  String -> t -> mt m t
_packNsString = _packNs . fromString

-- newtype NamespaceName e c m a = NamespaceName (a -> ExceptT (PackError e, a) m a)
newtype NamespaceName m a = NamespaceName (a -> m a)

pkNsTxt :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Text -> NamespaceName (ExceptT (PackError e, t) m) t
pkNsTxt = NamespaceName . _packNsText

pkNsStr :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  String -> NamespaceName (ExceptT (PackError e, t) m) t
pkNsStr = NamespaceName . _packNsString

(##) :: (
    DataTarget t1 ByteString (ExceptT e1 m1),
    DataTarget t2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  Text -> Text -> Maybe (Maybe (NamespaceName (ExceptT (PackError e1, t1) m1) t1), LocalName (ExceptT (PackError e2, t2) m2) t2)
textNS ## text = Just (Just $ pkNsTxt textNS, pkLnTxt text)

(#.) :: (
    DataTarget t1 ByteString (ExceptT e1 m1),
    DataTarget t2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  Text -> ByteString ->
  Maybe (Maybe (NamespaceName (ExceptT (PackError e1, t1) m1) t1), LocalName (ExceptT (PackError e2, t2) m2) t2)
textNS #. bin = Just (Just $ pkNsTxt textNS, pkLnBin bin)

(#~) :: (
    DataTarget t1 ByteString (ExceptT e1 m1),
    DataTarget t2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  Text -> String -> Maybe (Maybe (NamespaceName (ExceptT (PackError e1, t1) m1) t1), LocalName (ExceptT (PackError e2, t2) m2) t2)
textNS #~ str = Just (Just $ pkNsTxt textNS, pkLnStr str)

(~.) :: (
    DataTarget t1 ByteString (ExceptT e1 m1),
    DataTarget t2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  String -> ByteString ->
  Maybe (Maybe (NamespaceName (ExceptT (PackError e1, t1) m1) t1), LocalName (ExceptT (PackError e2, t2) m2) t2)
strNS ~. bin = Just (Just $ pkNsStr strNS, pkLnBin bin)

(~#) :: (
    DataTarget t1 ByteString (ExceptT e1 m1),
    DataTarget t2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  String -> Text -> Maybe (Maybe (NamespaceName (ExceptT (PackError e1, t1) m1) t1), LocalName (ExceptT (PackError e2, t2) m2) t2)
strNS ~# text = Just (Just $ pkNsStr strNS, pkLnTxt text)

(~~) :: (
    DataTarget t1 ByteString (ExceptT e1 m1),
    DataTarget t2 ByteString (ExceptT e2 m2), Monad m1, Monad m2) =>
  String -> String -> Maybe (Maybe (NamespaceName (ExceptT (PackError e1, t1) m1) t1), LocalName (ExceptT (PackError e2, t2) m2) t2)
strNS ~~ str = Just (Just $ pkNsStr strNS, pkLnStr str)

_packNumberBytes :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Bits a1, Integral a1, Num a2, Ord a2,
    Monad m) =>
  Word8 -> a1 -> a2 -> t -> mt m t
_packNumberBytes byte word numBytes = _packPrefixedBytes byte word numBytes B.empty

-- newtype Value e c m a = Value (a -> ExceptT (PackError e, a) m a)
newtype Value m a = Value (a -> m a)

pkNil :: (DataTarget t ByteString (ExceptT e m), Monad m) => Maybe (Value (ExceptT (PackError e, t) m) t)
pkNil = Just $ Value _pkNilByte

pkBin :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  ByteString -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkBin = Just . Value . _pkBin

pkTxt :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Text -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkTxt = Just . Value . _pkTxt

pkStr :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  String -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkStr = Just . Value . _pkStr

pkInt :: (Integral a, Monad m, DataTarget t ByteString (ExceptT e m)) =>
  a -> Maybe (Value (ExceptT (PackError e, t) m) t)
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

pkFloat :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Float -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkFloat float = Just . Value $ _packNumberBytes floatByte
  (floatToWord float) (4 :: Int8)

pkDouble :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Double -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkDouble double = Just . Value $ _packNumberBytes doubleByte
  (doubleToWord double) (8 :: Int8)

_packSingleByte :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) =>
  Word8 -> t -> mt m t
_packSingleByte byte = _givePack . B.cons byte $ B.empty

pkBool :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Bool -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkBool bool = Just . Value . _packSingleByte $ if bool then trueByte else falseByte

pkFalse, pkTrue :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Maybe (Value (ExceptT (PackError e, t) m) t)
pkFalse = pkBool False

pkTrue = pkBool True

_pkNilByte, _packCollectionEnd :: (
    MonadTrans mt, DataTarget t ByteString (ExceptT e m),
    MonadError (PackError e, t) (mt m), Monad m) => t -> mt m t
_pkNilByte = _packSingleByte nilByte

_packCollectionEnd = _packSingleByte collectionEndByte

pack :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Maybe (Value (ExceptT (PackError e, t) m) t) -> t -> ExceptT (PackError e, t) m t
pack mValue =
  case mValue of
    Just (Value m) -> m
    _ -> _pkNilByte

_packQualifiedName :: Monad m =>
  (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t) -> t ->
  ExceptT (PackError e, t) m t
_packQualifiedName (mNamespaceName, LocalName localName) =
  (case mNamespaceName of
    Just (NamespaceName m) -> m
    _ -> pure) >=> localName

_packaybeClassName :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t) -> t ->
  ExceptT (PackError e, t) m t
_packaybeClassName mClassName =
  case mClassName of
    Nothing -> pure
    Just mQualfiedName ->
      _packSingleByte classNameByte >=> _packQualifiedName mQualfiedName

pkSeq :: (Foldable fo, Monad m, DataTarget t ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t) ->
  fo (Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkSeq className values = Just . Value $
  _packSingleByte sequenceByte >=>
  _packaybeClassName className >=>
  flip (foldM $ flip pack) values >=>
  _packCollectionEnd

pkDict :: (Foldable fo, Monad m, DataTarget t ByteString (ExceptT e m)) =>
  fo (Maybe (Value (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkDict entries = Just . Value $ \ t -> do
  t' <- _packSingleByte dictionaryByte t
  foldM (\ (tAcc, lastWasNil) (key, value) -> do
    tAcc' <- (if lastWasNil
      then _pkNilByte
      else pure) tAcc >>= pack key
    case value of
      Just (Value m) -> (, False) <$> m tAcc'
      Nothing -> pure (tAcc', True)) (t', False) entries >>= _packCollectionEnd . fst

(|!) :: (Foldable fo, Monad m, DataTarget t ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t) ->
  fo (Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
(|!) = pkSeq

pkUSeq :: (Foldable fo, Monad m, DataTarget t ByteString (ExceptT e m)) =>
  fo (Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
pkUSeq = pkSeq Nothing

(.!) :: (Foldable fo, DataTarget t ByteString (ExceptT e m), Monad m) =>
  ByteString -> fo (Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
bin .! values = Just (Nothing, pkLnBin bin) |! values

(#!) :: (Foldable fo, DataTarget t ByteString (ExceptT e m), Monad m) =>
  Text -> fo (Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
text #! values = Just (Nothing, pkLnTxt text) |! values

(~!) :: (Foldable fo, DataTarget t ByteString (ExceptT e m), Monad m) =>
  String -> fo (Maybe (Value (ExceptT (PackError e, t) m) t)) -> Maybe (Value (ExceptT (PackError e, t) m) t)
str ~! values = Just (Nothing, pkLnStr str) |! values

pkObj :: (Monad m, Foldable fo, DataTarget t ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t) ->
  fo (Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) ->
  Maybe (Value (ExceptT (PackError e, t) m) t)
pkObj className properties = Just . Value $ \ t -> do
  t' <- _packSingleByte objectByte t >>= _packaybeClassName className
  foldM (\ (tAcc, lastWasNil) (mQualifiedName, value) -> do
    tAcc' <- (if lastWasNil
      then _pkNilByte
      else pure) tAcc >>= (case mQualifiedName of
      Nothing -> _packSingleByte nilByte
      Just mQualfiedName -> _packQualifiedName mQualfiedName)
    case value of
      Just (Value m) -> (, False) <$> m tAcc'
      Nothing -> pure (tAcc', True)
    ) (t', False) properties >>= _packCollectionEnd . fst

(|/) :: (Monad m, Foldable fo, DataTarget t ByteString (ExceptT e m)) =>
  Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t) ->
  fo (Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) ->
  Maybe (Value (ExceptT (PackError e, t) m) t)
(|/) = pkObj

pkUObj :: (Monad m, Foldable fo, DataTarget t ByteString (ExceptT e m)) =>
  fo (Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) ->
  Maybe (Value (ExceptT (PackError e, t) m) t)
pkUObj = pkObj Nothing

(./) :: (Foldable fo, DataTarget t ByteString (ExceptT e m), Monad m) =>
  ByteString ->
  fo (Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) ->
  Maybe (Value (ExceptT (PackError e, t) m) t)
bin ./ values = Just (Nothing, pkLnBin bin) |/ values

(#/) :: (Foldable fo, DataTarget t ByteString (ExceptT e m), Monad m) =>
  Text ->
  fo (Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) ->
  Maybe (Value (ExceptT (PackError e, t) m) t)
text #/ values = Just (Nothing, pkLnTxt text) |/ values

(~/) :: (Foldable fo, DataTarget t ByteString (ExceptT e m), Monad m) =>
  String ->
  fo (Maybe (Maybe (NamespaceName (ExceptT (PackError e, t) m) t), LocalName (ExceptT (PackError e, t) m) t), Maybe (Value (ExceptT (PackError e, t) m) t)) ->
  Maybe (Value (ExceptT (PackError e, t) m) t)
str ~/ values = Just (Nothing, pkLnStr str) |/ values

(.:) :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  ByteString -> b -> (Maybe (Maybe a, LocalName (ExceptT (PackError e, t) m) t), b)
bin .: value = (Just (Nothing, pkLnBin bin), value)

(#:) :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Text -> b -> (Maybe (Maybe a, LocalName (ExceptT (PackError e, t) m) t), b)
text #: value = (Just (Nothing, pkLnTxt text), value)

(~:) :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  String -> b -> (Maybe (Maybe a, LocalName (ExceptT (PackError e, t) m) t), b)
str ~: value = (Just (Nothing, pkLnStr str), value)

runPack :: (DataTarget t ByteString (ExceptT e m), Monad m) =>
  Maybe (Value (ExceptT (PackError e, t) m) t) -> t -> m (Either (PackError e, t) t)
runPack = (runExceptT .) . pack
