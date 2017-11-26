--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.DataPack.Get
-- Copyright : David M. Sledge 2017
-- License   : BSD3
--
-- Maintainer:  <lastname><firstinitial>@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- DataPack Deserializer using @Data.Binary@
--
--------------------------------------------------------------------

module Data.DataPack.Get where

import Prelude hiding (head, length, take)
import Control.Exception.Safe
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Binary.IEEE754
import Data.Bits
import Data.ByteString hiding (take, uncons)
import Data.Int
import Data.Text hiding (take, empty, append, length, head, uncons)
import Data.Text.Encoding
import Data.Typeable
import Data.Word

import Data.DataPack

data Format = Fixint Int
  | FNil
  | FColEnd
  | FBool Bool
  | FUInt8 | FUInt16 | FUInt32 | FUInt64
  | FInt8 | FInt16 | FInt32 | FInt64
  | FFloat32 | FFloat64
  | FBin8 | FBin16 | FBin32
  | FStr8 | FStr16 | FStr32
  | FNs8 | FNs16 | FNs32
  | FClassname
  | FArray
  | FMap
  | FObj
  | FFixbin Int | FFixstr Int | FFixns Int
  | FUnused
  deriving (Show)

idFormat byte
  -- positive fixint
  | byte .&. fixintMask == 0x00 = Fixint $ fromIntegral byte
  -- negative fixint
  | byte .&. fixintMask == fixintMask =
    Fixint $ fromIntegral (fromIntegral byte :: Int8)
  | byte == nilByte     = FNil
  | byte == colEndByte  = FColEnd
  | byte == falseByte   = FBool False
  | byte == trueByte    = FBool True
  | byte == uint8Byte   = FUInt8
  | byte == uint16Byte  = FUInt16
  | byte == uint32Byte  = FUInt32
  | byte == uint64Byte  = FUInt64
  | byte == int8Byte    = FInt8
  | byte == int16Byte   = FInt16
  | byte == int32Byte   = FInt32
  | byte == int64Byte   = FInt64
  | byte == float32Byte = FFloat32
  | byte == float64Byte = FFloat64
  | byte == bin8Byte    = FBin8
  | byte == bin16Byte   = FBin16
  | byte == bin32Byte   = FBin32
  | byte == str8Byte    = FStr8
  | byte == str16Byte   = FStr16
  | byte == str32Byte   = FStr32
  | byte == ns8Byte     = FNs8
  | byte == ns16Byte    = FNs16
  | byte == ns32Byte    = FNs32
  | byte == cnameByte   = FClassname
  | byte == arrayByte   = FArray
  | byte == objByte     = FObj
  | byte == mapByte     = FMap
  | otherwise = let len = fromIntegral $ byte .&. lenMask in
        case byte .&. fixMask of
          mask
            | mask == fixbinMask -> FFixbin len
            | mask == fixstrMask -> FFixstr len
            | mask == fixnsMask -> FFixns len
          _ -> FUnused

--------------------------------------------------------------------------------
-- TODO: implement MonadThrow to replace ExceptT

class (Monad m) => ByteStream s m where
  take :: Int -> s -> m (ByteString, s)
  takeS :: Int -> StateT s m ByteString
  takeS x = StateT $ take x
  uncons :: s -> m (Word8, s)
  uncons s = do
    (byteString, s') <- take 1 s
    pure (head byteString, s')
  unconsS :: StateT s m Word8
  unconsS = StateT uncons

data FormatException = Unused
  deriving (Show, Typeable)

instance Exception FormatException where
  displayException Unused = "Unused byte format"

data DPException = FormatException FormatException
  | ByteStreamException SomeException
  | CallbackException SomeException
  deriving (Show, Typeable)

instance Exception DPException --where
--   displayException (FormatException e) = displayException e
--   displayException (ByteStreamException e) = displayException e
--   displayException (CallbackException e) = displayException e

readBytesSE n =
  if n > 0
  then do
    let n' = n - 1
    byte <- catch (lift unconsS) $ throwM . ByteStreamException
    (fromIntegral byte `shiftL` (n' * 8) .|.) <$> readBytesSE n'
  else pure (0::Word64)

data PackHandler s m = PackHandler
  { int :: Int -> StateT (PackHandler s m) (StateT s m) ()
  , uInt :: Word64 -> StateT (PackHandler s m) (StateT s m) ()
  , colEnd :: StateT (PackHandler s m) (StateT s m) ()
  , nil :: StateT (PackHandler s m) (StateT s m) ()
  , bool :: Bool -> StateT (PackHandler s m) (StateT s m) ()
  , float :: Float -> StateT (PackHandler s m) (StateT s m) ()
  , double :: Double -> StateT (PackHandler s m) (StateT s m) ()
  }

readDataE :: (MonadThrow m, MonadCatch m, ByteStream s m) =>
  StateT (PackHandler s m) (StateT s m) ()
readDataE = do
  byte <- catch (lift unconsS) $ throwM . ByteStreamException
  pHandler <- get
  case idFormat byte of
    Fixint x -> int pHandler x
    FNil -> nil pHandler
    FColEnd -> colEnd pHandler
    FBool b -> bool pHandler b
    FUInt8 -> readBytesSE 1 >>= int pHandler . fromIntegral
    FUInt16 -> readBytesSE 2 >>= int pHandler . fromIntegral
    FUInt32 -> readBytesSE 4 >>= int pHandler . fromIntegral
    FUInt64 -> readBytesSE 8 >>= uInt pHandler
    FInt8 -> do
      byte <- readBytesSE 1
      int pHandler $ fromIntegral (fromIntegral byte :: Int8)
    FInt16 -> do
      word <- readBytesSE 2
      int pHandler $ fromIntegral (fromIntegral word :: Int16)
    FInt32 -> do
      word <- readBytesSE 4
      int pHandler $ fromIntegral (fromIntegral word :: Int32)
    FInt64 -> do
      word <- readBytesSE 8
      int pHandler $ fromIntegral (fromIntegral word :: Int64)
    FFloat32 -> readBytesSE 4 >>= float pHandler . wordToFloat . fromIntegral
    FFloat64 -> readBytesSE 8 >>= double pHandler . wordToDouble
    --Bin8 | Bin16 | Bin32
    --Str8 | Str16 | Str32
    --Ns8 | Ns16 | Ns32
    --Classname
    --Array
    --Map
    --Obj
    --Fixbin Int | Fixstr Int | Fixns Int
    FUnused -> throwM $ FormatException Unused

readBytes n =
  if n > 0
  then do
    let n' = n - 1
    byte <- catch (lift unconsS) $ throwM . ByteStreamException
    (fromIntegral byte `shiftL` (n' * 8) .|.) <$> readBytes n'
  else pure (0::Word64)

data PackType = Int Int
  | UInt Word64
  | Nil
  | ColEnd
  | Bool Bool
  | Float Float
  | Double Double
  | Bin ByteString
  | Str Text
  | NS Text

data StateStack = Array
  | Map
  | Obj
  | ColInit
  | EntryValue
  | LocalName

--readString :: Int -> ByteString -> StateT [StateStack] (StateT s m) ByteString
readString n byteString =
  let len = length byteString in
  if len < n
  then do
    suffix <- lift $ takeS (n - len)
    readString n $ append byteString suffix
  else pure byteString

--valueEnd :: StateT [StateStack] (StateT s m) ()
--valueEnd =

readValue :: (MonadThrow m, MonadCatch m, ByteStream s m) =>
  StateT [StateStack] (StateT s m) PackType
readValue = do
  byte <- catch (lift unconsS) $ throwM . ByteStreamException
  pHandler <- get
  case idFormat byte of
    Fixint x -> pure $ Int x
    FNil -> pure Nil
    FColEnd -> pure ColEnd
    FBool b -> pure $ Bool b
    FUInt8 -> Int . fromIntegral <$> readBytes 1
    FUInt16 -> Int . fromIntegral <$> readBytes 2
    FUInt32 -> Int . fromIntegral <$> readBytes 4
    FUInt64 -> UInt <$> readBytes 8
    FInt8 -> do
      byte <- readBytes 1
      pure . Int $ fromIntegral (fromIntegral byte :: Int8)
    FInt16 -> do
      word <- readBytes 2
      pure . Int $ fromIntegral (fromIntegral word :: Int16)
    FInt32 -> do
      word <- readBytes 4
      pure . Int $ fromIntegral (fromIntegral word :: Int32)
    FInt64 -> do
      word <- readBytes 8
      pure . Int $ fromIntegral (fromIntegral word :: Int64)
    FFloat32 -> Float . wordToFloat . fromIntegral <$> readBytes 4
    FFloat64 -> Double . wordToDouble <$> readBytes 8
    FBin8 -> do
      len <- fromIntegral <$> readBytes 1
      Bin <$> readString len empty
    FBin16 -> do
      len <- fromIntegral <$> readBytes 2
      Bin <$> readString len empty
    FBin32 -> do
      len <- fromIntegral <$> readBytes 4
      Bin <$> readString len empty
    FStr8 -> do
      len <- fromIntegral <$> readBytes 1
      Str . decodeUtf8 <$> readString len empty
    FStr16 -> do
      len <- fromIntegral <$> readBytes 2
      Str . decodeUtf8 <$> readString len empty
    FStr32 -> do
      len <- fromIntegral <$> readBytes 4
      Str . decodeUtf8 <$> readString len empty
    FNs8 -> do
      len <- fromIntegral <$> readBytes 1
      NS . decodeUtf8 <$> readString len empty
    FNs16 -> do
      len <- fromIntegral <$> readBytes 2
      NS . decodeUtf8 <$> readString len empty
    FNs32 -> do
      len <- fromIntegral <$> readBytes 4
      NS . decodeUtf8 <$> readString len empty
    --Classname
    --Array
    --Map
    --Obj
    --Fixbin Int | Fixstr Int | Fixns Int
    FUnused -> throwM $ FormatException Unused
