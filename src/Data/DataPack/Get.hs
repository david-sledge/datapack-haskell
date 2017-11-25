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

import Data.Word
import Data.Bits
import Data.Int

import Control.Exception.Safe
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Binary.IEEE754
import Data.Typeable

fixintMask  = 0xc0::Word8
nilByte     = 0x40::Word8
colEndByte  = 0x41::Word8
falseByte   = 0x42::Word8
trueByte    = 0x43::Word8
uint8Byte   = 0x44::Word8
uint16Byte  = 0x45::Word8
uint32Byte  = 0x46::Word8
uint64Byte  = 0x47::Word8
int8Byte    = 0x48::Word8
int16Byte   = 0x49::Word8
int32Byte   = 0x4a::Word8
int64Byte   = 0x4b::Word8
float32Byte = 0x4c::Word8
float64Byte = 0x4d::Word8
bin8Byte    = 0x4e::Word8
bin16Byte   = 0x4f::Word8
bin32Byte   = 0x50::Word8
str8Byte    = 0x51::Word8
str16Byte   = 0x52::Word8
str32Byte   = 0x53::Word8
ns8Byte     = 0x54::Word8
ns16Byte    = 0x55::Word8
ns32Byte    = 0x56::Word8
cnameByte   = 0x57::Word8
arrayByte   = 0x58::Word8
objByte     = 0x59::Word8
mapByte     = 0x5a::Word8
--unusedBytes = [0x5b..0x5f]
fixbinMask  = 0x60::Word8
fixstrMask  = 0x80::Word8
fixnsMask   = 0xa0::Word8
fixMask     = 0xe0::Word8
lenMask     = 0x1f::Word8

data Format = Fixint Int
  | FNil
  | ColEnd
  | FBool Bool
  | UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Float32 | Float64
  | Bin8 | Bin16 | Bin32
  | Str8 | Str16 | Str32
  | Ns8 | Ns16 | Ns32
  | Classname
  | Array
  | Map
  | Obj
  | Fixbin Int | Fixstr Int | Fixns Int
  | FUnused
  deriving (Show)

idFormat byte
  -- positive fixint
  | byte .&. fixintMask == 0x00 = Fixint $ fromIntegral byte
  -- negative fixint
  | byte .&. fixintMask == fixintMask =
    Fixint $ fromIntegral (fromIntegral byte :: Int8)
  | byte == nilByte     = FNil
  | byte == colEndByte  = ColEnd
  | byte == falseByte   = FBool False
  | byte == trueByte    = FBool True
  | byte == uint8Byte   = UInt8
  | byte == uint16Byte  = UInt16
  | byte == uint32Byte  = UInt32
  | byte == uint64Byte  = UInt64
  | byte == int8Byte    = Int8
  | byte == int16Byte   = Int16
  | byte == int32Byte   = Int32
  | byte == int64Byte   = Int64
  | byte == float32Byte = Float32
  | byte == float64Byte = Float64
  | byte == bin8Byte    = Bin8
  | byte == bin16Byte   = Bin16
  | byte == bin32Byte   = Bin32
  | byte == str8Byte    = Str8
  | byte == str16Byte   = Str16
  | byte == str32Byte   = Str32
  | byte == ns8Byte     = Ns8
  | byte == ns16Byte    = Ns16
  | byte == ns32Byte    = Ns32
  | byte == cnameByte   = Classname
  | byte == arrayByte   = Array
  | byte == objByte     = Obj
  | byte == mapByte     = Map
  | otherwise = let len = fromIntegral $ byte .&. lenMask in
        case byte .&. fixMask of
          mask
            | mask == fixbinMask -> Fixbin len
            | mask == fixstrMask -> Fixstr len
            | mask == fixnsMask -> Fixns len
          _ -> FUnused

--------------------------------------------------------------------------------
-- TODO: implement MonadThrow to replace ExceptT

class (Monad m) => ByteStream s m where
    uncons :: s -> m (Word8, s)
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

-- data PackType = Int Int
--   | UInt Word64
--   | Nil
--   | Bool Bool
--   | Float Float
--   | Double Double

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
    ColEnd -> colEnd pHandler
    FBool b -> bool pHandler b
    UInt8 -> readBytesSE 1 >>= int pHandler . fromIntegral
    UInt16 -> readBytesSE 2 >>= int pHandler . fromIntegral
    UInt32 -> readBytesSE 4 >>= int pHandler . fromIntegral
    UInt64 -> readBytesSE 8 >>= uInt pHandler
    Int8 -> do
      byte <- readBytesSE 1
      int pHandler $ fromIntegral (fromIntegral byte :: Int8)
    Int16 -> do
      word <- readBytesSE 2
      int pHandler $ fromIntegral (fromIntegral word :: Int16)
    Int32 -> do
      word <- readBytesSE 4
      int pHandler $ fromIntegral (fromIntegral word :: Int32)
    Int64 -> do
      word <- readBytesSE 8
      int pHandler $ fromIntegral (fromIntegral word :: Int64)
    Float32 -> readBytesSE 4 >>= float pHandler . wordToFloat . fromIntegral
    Float64 -> readBytesSE 8 >>= double pHandler . wordToDouble
    --Bin8 | Bin16 | Bin32
    --Str8 | Str16 | Str32
    --Ns8 | Ns16 | Ns32
    --Classname
    --Array
    --Map
    --Obj
    --Fixbin Int | Fixstr Int | Fixns Int
    FUnused -> throwM $ FormatException Unused
