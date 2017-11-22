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

import  Data.Word
import  Data.Bits
import  Data.Int

import  Control.Monad.Trans.Except
import  Control.Monad.Trans.State
import  Control.Monad.Identity

fixintMask  = 0xc0::Word8
nilByte     = 0x40::Word8
colEnd      = 0x41::Word8
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
  | byte == colEnd      = ColEnd
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

to16Bit (b1, b2) = ((fromIntegral b1::Word16) `shiftL` 8) .|. fromIntegral b2

to32Bit (b1, b2, b3, b4) = ((fromIntegral (to16Bit (b1, b2))::Word32) `shiftL` 16)
  .|. fromIntegral (to16Bit (b3, b4))

to64Bit (b1, b2, b3, b4, b5, b6, b7, b8) =
  ((fromIntegral (to32Bit (b1, b2, b3, b4))::Word64) `shiftL` 32)
  .|. fromIntegral (to32Bit (b5, b6, b7, b8))

--------------------------------------------------------------------------------

class (Monad m) => ByteStream s m where
    uncons :: s -> m (Word8, s)

data PackType = Int Int
  | Nil
  | Bool Bool

data FormatException s = Unused s

data Exception s e = FormatException (FormatException s)
  | ByteStreamException e
--
-- --hmmm :: ByteStream s m => s -> m (ExceptT FormatError m2 PackType)
-- --Just (Right $ Int x, s')
-- hmmm s = do
--   (byte, s') <- uncons s
--   pure (case idFormat byte of
--     Fixint x -> ExceptT . pure . Right $ Int x
--     FNil -> ExceptT . pure $ Right Nil
--     --ColEnd -> TODO
--     FBool b -> ExceptT . pure . Right $ Bool b
--     --UInt8 | UInt16 | UInt32 | UInt64
--     --Int8 | Int16 | Int32 | Int64
--     --Float32 | Float64
--     --Bin8 | Bin16 | Bin32
--     --Str8 | Str16 | Str32
--     --Ns8 | Ns16 | Ns32
--     --Classname
--     --Array
--     --Map
--     --Obj
--     --Fixbin Int | Fixstr Int | Fixns Int
--     FUnused -> throwError Unused, s')
--
-- hmmm2 s =
--   case uncons s of
--     Nothing -> Nothing
--     Just (byte, s') ->
--       case idFormat byte of
--         Fixint x -> Just (Right $ Int x, s')
--         FUnused -> Just (Left Unused, s')

hmmm3 s =
  case uncons s of
    Just (byte, s') ->
      case idFormat byte of
        Fixint x -> pure (Int x, s')
        FUnused -> throwE . FormatException $ Unused s'
    e -> throwE $ ByteStreamException e

hmmm4 s = do
  (byte, s') <- uncons s
  case idFormat byte of
    Fixint x -> pure (Int x, s')
    FUnused -> throwE . FormatException $ Unused s'

hmmm5 = StateT hmmm4
--}
  {-
getInt :: Get Int
getInt =
  getWord8 >>= \case
    -- positive fixint
    c | c .&. fixintMask == 0x00 ->
        return $ fromIntegral c
    -- negative fixint
      | c .&. fixintMask == 0xC0 ->
        return $ fromIntegral (fromIntegral c :: Int8)
    uint8Byte -> fromIntegral <$> getWord8
    uint16Byte -> fromIntegral <$> getWord16be
    uint32Byte -> fromIntegral <$> getWord32be
    uint64Byte -> fromIntegral <$> getWord64be
    int8Byte -> fromIntegral <$> getInt8
    int16Byte -> fromIntegral <$> getInt16be
    int32Byte -> fromIntegral <$> getInt32be
    int64Byte -> fromIntegral <$> getInt64be
    _    -> empty

getFloat :: Get Float
getFloat = tag float32Byte >> getFloat32be

getDouble :: Get Double
getDouble = tag float64Byte >> getFloat64be

getStr :: Get T.Text
getStr = do
  len <- getWord8 >>= \case
    t | t .&. 0xE0 == 0xA0 ->
      return $ fromIntegral $ t .&. 0x1F
    0xD9 -> fromIntegral <$> getWord8
    0xDA -> fromIntegral <$> getWord16be
    0xDB -> fromIntegral <$> getWord32be
    _    -> empty
  bs <- getByteString len
  case T.decodeUtf8' bs of
    Left _ -> empty
    Right v -> return v

getBin :: Get S.ByteString
getBin = do
  len <- getWord8 >>= \case
    0xC4 -> fromIntegral <$> getWord8
    0xC5 -> fromIntegral <$> getWord16be
    0xC6 -> fromIntegral <$> getWord32be
    _    -> empty
  getByteString len

getArray :: Get a -> Get (V.Vector a)
getArray g = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x90 ->
      return $ fromIntegral $ t .&. 0x0F
    0xDC -> fromIntegral <$> getWord16be
    0xDD -> fromIntegral <$> getWord32be
    _    -> empty
  V.replicateM len g

getMap :: Get a -> Get b -> Get (V.Vector (a, b))
getMap k v = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x80 ->
      return $ fromIntegral $ t .&. 0x0F
    0xDE -> fromIntegral <$> getWord16be
    0xDF -> fromIntegral <$> getWord32be
    _    -> empty
  V.replicateM len $ (,) <$> k <*> v

getExt :: Get (Word8, S.ByteString)
getExt = do
  len <- getWord8 >>= \case
    0xD4 -> return 1
    0xD5 -> return 2
    0xD6 -> return 4
    0xD7 -> return 8
    0xD8 -> return 16
    0xC7 -> fromIntegral <$> getWord8
    0xC8 -> fromIntegral <$> getWord16be
    0xC9 -> fromIntegral <$> getWord32be
    _ -> empty
  (,) <$> getWord8 <*> getByteString len

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be

tag :: Word8 -> Get ()
tag t = do
  b <- getWord8
  guard $ t == b
-}
