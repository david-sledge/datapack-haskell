--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

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

import Prelude hiding (take)
import Control.Exception.Safe
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString as C
import Data.Functor
import Data.Int
import qualified Data.Text as Text
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
  deriving (Show, Eq, Ord)

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

class (Monad m, Show p) => Stream s m p | s -> p where
  take :: Int -> s -> m (C.ByteString, s)
  takeS :: Int -> StateT s m C.ByteString
  takeS x = StateT $ take x
  uncons :: s -> m (Word8, s)
  uncons s = do
    (byteString, s') <- take 1 s
    pure (C.head byteString, s')
  unconsS :: StateT s m Word8
  unconsS = StateT uncons
  getPos :: s -> m p

data FormatException =
    Unused
  | QualifiedNameException
  | InvalidStateException
  | LocalNameException
  | ClassnameNotAllowedException
  deriving (Show, Typeable, Eq, Ord)

instance Exception FormatException where
  displayException Unused = "Unused byte format"
  displayException QualifiedNameException = "namespace or local name expected"
  displayException InvalidStateException = "invalid state"
  displayException LocalNameException = "local name expected"
  displayException ClassnameNotAllowedException = "classname not allowed here"

data DPException = FormatException FormatException
  | ByteStreamException SomeException
  | CallbackException SomeException
  deriving (Show, Typeable)

instance Exception DPException --where
--   displayException (FormatException e) = displayException e
--   displayException (ByteStreamException e) = displayException e
--   displayException (CallbackException e) = displayException e

-- read n bytes and OR them into a single n-byte word
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
  | Bin C.ByteString
  | Str Text.Text
  | NS Text.Text
  | Array
  | Object
  | Map
  | Classname
  deriving (Show, Eq, Ord)

data StateStack = Arr
  | Mp
  | Obj
  | ColInit
  | ClsNm
  | EntryValue
  | LocalName
  deriving (Show, Eq, Ord)

--readString :: Int -> C.ByteString -> StateT [StateStack] (StateT s m) C.ByteString
readString n byteString =
  let len = C.length byteString in
  if len < n
  then do
    suffix <- lift $ takeS (n - len)
    readString n $ C.append byteString suffix
  else pure byteString

readStream len = readString len C.empty

--valueEnd :: StateT [StateStack] (StateT s m) ()
--valueEnd =

validateNonText :: (MonadThrow m, MonadCatch m, Stream s m p) =>
 StateT [StateStack] (StateT s m) ()
validateNonText = do
  ss <- get
  case ss of
    [] -> pure ()
    state:ss' ->
      case state of
        Arr -> pure ()
        Mp -> pure ()
        Obj -> throwM $ FormatException QualifiedNameException
        ColInit ->
          case ss' of
            [] -> throwM $ FormatException InvalidStateException
            state':ss'' ->
              case state' of
                Obj -> throwM $ FormatException QualifiedNameException
                Arr -> put $ state:ss''
                _ -> throwM $ FormatException InvalidStateException
        ClsNm -> throwM $ FormatException QualifiedNameException
        EntryValue -> pure ()
        LocalName -> throwM $ FormatException LocalNameException

readValue :: (MonadThrow m, MonadCatch m, Stream s m p) =>
  StateT [StateStack] (StateT s m) PackType
readValue = do
  byte <- catch (lift unconsS) $ throwM . ByteStreamException
  pHandler <- get
  case idFormat byte of
    Fixint x -> validateNonText $> Int x
    FNil -> validateNonText $> Nil
    FColEnd -> validateNonText $> ColEnd
    FBool b -> validateNonText $> Bool b
    FUInt8 -> validateNonText *> (Int . fromIntegral <$> readBytes 1)
    FUInt16 -> validateNonText *> (Int . fromIntegral <$> readBytes 2)
    FUInt32 -> validateNonText *> (Int . fromIntegral <$> readBytes 4)
    FUInt64 -> validateNonText *> (UInt <$> readBytes 8)
    FInt8 -> do
      validateNonText
      byte <- readBytes 1
      pure . Int $ fromIntegral (fromIntegral byte :: Int8)
    FInt16 -> do
      validateNonText
      word <- readBytes 2
      pure . Int $ fromIntegral (fromIntegral word :: Int16)
    FInt32 -> do
      validateNonText
      word <- readBytes 4
      pure . Int $ fromIntegral (fromIntegral word :: Int32)
    FInt64 -> do
      validateNonText
      word <- readBytes 8
      pure . Int $ fromIntegral (fromIntegral word :: Int64)
    FFloat32 -> validateNonText *> (Float . wordToFloat . fromIntegral <$> readBytes 4)
    FFloat64 -> validateNonText *> (Double . wordToDouble <$> readBytes 8)
    FBin8 -> validateNonText *> (Bin <$> ((fromIntegral <$> readBytes 1) >>= readStream))
    FBin16 -> validateNonText *> (Bin <$> ((fromIntegral <$> readBytes 2) >>= readStream))
    FBin32 -> validateNonText *> (Bin <$> ((fromIntegral <$> readBytes 4) >>= readStream))
    FStr8 -> Str . decodeUtf8 <$> ((fromIntegral <$> readBytes 1) >>= readStream)
    FStr16 -> Str . decodeUtf8 <$> ((fromIntegral <$> readBytes 2) >>= readStream)
    FStr32 -> Str . decodeUtf8 <$> ((fromIntegral <$> readBytes 4) >>= readStream)
    FNs8 -> NS . decodeUtf8 <$> ((fromIntegral <$> readBytes 1) >>= readStream)
    FNs16 -> NS . decodeUtf8 <$> ((fromIntegral <$> readBytes 2) >>= readStream)
    FNs32 -> NS . decodeUtf8 <$> ((fromIntegral <$> readBytes 4) >>= readStream)
    --{-
    FClassname -> do
      ss <- get
      case ss of
        [] -> throwM $ FormatException InvalidStateException
        state:ss' ->
          case state of
            ColInit -> put (ClsNm:ss') $> Classname
            _ -> throwM $ FormatException ClassnameNotAllowedException
    --}
    FArray -> validateNonText *> modify ((:) Arr . (:) ColInit) $> Array
    FMap -> validateNonText *> modify ((:) Mp) $> Map
    FObj -> validateNonText *> modify ((:) Obj . (:) ColInit) $> Object
    FFixbin len -> validateNonText *> (Bin <$> readStream len)
    FFixstr len -> Str . decodeUtf8 <$> readStream len
    FFixns len -> NS . decodeUtf8 <$> readStream len
    FUnused -> throwM $ FormatException Unused
