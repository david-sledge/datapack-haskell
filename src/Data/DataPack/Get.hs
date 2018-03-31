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

import Prelude hiding (take, map)
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
    UnusedException
  | ValueException
  | NamespaceException
  | ClassnameException
  | LocalNameException
  | CollectionEndException
  | InvalidStateException
  | OptionException FormatException FormatException
  deriving (Show, Typeable, Eq, Ord)

instance Exception FormatException where
  displayException UnusedException = "Unused byte format"
  displayException ValueException = "value expected"
  displayException NamespaceException = "namespace expected"
  displayException ClassnameException = "classname expected"
  displayException LocalNameException = "local name (string) expected"
  displayException CollectionEndException = "collection end expected"
  displayException InvalidStateException = "invalid state"
  displayException (OptionException e1 e2) = displayException e1 ++ " or " ++ displayException e2

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

readString n byteString =
  let len = C.length byteString in
  if len < n
  then do
    suffix <- lift $ takeS (n - len)
    readString n $ C.append byteString suffix
  else pure byteString

readStream len = readString len C.empty

data StateProcessor m = StateProcessor
  { empty :: m ()
  , arr :: m ()
  , map :: m ()
  , obj :: m ()
  , arrInit :: m ()
  , objInit :: m ()
  , clsNm :: m ()
  , entryVal :: m ()
  , loclNm :: m ()
}

processState handler = do
  ss <- get
  case ss of
    [] -> empty handler
    state:ss' ->
      case state of
        Arr -> arr handler
        Mp -> map handler
        Obj -> obj handler
        ColInit ->
          case ss' of
            [] -> throwM $ FormatException InvalidStateException
            state':_ ->
              case state' of
                Arr -> arrInit handler <* put ss'
                Obj -> objInit handler <* put ss'
                _ -> throwM $ FormatException InvalidStateException
        ClsNm -> clsNm handler
        EntryValue -> entryVal handler
        LocalName -> loclNm handler

data FormatValidator m = FormatValidator
  { valInColV :: m ()
  , valOnlyV :: m ()
  , clsNmV :: m ()
  , objV :: m ()
  , arrInitV :: m ()
  , objInitV :: m ()
  , loclNmV :: m ()
}

formatToState fv = StateProcessor
  { empty = valOnlyV fv
  , arr = valInColV fv
  , map = valInColV fv
  , obj = objV fv
  , arrInit = arrInitV fv
  , objInit = objInitV fv
  , clsNm = clsNmV fv
  , entryVal = valOnlyV fv
  , loclNm = loclNmV fv
}

defaultValidator :: (MonadThrow m) => FormatValidator m
defaultValidator = FormatValidator
  { valInColV = throwM . FormatException $ OptionException ValueException CollectionEndException
  , valOnlyV = throwM $ FormatException ValueException
  , clsNmV = throwM . FormatException $ OptionException NamespaceException LocalNameException
  , objV = throwM . FormatException . OptionException CollectionEndException $ OptionException NamespaceException LocalNameException
  , arrInitV = throwM . FormatException . OptionException CollectionEndException $ OptionException ValueException ClassnameException
  , objInitV = throwM . FormatException . OptionException (OptionException ClassnameException CollectionEndException) $ OptionException NamespaceException LocalNameException
  , loclNmV = throwM $ FormatException LocalNameException
}

validateNonText :: (MonadThrow m) => StateT [StateStack] m ()
validateNonText = processState . formatToState
  $ defaultValidator {valOnlyV = pure (), valInColV = pure (), arrInitV = pure ()}

validateString :: (MonadThrow m) => StateT [StateStack] m ()
validateString = processState . formatToState
  $ FormatValidator
    { valInColV = pure ()
    , valOnlyV = pure ()
    , clsNmV = pure ()
    , objV = pure ()
    , arrInitV = pure ()
    , objInitV = pure ()
    , loclNmV = pure ()
  }

validateNamespace :: (MonadThrow m) => StateT [StateStack] m ()
validateNamespace = processState . formatToState
  $ defaultValidator
    { clsNmV = pure ()
    , objV = pure ()
    , objInitV = pure ()
  }

validateClassname :: (MonadThrow m) => StateT [StateStack] m ()
validateClassname = processState . formatToState
  $ defaultValidator
    { arrInitV = pure ()
    , objInitV = pure ()
  }

validateColEnd :: (MonadThrow m) => StateT [StateStack] m ()
validateColEnd = processState . formatToState
  $ defaultValidator
    { valInColV = pure ()
    , objV = pure ()
    , arrInitV = pure ()
    , objInitV = pure ()
  }

defaultTransition :: (MonadThrow m) => StateProcessor m
defaultTransition = StateProcessor
  { empty = throwM $ FormatException InvalidStateException
  , arr = throwM $ FormatException InvalidStateException
  , map = throwM $ FormatException InvalidStateException
  , obj = throwM $ FormatException InvalidStateException
  , arrInit = throwM $ FormatException InvalidStateException
  , objInit = throwM $ FormatException InvalidStateException
  , clsNm = throwM $ FormatException InvalidStateException
  , entryVal = throwM $ FormatException InvalidStateException
  , loclNm = throwM $ FormatException InvalidStateException
}

transitionNonText :: (MonadThrow m) => StateT [StateStack] m ()
transitionNonText = processState $ defaultTransition
  { empty = pure ()
  , arr = pure ()
  , map = modify $ (:) EntryValue
  , entryVal = modify tail
}

transitionColEnd :: (MonadThrow m) => StateT [StateStack] m ()
transitionColEnd = processState $ defaultTransition
  { arr = modify tail
  , map = modify tail
  , obj = modify tail
}

transitionString :: (MonadThrow m) => StateT [StateStack] m ()
transitionString = processState $ defaultTransition
  { empty = pure ()
  , arr = pure ()
  , map = modify $ (:) EntryValue
  , obj = modify $ (:) EntryValue
  , clsNm = modify tail
  , entryVal = modify tail
  , loclNm = do
    modify tail
    ss <- get
    case ss of
      [] -> throwM $ FormatException InvalidStateException
      s:ss' ->
        case s of
        Obj -> modify $ (:) EntryValue
        ClsNm -> modify tail
        _ -> pure ()
}

transitionNamespace :: (MonadThrow m) => StateT [StateStack] m ()
transitionNamespace = processState $ defaultTransition
  { obj = modify $ (:) LocalName
  , clsNm = modify $ (:) LocalName
}

transitionClassname :: (MonadThrow m) => StateT [StateStack] m ()
transitionClassname = processState $ defaultTransition
  { arr = modify $ (:) ClsNm
  , obj = modify $ (:) ClsNm
}

transitionArray :: (MonadThrow m) => StateT [StateStack] m ()
transitionArray = processState $ defaultTransition
  { empty = modify $ (:) ColInit . (:) Arr
  , arr = modify $ (:) ColInit . (:) Arr
  , map = modify $ (:) ColInit . (:) Arr
  , entryVal = modify $ (:) ColInit . (:) Arr
}

transitionMap :: (MonadThrow m) => StateT [StateStack] m ()
transitionMap = processState $ defaultTransition
  { empty = modify $ (:) Mp
  , arr = modify $ (:) Mp
  , map = modify $ (:) Mp
  , entryVal = modify $ (:) Mp
}

transitionObject :: (MonadThrow m) => StateT [StateStack] m ()
transitionObject = processState $ defaultTransition
  { empty = modify $ (:) ColInit . (:) Obj
  , arr = modify $ (:) ColInit . (:) Obj
  , map = modify $ (:) ColInit . (:) Obj
  , entryVal = modify $ (:) ColInit . (:) Obj
}

readValue :: (MonadThrow m, MonadCatch m, Stream s m p) =>
  StateT [StateStack] (StateT s m) PackType
readValue = do
  byte <- catch (lift unconsS) $ throwM . ByteStreamException
  pHandler <- get
  case idFormat byte of
    Fixint x -> validateNonText $> Int x <* transitionNonText
    FNil -> validateNonText $> Nil <* transitionNonText
    FColEnd -> validateColEnd $> ColEnd <* transitionColEnd
    FBool b -> validateNonText $> Bool b <* transitionNonText
    FUInt8 -> validateNonText *> (Int . fromIntegral <$> readBytes 1) <* transitionNonText
    FUInt16 -> validateNonText *> (Int . fromIntegral <$> readBytes 2) <* transitionNonText
    FUInt32 -> validateNonText *> (Int . fromIntegral <$> readBytes 4) <* transitionNonText
    FUInt64 -> validateNonText *> (UInt <$> readBytes 8) <* transitionNonText
    FInt8 -> do
      validateNonText
      byte <- readBytes 1
      Int (fromIntegral (fromIntegral byte :: Int8)) <$ transitionNonText
    FInt16 -> do
      validateNonText
      word <- readBytes 2
      Int (fromIntegral (fromIntegral word :: Int16)) <$ transitionNonText
    FInt32 -> do
      validateNonText
      word <- readBytes 4
      Int (fromIntegral (fromIntegral word :: Int32)) <$ transitionNonText
    FInt64 -> do
      validateNonText
      word <- readBytes 8
      Int (fromIntegral (fromIntegral word :: Int64)) <$ transitionNonText
    FFloat32 -> validateNonText *> (Float . wordToFloat . fromIntegral <$> readBytes 4) <* transitionNonText
    FFloat64 -> validateNonText *> (Double . wordToDouble <$> readBytes 8) <* transitionNonText
    FBin8 -> validateNonText *> (Bin <$> ((fromIntegral <$> readBytes 1) >>= readStream)) <* transitionNonText
    FBin16 -> validateNonText *> (Bin <$> ((fromIntegral <$> readBytes 2) >>= readStream)) <* transitionNonText
    FBin32 -> validateNonText *> (Bin <$> ((fromIntegral <$> readBytes 4) >>= readStream)) <* transitionNonText
    FStr8 -> validateString *> (Str . decodeUtf8 <$> ((fromIntegral <$> readBytes 1) >>= readStream)) <* transitionString
    FStr16 -> validateString *> (Str . decodeUtf8 <$> ((fromIntegral <$> readBytes 2) >>= readStream)) <* transitionString
    FStr32 -> validateString *> (Str . decodeUtf8 <$> ((fromIntegral <$> readBytes 4) >>= readStream)) <* transitionString
    FNs8 -> validateNamespace *> (NS . decodeUtf8 <$> ((fromIntegral <$> readBytes 1) >>= readStream)) <* transitionNamespace
    FNs16 -> validateNamespace *> (NS . decodeUtf8 <$> ((fromIntegral <$> readBytes 2) >>= readStream)) <* transitionNamespace
    FNs32 -> validateNamespace *> (NS . decodeUtf8 <$> ((fromIntegral <$> readBytes 4) >>= readStream)) <* transitionNamespace
    FClassname -> validateClassname $> Classname <* transitionClassname
    FArray -> validateNonText $> Array <* transitionArray
    FMap -> validateNonText $> Map <* transitionMap
    FObj -> validateNonText $> Object <* transitionObject
    FFixbin len -> validateNonText *> (Bin <$> readStream len) <* transitionNonText
    FFixstr len -> validateString *> (Str . decodeUtf8 <$> readStream len) <* transitionString
    FFixns len -> validateNamespace *> (NS . decodeUtf8 <$> readStream len) <* transitionNamespace
    FUnused -> throwM $ FormatException UnusedException
