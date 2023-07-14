{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.DataPack.Unpack
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

module Data.DataPack.Unpack (
    fixintMask,
    nilByte,
    collectionEndByte,
    falseByte,
    trueByte,
    uint8Byte,
    uint16Byte,
    uint32Byte,
    uint64Byte,
    int8Byte,
    int16Byte,
    int32Byte,
    int64Byte,
    floatByte,
    doubleByte,
    bin8Byte,
    bin16Byte,
    bin32Byte,
    str8Byte,
    str16Byte,
    str32Byte,
    ns8Byte,
    ns16Byte,
    ns32Byte,
    classNameByte,
    sequenceByte,
    dictionaryByte,
    objectByte,
    fixbinMask,
    fixstrMask,
    fixnsMask,
    fixMask,
    lenMask,

    BitSize(..),
    DataSize(..),
    DataStringType(..),
    NumberType(..),
    PackType(..),
    UnpackError(..),
    UnpackState(..),
    Wrapper,
    unpack,
    unpackNext,
    unwrap,
    validBytesForState,
    wrapRoot,
  ) where

import Prelude hiding (head, length, take)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (MonadState, get, runStateT, state)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Data.Binary.IEEE754 (wordToDouble, wordToFloat)
import Data.Bits (Bits, shiftL, xor, (.&.), (.|.))
import Data.ByteString.Lazy qualified as BL
import Data.DataPack
    ( fixintMask,
      nilByte,
      collectionEndByte,
      falseByte,
      trueByte,
      uint8Byte,
      uint16Byte,
      uint32Byte,
      uint64Byte,
      int8Byte,
      int16Byte,
      int32Byte,
      int64Byte,
      floatByte,
      doubleByte,
      bin8Byte,
      bin16Byte,
      bin32Byte,
      str8Byte,
      str16Byte,
      str32Byte,
      ns8Byte,
      ns16Byte,
      ns32Byte,
      classNameByte,
      sequenceByte,
      dictionaryByte,
      objectByte,
      fixbinMask,
      fixstrMask,
      fixnsMask,
      fixMask,
      lenMask )
import Data.Int (Int8, Int64, Int16, Int32)
import Data.Source (DataSource, takeFromSource, DataSourceError)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as EL
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts (Item)
import Data.ListLike (ListLike, uncons)
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1)

type DataStringType :: Type
data DataStringType where
  Binary :: DataStringType
  String :: DataStringType
  NamespaceName :: DataStringType
  deriving stock (Show, Ord, Eq, Read)

type DataSize :: Type
data DataSize where
  Data8 :: DataSize
  Data16 :: DataSize
  Data32 :: DataSize
  deriving stock (Show, Ord, Eq, Read)

type BitSize :: Type
data BitSize where
  Bit8 :: BitSize
  Bit16 :: BitSize
  Bit32 :: BitSize
  Bit64 :: BitSize
  deriving stock (Show, Ord, Eq, Read)

type NumberType :: Type
data NumberType where
  NUInt :: BitSize -> NumberType
  NInt :: BitSize -> NumberType
  NFloat :: NumberType
  NDouble :: NumberType
  deriving stock (Show, Ord, Eq, Read)

type UnpackState :: Type -> Type
data UnpackState a where
  Root :: UnpackState a

  -- has parent state of Root | Sequence | EntryValue
  Sequence :: UnpackState a -> UnpackState a

  -- has parent state of Root | Sequence | EntryValue
  Dictionary :: UnpackState a -> UnpackState a

  -- has parent state of Root | Sequence | EntryValue
  Object :: UnpackState a -> UnpackState a

  -- has parent state of Root | Sequence | EntryValue
  SequenceStart :: UnpackState a -> UnpackState a

  -- has parent state of Root | Sequence | EntryValue
  ObjectStart :: UnpackState a -> UnpackState a

  -- has parent state of Sequence | Object
  ClassName :: UnpackState a -> UnpackState a

  -- has parent state of Dictionary | Object
  EntryValue :: UnpackState a -> UnpackState a

  -- has parent state of Object | ClassName
  LocalName :: UnpackState a -> UnpackState a

  -- has parent state of Object | LocalName | ClassName | Root | Sequence | EntryValue
  DataString :: DataStringType -> a -> UnpackState a -> UnpackState a

  -- has parent state of Object | LocalName | ClassName | Root | Sequence | EntryValue
  DataStringLen :: DataStringType -> DataSize -> UnpackState a -> UnpackState a

  -- has parent state of Root | Sequence | EntryValue
  Number :: NumberType -> UnpackState a -> UnpackState a

  -- has parent state of Dictionary | Object
  EarlyCollectionEnd :: UnpackState a -> UnpackState a
  deriving stock (Show, Ord, Eq, Read)

type UnpackError :: Type -> Type -> Type -> Type
data UnpackError e a s where
  SourceError :: DataSourceError e a -> UnpackError e a s
  InvalidByte :: Word8 -> [(Word8, Word8)] -> UnpackError e a s
  UnusedByte :: Word8 -> UnpackError e a s
  FlogTheDeveloper :: String -> (UnpackState a, s) -> UnpackError e a s
  deriving stock (Show, Ord, Eq, Read)

type PackType :: Type
data PackType where
  PNil :: PackType
  PCollectionEnd :: PackType
  PBoolean :: Bool -> PackType
  PUInt :: Word64 -> PackType
  PInt :: Int64 -> PackType
  PFloat :: Float -> PackType
  PDouble :: Double -> PackType
  PBin :: BL.ByteString -> PackType
  PStr :: TL.Text -> PackType
  PNs :: TL.Text -> PackType
  PClassName :: PackType
  PSequence :: PackType
  PDictionary :: PackType
  PObject :: PackType
  deriving stock (Show, Ord, Eq, Read)

putSourceAnd :: MonadState (a1, b) m => b -> a2 -> m a2
putSourceAnd src r = state $ \ (st, _) -> (r, (st, src))

putStateAnd :: MonadState (a1, b) m => a1 -> a2 -> m a2
putStateAnd st r = state $ \ (_, src) -> (r, (st, src))

putState :: MonadState (a1, b) m => a1 -> m ()
putState st = putStateAnd st ()

-- interprets byte as a signed 8-bit integer
fromByteToInt :: (Integral a, Num b) => a -> b
fromByteToInt byte = fromIntegral (fromIntegral byte :: Int8)

takeUnpack :: (
    MonadError (UnpackError e a s) (t1 (t2 m)), Integral a,
    ListLike ll (Item ll), Monad m, Monad (t2 m),
    MonadTrans t2, MonadTrans t1,
    DataSource s ll (ReaderT st (ExceptT (DataSourceError e a) m)),
    MonadState (st, s) (t1 (t2 m))) => a -> t1 (t2 m) ll
takeUnpack n = do
  -- get the data source and
  (st, src) <- get
  -- take n words from the source
  res <- lift . lift . runExceptT $ runReaderT (takeFromSource n src) st
  case res of
    Left e -> throwError $ SourceError e
    Right (d, src') -> putSourceAnd src' d

toWord :: (Bits a, ListLike bc Word8, Num a, Integral n) => n -> bc -> a
toWord n byteContainer =
  let n' = n - 1 in
  if n > 0
  then
    case uncons byteContainer of
    Just (byte, byteContainer') ->
      shiftL (fromIntegral byte) (fromIntegral $ n' * 8) .|.
        toWord n' byteContainer'
    _ -> 0
  else 0

readBytesToWord :: (
    Bits b, Integral a, ListLike bc Word8, MonadError (UnpackError e a s) (t1 (t2 m)),
    MonadTrans t2, MonadTrans t1,
    DataSource s bc (ReaderT st (ExceptT (DataSourceError e a) m)),
    MonadState (st, s) (t1 (t2 m)), Monad m, Monad (t2 m), Num b) =>
  a -> t1 (t2 m) b
readBytesToWord n = toWord n <$> takeUnpack n

readNum :: (
    Bits b, Integral a, ListLike bc Word8, MonadError (UnpackError e a s) (t1 (t2 m)),
    MonadTrans t2, MonadTrans t1,
    DataSource s bc (ReaderT st (ExceptT (DataSourceError e a) m)),
    MonadState (st, s) (t1 (t2 m)), Monad m, Monad (t2 m), Num b) =>
  a -> (b -> c) -> t1 (t2 m) c
readNum n f = f <$> readBytesToWord n

readUint8, readUint16, readUint32, read64 :: (
    Integral a, ListLike bc Word8, MonadError (UnpackError e a s) (t1 (t2 m)),
    MonadTrans t2, MonadTrans t1,
    DataSource s bc (ReaderT st (ExceptT (DataSourceError e a) m)),
    MonadState (st, s) (t1 (t2 m)), Monad m, Monad (t2 m), Num b) => t1 (t2 m) b
readUint8 = readNum 1 (fromIntegral::Num c => Word8 -> c)
readUint16 = readNum 2 (fromIntegral::Num c => Word16 -> c)
readUint32 = readNum 4 (fromIntegral::Num c => Word32 -> c)
read64 = readNum 8 (fromIntegral::Num c => Word64 -> c)

validBytesForState :: UnpackState a -> [(Word8, Word8)]
validBytesForState st =
  let valueRanges = [
          (fixintMask, fixintMask `xor` 0xff),
          (nilByte, nilByte),
          (falseByte, trueByte),
          (uint8Byte, uint64Byte),
          (int8Byte, int64Byte),
          (floatByte, doubleByte),
          (bin8Byte, bin32Byte),
          (str8Byte, str32Byte),
          (sequenceByte, sequenceByte),
          (dictionaryByte, dictionaryByte),
          (objectByte, objectByte),
          (fixbinMask, fixbinMask .|. lenMask),
          (fixstrMask, fixstrMask .|. lenMask) ]
      sequenceRanges = (collectionEndByte, collectionEndByte):valueRanges
      localnameRanges = [
          (bin8Byte, bin32Byte),
          (str8Byte, str32Byte),
          (fixbinMask, fixbinMask .|. lenMask),
          (fixstrMask, fixstrMask .|. lenMask) ]
      qnameRanges =
          (ns8Byte, ns32Byte):(fixnsMask, fixstrMask .|. lenMask):localnameRanges
      propNameRanges = (nilByte, nilByte):
          (collectionEndByte, collectionEndByte):qnameRanges
  in
  case st of
    Sequence _ -> sequenceRanges
    SequenceStart _ -> (classNameByte, classNameByte):sequenceRanges
    Dictionary _ -> sequenceRanges
    Object _ -> propNameRanges
    ObjectStart _ -> (classNameByte, classNameByte):propNameRanges
    ClassName _ -> qnameRanges
    EntryValue _ -> sequenceRanges
    LocalName _ -> localnameRanges
    Root -> valueRanges
    _ -> [(0, 0xff)]

unpackData :: (
    DataSource s BL.ByteString (ReaderT (UnpackState a) (ExceptT (DataSourceError e a) m)),
    Integral a, MonadError (UnpackError e a s) (t1 (t2 m)),
    MonadTrans t2, MonadTrans t1,
    MonadState (UnpackState a, s) (t1 (t2 m)), Monad m, Monad (t2 m)) =>
  t1 (t2 m) PackType
unpackData = do
  (st, _) <- get
  let orderTheDeveloperFlogged label = do
        s <- get
        throwError $ FlogTheDeveloper label s
      transitionValueState fromState r =
        let f st' = putStateAnd st' r in
        case fromState of
          Root -> f fromState
          Sequence _ -> f fromState
          Dictionary _ -> f $ EntryValue fromState
          SequenceStart toState -> f $ Sequence toState
          EntryValue toState -> f toState
          _ -> orderTheDeveloperFlogged "value transition"
      -- validate & transition = valisition
      valisitionCollectionEndState fromState entVal e =
        let f st' = transitionValueState st' PCollectionEnd in
        case fromState of
          Sequence state' -> f state'
          Dictionary state' -> f state'
          Object state' -> f state'
          SequenceStart state' -> f state'
          ObjectStart state' -> f state'
          EntryValue state'' -> entVal state'' -- putStateAnd (EarlyCollectionEnd state'') PNil | Flog
          _ -> e -- Invalid | Flog
  case st of
    EarlyCollectionEnd parentState -> valisitionCollectionEndState parentState (const $ orderTheDeveloperFlogged "collection end state") $ orderTheDeveloperFlogged "How did this get here?"
    DataStringLen typ siz fromState -> do
      len <- case siz of
        Data8 -> readUint8
        Data16 -> readUint16
        Data32 -> readUint32
      putState $ DataString typ len fromState
      unpackData
    DataString typ len parentState -> do
      b <- takeUnpack len
      let t = EL.decodeUtf8With lenientDecode b
          f st' = putStateAnd st' $ case typ of
            Binary -> PBin b
            String -> PStr t
            NamespaceName -> PNs t
      case typ of
        NamespaceName ->
          case parentState of
            Object _ -> f $ LocalName parentState
            ClassName _ -> f $ LocalName parentState
            _ -> orderTheDeveloperFlogged ""
        _ ->
          case parentState of
            Sequence _ -> f parentState
            Dictionary _ -> f $ EntryValue parentState
            Object _ -> f $ EntryValue parentState
            SequenceStart toState -> f $ Sequence toState
            ObjectStart toState -> f . EntryValue $ Object toState
            ClassName toState -> f toState
            EntryValue toState -> f toState
            LocalName toState ->
              case toState of
              -- transition to entry value (object) or
              Object _ -> f $ EntryValue toState
              -- pop ClassName (object/sequence) off the state
              ClassName jumpState -> f jumpState
              -- Programmatic error in this library
              _ -> orderTheDeveloperFlogged ""
            Root -> f parentState
            _ -> orderTheDeveloperFlogged ""
    Number typ parentState -> do
      transitionValueState parentState =<< (
        case typ of
        NUInt bitSize ->
          case bitSize of
          Bit8 -> PUInt <$> readUint8
          Bit16 -> PUInt <$> readUint16
          Bit32 -> PUInt <$> readUint32
          Bit64 -> PUInt <$> read64
        NInt bitSize ->
          case bitSize of
          Bit8 -> PInt <$> readNum 1 (fromByteToInt::Num a => Word8 -> a)
          Bit16 -> PInt <$> readNum 2 ((\biByte ->
            fromIntegral (fromIntegral biByte :: Int16))::Num a => Word16 -> a
            )
          Bit32 -> PInt <$> readNum 4 ((\quByte ->
            fromIntegral (fromIntegral quByte :: Int32))::Num a => Word32 -> a
            )
          Bit64 -> PInt <$> read64
        NFloat -> PFloat <$> readNum 4 wordToFloat
        NDouble -> PDouble <$> readNum 8 wordToDouble
        )
    fromState -> do
      b <- takeUnpack 1
      let byte = BL.head b
          e = throwError . InvalidByte byte $ validBytesForState fromState
          validateValueState g =
            case fromState of
            Root -> g
            Sequence _ -> g
            Dictionary _ -> g
            SequenceStart _ -> g
            EntryValue _ -> g
            _ -> e
          boolValisitionState = validateValueState . transitionValueState fromState . PBoolean
          numValisitionState numType = validateValueState $ do
            putState . Number numType $ case fromState of SequenceStart toState -> Sequence toState; _ -> fromState
            unpackData
          uintValisitionState = numValisitionState . NUInt
          intValisitionState = numValisitionState . NInt
          validateStringState g =
            case fromState of
            Root -> g
            Sequence _ -> g
            Dictionary _ -> g
            Object _ -> g
            SequenceStart _ -> g
            ObjectStart _ -> g
            ClassName _ -> g
            EntryValue _ -> g
            LocalName toState ->
              case toState of
              Object _ -> g
              ClassName _ -> g
              _ -> e
          valisitionToDataStringState typ siz = validateStringState $ do
            let f parentState = putState $ DataStringLen typ siz parentState
                g = f fromState
            case fromState of
              Root -> g
              Sequence _ -> g
              Dictionary _ -> g
              Object _ -> g
              SequenceStart state' -> f $ Sequence state'
              ObjectStart state' -> f . EntryValue $ Object state'
              ClassName _ -> g
              EntryValue _ -> g
              LocalName _ -> g
            unpackData
          valisitionToBinaryState = valisitionToDataStringState Binary
          valisitionToStringState = valisitionToDataStringState String
          valisitionToNsNameState siz =
            let f parentState = putState $ DataStringLen NamespaceName siz parentState
                g = f fromState
                h = do
                  case fromState of
                    Object _ -> g
                    ObjectStart state' -> f $ Object state'
                    ClassName _ -> g
                    _ -> orderTheDeveloperFlogged $ show siz
                  unpackData
            in
            case fromState of
              Object _ -> h
              ObjectStart _ -> h
              ClassName _ -> h
              _ -> e
          valisitionCollectionState colSt packType =
            let f st' = putStateAnd (colSt st') packType
                g = f fromState
            in
            case fromState of
            Root -> g
            Sequence _ -> g
            Dictionary _ -> g
            SequenceStart parentState -> f $ Sequence parentState
            EntryValue _ -> g
            _ -> e
      case byte of
        0x40 {- nilByte -} ->
          let f st' = putStateAnd st' PNil in
          case fromState of
          Root -> f fromState
          Sequence _ -> f fromState
          Dictionary _ -> f $ EntryValue fromState
          Object _ -> f $ EntryValue fromState
          SequenceStart state' -> f $ Sequence state'
          ObjectStart state' -> f . EntryValue $ Object state'
          EntryValue state' -> f state'
          _ -> e
        0x41 {- collectionEndByte -} -> valisitionCollectionEndState fromState (\ st' -> putStateAnd (EarlyCollectionEnd st') PNil) e
        0x42 {- falseByte -} -> boolValisitionState False
        0x43 {- trueByte -} -> boolValisitionState True
        0x44 {- uint8Byte -} -> uintValisitionState Bit8
        0x45 {- uint16Byte -} -> uintValisitionState Bit16
        0x46 {- uint32Byte -} -> uintValisitionState Bit32
        0x47 {- uint64Byte -} -> uintValisitionState Bit64
        0x48 {- int8Byte -} -> intValisitionState Bit8
        0x49 {- int16Byte -} -> intValisitionState Bit16
        0x4a {- int32Byte -} -> intValisitionState Bit32
        0x4b {- int64Byte -} -> intValisitionState Bit64
        0x4c {- floatByte -} -> numValisitionState NFloat
        0x4d {- doubleByte -} -> numValisitionState NDouble
        0x4e {- bin8Byte -} -> valisitionToBinaryState Data8
        0x4f {- bin16Byte -} -> valisitionToBinaryState Data16
        0x50 {- bin32Byte -} -> valisitionToBinaryState Data32
        0x51 {- str8Byte -} -> valisitionToStringState Data8
        0x52 {- str16Byte -} -> valisitionToStringState Data16
        0x53 {- str32Byte -} -> valisitionToStringState Data32
        0x54 {- ns8Byte -} -> valisitionToNsNameState Data8
        0x55 {- ns16Byte -} -> valisitionToNsNameState Data16
        0x56 {- ns32Byte -} -> valisitionToNsNameState Data32
        0x57 {- classNameByte -} ->
          let f st' = putStateAnd (ClassName st') PClassName in
          case fromState of
          SequenceStart state' -> f $ Sequence state'
          ObjectStart state' -> f $ Object state'
          _ -> e
        0x58 {- sequenceByte -} -> valisitionCollectionState SequenceStart PSequence
        0x59 {- dictionaryByte -} -> valisitionCollectionState Dictionary PDictionary
        0x5a {- objectByte -} -> valisitionCollectionState ObjectStart PObject
        _ ->
          let len = fromIntegral $ byte .&. lenMask
              valisitionToDataFixStringState typ = validateStringState $ do
                let f parentState = putState $ DataString typ len parentState
                    g = f fromState
                case fromState of
                  Root -> g
                  Sequence _ -> g
                  Dictionary _ -> g
                  Object _ -> g
                  SequenceStart state' -> f $ Sequence state'
                  ObjectStart state' -> f . EntryValue $ Object state'
                  ClassName _ -> g
                  EntryValue _ -> g
                  LocalName _ -> g
                unpackData
          in
          -- a fix data type?
          case byte .&. fixMask of
          0x60 {- fixbinMask -} -> valisitionToDataFixStringState Binary
          0x80 {- fixstrMask -} -> valisitionToDataFixStringState String
          0xa0 {- fixnsMask -} ->
            let f parentState = putState $ DataString NamespaceName len parentState
                g = f fromState
                h = do
                  case fromState of
                    Object _ -> g
                    ObjectStart state' -> f $ Object state'
                    ClassName _ -> g
                    _ -> orderTheDeveloperFlogged "fixNs"
                  unpackData
            in
            case fromState of
              Object _ -> h
              ObjectStart _ -> h
              ClassName _ -> h
              _ -> e
          _ -> 
            let value = byte .&. fixintMask in
            -- is fixint?
            if value == 0 || value == fixintMask
            -- yes:
            then validateValueState . transitionValueState fromState . PInt $ fromByteToInt byte
            -- 0x5b, 0x5c, 0x5d, 0x5e, 0x5f
            else throwError $ UnusedByte byte

type Wrapper :: Type -> Type
newtype Wrapper a = Wrapper (UnpackState a)
  deriving stock ( Eq
                 , Ord
                 , Show
                 , Generic
                 , Generic1
                 )

unwrap :: Wrapper a -> UnpackState a
unwrap (Wrapper s) = s

unpackNext :: (
    DataSource s BL.ByteString (ReaderT (UnpackState a) (ExceptT (DataSourceError e a) m)),
    Integral a, Monad m) =>
  s -> Wrapper a -> m (Either (UnpackError e a s) PackType, (s, Wrapper a))
unpackNext src (Wrapper st) = do
  (res, (st', src')) <- runStateT (runExceptT unpackData) (st, src)
  pure (res, (src', Wrapper st'))

wrapRoot :: Wrapper a
wrapRoot = Wrapper Root

unpack :: (
    DataSource s BL.ByteString (ReaderT (UnpackState a) (ExceptT (DataSourceError e a) m)),
    Integral a, Monad m) =>
  s -> m (Either (UnpackError e a s) PackType, (s, Wrapper a))
unpack src = unpackNext src wrapRoot
