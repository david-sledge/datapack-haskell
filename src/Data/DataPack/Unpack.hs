{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

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
import Control.Monad.State (MonadState, get, put, runStateT, state)
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
import Data.Source (DataSource, takeFromSource, DataSourceError(MoreData2, Source2))
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as EL
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts (Item)
import Data.ListLike (ListLike (empty), uncons)
import GHC.Generics (Generic, Generic1)

data DataStringType =
  Binary |
  String |
  NamespaceName
  deriving stock (Show, Ord, Eq, Read)

data DataSize =
  Data8 |
  Data16 |
  Data32
  deriving stock (Show, Ord, Eq, Read)

data BitSize =
  Bit8 |
  Bit16 |
  Bit32 |
  Bit64
  deriving stock (Show, Ord, Eq, Read)

data NumberType =
  NUInt BitSize |
  NInt BitSize |
  NFloat |
  NDouble
  deriving stock (Show, Ord, Eq, Read)

data UnpackState a =
  Root |
  -- has parent state of Root | Sequence | EntryValue
  Sequence (UnpackState a) |
  -- has parent state of Root | Sequence | EntryValue
  Dictionary (UnpackState a) |
  -- has parent state of Root | Sequence | EntryValue
  Object (UnpackState a) |
  -- has parent state of Root | Sequence | EntryValue
  SequenceStart (UnpackState a) |
  -- has parent state of Root | Sequence | EntryValue
  ObjectStart (UnpackState a) |
  -- has parent state of Sequence | Object
  ClassName (UnpackState a) |
  -- has parent state of Dictionary | Object
  EntryValue (UnpackState a) |
  -- has parent state of Object | ClassName
  LocalName (UnpackState a) |
  -- has parent state of Object | LocalName | ClassName | Root | Sequence | EntryValue
  DataString DataStringType a (UnpackState a) |
  -- has parent state of Object | LocalName | ClassName | Root | Sequence | EntryValue
  DataStringLen DataStringType DataSize (UnpackState a) |
  -- has parent state of Root | Sequence | EntryValue
  Number NumberType (UnpackState a) |
  -- has parent state of Dictionary | Object
  EarlyCollectionEnd (UnpackState a)
  deriving stock (Show, Ord, Eq, Read)

data UnpackError e n =
  SourceError e |
  NeedMoreData n |
  InvalidByte Word8 [(Word8, Word8)] |
  UnusedByte Word8 |
  FlogTheDeveloper String
  deriving stock (Show, Ord, Eq, Read)

data PackType =
  PNil |
  PCollectionEnd |
  PBoolean Bool |
  PUInt Word64 |
  PInt Int64 |
  PFloat Float |
  PDouble Double |
  PBin BL.ByteString |
  PStr TL.Text |
  PNs TL.Text |
  PClassName |
  PSequence |
  PDictionary |
  PObject
  deriving stock (Show, Ord, Eq, Read)

data {- Statey -} McStateface a s b = McStateface {
  unpackState :: UnpackState a,
  source :: s,
  buffer :: b
}

putStateAnd :: MonadState (McStateface a s b) m => UnpackState a -> r -> m r
putStateAnd st r = state $ \ mcS -> (r, mcS {unpackState = st})

putState :: MonadState (McStateface a s b) m => UnpackState a -> m ()
putState st = putStateAnd st ()

-- interprets byte as a signed 8-bit integer
fromByteToInt :: (Integral a, Num b) => a -> b
fromByteToInt byte = fromIntegral (fromIntegral byte :: Int8)

takeUnpack :: (
    ListLike b (Item b), MonadState (McStateface a s b) (t1 (t2 m)),
    DataSource s b n (ExceptT e (
      ReaderT (UnpackState a) (ExceptT (DataSourceError e s b n) m))),
    MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m),
    MonadError (UnpackError e n) (t1 (t2 m))) =>
  n -> t1 (t2 m) b
takeUnpack n = do
  -- get the data source and
  mcS <- get
  -- take n words from the source
  res <- lift . lift . runExceptT . runReaderT (takeFromSource n (buffer mcS) $ source mcS) $ unpackState mcS
  case res of
    Left e ->
      case e of
        MoreData2 src' buff n' -> do
          put $ mcS {source = src', buffer = buff}
          throwError $ NeedMoreData n'
        Source2 ex -> throwError $ SourceError ex
    Right (d, src') -> state $ const (d, mcS {source = src', buffer = empty})

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

readNum :: (
    Item b ~ Word8, ListLike b Word8, MonadTrans t2, MonadTrans t1,
    MonadState (McStateface a s b) (t1 (t2 m)), Bits b1,
    DataSource s b n (ExceptT e (
      ReaderT (UnpackState a) (ExceptT (DataSourceError e s b n) m))),
    MonadError (UnpackError e n) (t1 (t2 m)), Num b1, Monad m,
    Monad (t2 m)) =>
  n -> (b1 -> b2) -> t1 (t2 m) b2
readNum n f = f . toWord n <$> takeUnpack n

readUint8, readUint16, readUint32, read64 :: (
    Item b ~ Word8, ListLike b Word8, MonadTrans t2, MonadTrans t1,
    MonadState (McStateface a s b) (t1 (t2 m)),
    DataSource s b n (ExceptT e (
      ReaderT (UnpackState a) (ExceptT (DataSourceError e s b n) m))),
    MonadError (UnpackError e n) (t1 (t2 m)), Num b1, Monad m,
    Monad (t2 m)) => t1 (t2 m) b1
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
    DataSource s BL.ByteString n (ExceptT e (ReaderT (UnpackState n) (ExceptT (DataSourceError e s BL.ByteString n) m))),
    MonadError (UnpackError e n) (t1 (t2 m)),
    MonadTrans t2, MonadTrans t1,
    MonadState (McStateface n s BL.ByteString) (t1 (t2 m)), Monad m, Monad (t2 m)) =>
  t1 (t2 m) PackType
unpackData = do
  mcS <- get
  let st = unpackState mcS
      orderTheDeveloperFlogged = throwError . FlogTheDeveloper
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

newtype Wrapper a = Wrapper (UnpackState a)
  deriving stock ( Eq
                 , Ord
                 , Show
                 , Generic
                 , Generic1
                 )

unwrap :: Wrapper a -> UnpackState a
unwrap (Wrapper s) = s

unpackNext :: (Monad m,
    DataSource s BL.ByteString n (ExceptT e (ReaderT
      (UnpackState n)
      (ExceptT (DataSourceError e s BL.ByteString n) m)))) =>
  (Wrapper n, s, BL.ByteString) -> m (Either (UnpackError e n) PackType,
    (Wrapper n, s, BL.ByteString))
unpackNext (Wrapper st, src, buff) = do
  (res, mcS) <- runStateT (runExceptT unpackData) $ McStateface st src buff
  pure (res, (Wrapper $ unpackState mcS, source mcS, buffer mcS))

wrapRoot :: Wrapper a
wrapRoot = Wrapper Root

unpack :: (
    DataSource s BL.ByteString n (ExceptT e (ReaderT
      (UnpackState n)
      (ExceptT (DataSourceError e s BL.ByteString n) m))),
    Monad m) =>
  s -> m (Either (UnpackError e n) PackType,
    (Wrapper n, s, BL.ByteString))
unpack src = unpackNext (wrapRoot, src, empty)
--}
