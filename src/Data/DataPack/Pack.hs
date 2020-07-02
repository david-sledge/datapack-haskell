{-# LANGUAGE FunctionalDependencies #-}

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

module Data.DataPack.Pack (
  DataDestination,
  PackState(..),
  PackCatchers(..),
  give,
  packNil,
  packFalse,
  packTrue,
  packInt,
  packFloat,
  packDouble,
  packStringValue,
  packBinValue,
  packSequence,
  packDictionary,
  packObject,
  packProperty,
  packDataT,
) where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString.Lazy as C
import Data.Int
import Data.Maybe
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.Word
import Data.DataPack

class DataDestination d e | d -> e where
    give ::
      C.ByteString
       -> d -- destination value
       -> (e -> (Word32, d) -> r) -- error handler
       -> (d -> r) -- success handler
       -> r -- result of error or success handler

data PackState = Root
  | Sequence PackState
  | Dictionary PackState
  | Object PackState
  | EntryValue PackState
  | PostNilEntryValue PackState
  deriving (Ord, Eq, Show)

data States d = States {
     packState :: PackState,
     destination :: d }
   deriving (Show, Ord, Eq)

type Catch d c = PackState -> d -> c

type Handle d r = Catch d ((d -> r) -> r)

-- exception handlers
data PackCatchers e d r = PackCatchers {
    stream :: e -> Word32 -> Catch d ((d -> r) -> r),
    tooBig :: C.ByteString -> Handle d r,
    invalidPackType :: Handle d r,
    flogTheDeveloper :: Catch d r }

putDestination d = modify (\states -> states { destination = d })

putState state = modify (\states -> states { packState = state })

getState f = get >>= f . packState

givePack dat =
  get >>= \s ->
  give dat (destination s) (
      \e (n, d') ->
        contAsk $ \catchers ->
        stateReaderContT (stream catchers e n (packState s) d') >>=
        putDestination )
    $ \d' -> putDestination d'

fromWord word n byteString =
  let n' = n - 1 in
  if n > 0
  then fromWord (shiftR word 8) n' $
    C.cons (fromIntegral word) byteString
  else byteString

-- verify + transition = verisition
verisitionValue pack = get >>= \s ->
  let state = packState s in
  case state of
  Dictionary _ -> pack >> putState (EntryValue state)
  EntryValue parentState -> pack >> putState parentState
  PostNilEntryValue parentState -> packSingleByte nilByte >> case parentState of
    Dictionary _ -> pack >> putState (EntryValue parentState)
    Object _ -> packSingleByte nilByte >>
      putState (EntryValue state) >>
      pack
    _ -> contAsk $ \catchers -> stateReaderContT . const . flogTheDeveloper catchers state $ destination s
  Object _ -> packSingleByte nilByte >>
    putState (EntryValue state) >>
    pack
  _ -> pack

packSingleByte byte = givePack . C.cons byte $ C.empty

packNil :: (DataDestination d e) =>
  StateT (States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) ()
packNil = get >>= \s ->
  let
  pack = packSingleByte nilByte
  state = packState s
  in
  case state of
    Dictionary _ -> pack >> putState (EntryValue state)
    EntryValue parentState -> putState (PostNilEntryValue parentState)
    PostNilEntryValue parentState -> pack >> pack >> case parentState of
      Dictionary _ -> putState (EntryValue parentState)
      Object _ -> putState (PostNilEntryValue parentState)
      _ -> contAsk $ \catchers -> stateReaderContT . const . flogTheDeveloper catchers state $ destination s
    Object _ -> pack >>
      putState (PostNilEntryValue state)
    _ -> pack

packFalse :: (DataDestination d e) =>
  StateT (States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) ()
packFalse = verisitionValue $ packSingleByte falseByte

packTrue :: (DataDestination d e) =>
  StateT (States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) ()
packTrue = verisitionValue $ packSingleByte trueByte

packPrefixedBytes byte word numBytes byteString =
    givePack . C.cons byte $ fromWord word numBytes byteString

packNumberBytes byte word numBytes =
  packPrefixedBytes byte word numBytes C.empty

packInt :: (Num b, Bits b, Ord b, Integral b, DataDestination d e) =>
  b -> StateT (States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) ()
packInt word = verisitionValue (
    case word of
    word | 0 <= word && (0x7fffffffffffffff::Word64) < fromIntegral word ->
      packNumberBytes uint64Byte (fromIntegral word::Word64) 8
    _ -> case fromIntegral word::Int64 of
      int64 | int64 < -0x7fffffff - 1 || 0xffffffff < int64 ->
        packNumberBytes int64Byte int64 8
            | 0x7fffffff < int64 ->
        packNumberBytes uint32Byte (fromIntegral word::Word32) 4
            | int64 < -0x7fff - 1 || 0xffff < int64 ->
        packNumberBytes int32Byte (fromIntegral word::Int32) 4
            | 0x7fff < int64 ->
        packNumberBytes uint16Byte (fromIntegral word::Word16) 2
            | int64 < -0x7f - 1 || 0xff < int64 ->
        packNumberBytes int16Byte (fromIntegral word::Int16) 2
            | 0x7f < int64 ->
        packNumberBytes uint8Byte (fromIntegral word::Word8) 1
            | int64 < -0x3f - 1 || 0x3f < int64 ->
        packNumberBytes int8Byte (fromIntegral word::Int8) 1
      _ -> givePack $ fromWord (fromIntegral word::Int8) 1 C.empty )

packFloat float = packNumberBytes floatByte (floatToWord float) 4

packDouble double = packNumberBytes doubleByte (doubleToWord double) 8

packData byteString fixbyte byte8 byte16 byte32 = let
    len = C.length byteString
  in
  case len of
  len | len <= 0x1f ->
        givePack $ C.cons (fixbyte .|. (fromIntegral len::Word8)) byteString
      | len <= 0xff ->
        packPrefixedBytes byte8 (fromIntegral len::Word8) 1 byteString
      | len <= 0xffff ->
        packPrefixedBytes byte16 (fromIntegral len::Word16) 2 byteString
      | len <= 0xffffffff ->
        packPrefixedBytes byte32 (fromIntegral len::Word32) 4 byteString
  _ -> contAsk $ \catchers ->
      get >>= \s -> (stateReaderContT . tooBig catchers byteString (packState s) $ destination s) >>= putDestination

packText text = packData (encodeUtf8 text) fixstrMask str8Byte str16Byte str32Byte

packString string = packText $ T.pack string

packStringValue string = verisitionValue $ packString string

packBin byteString = packData byteString fixbinMask bin8Byte bin16Byte bin32Byte

packBinValue byteString = verisitionValue $ packBin byteString

packNsText nsName = packData (encodeUtf8 nsName) fixnsMask ns8Byte ns16Byte ns32Byte

packNs nsName = packNsText $ T.pack nsName

packCollectionEnd :: (DataDestination d e) =>
  StateT (States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) ()
packCollectionEnd = get >>= \s -> let
    packEnd = packSingleByte collectionEndByte
    state = packState s
  in
  case state of
    Root -> contAsk $ \catchers -> stateReaderContT . const . flogTheDeveloper catchers state $ destination s
    _ -> packEnd

packQName nsName localName = let
    packStr = packString localName
  in
  case nsName of
  Just name -> packNs name >> packStr
  _ -> packStr

packClassName nsName localName =
  packSingleByte classNameByte >>
  packQName nsName localName

transitionToCollection state packContents = verisitionValue (
  getState $ \packState ->
    putState (state packState) >>
    packContents >>
    packCollectionEnd >>
    putState packState )

packSequence className packContents = let
    transition = transitionToCollection Sequence packContents
  in
  packSingleByte sequenceByte >>
  case className of
  Just (nsName, localName) -> packClassName nsName localName >> transition
  _ -> transition

packDictionary packContents = packSingleByte dictionaryByte >>
  transitionToCollection Dictionary packContents

packObject className packContents = let
    transition = transitionToCollection Object packContents
  in
  packSingleByte objectByte >>
  case className of
  Just (nsName, localName) -> packClassName nsName localName >> transition
  _ -> transition

packProperty nsName localName packValue =
  get >>= \s -> let
  state = packState s
  in
  case state of
  Object _ ->
    packQName nsName localName >>
    putState (EntryValue state) >>
    packValue
  PostNilEntryValue (Object _) ->
    packSingleByte nilByte >>
    packQName nsName localName >>
    putState (EntryValue state) >>
    packValue
  _ -> contAsk $ \catchers ->
      stateReaderContT (invalidPackType catchers state $ destination s) >>= putDestination

packDataT d catchers packRoot c =
  (runContT $ (runReaderT . runStateT packRoot $ States Root d) catchers) (
    \(_, states) ->
      case packState states of
        Root -> c $ destination states
        _ -> flogTheDeveloper catchers (packState states) $ destination states
  )
