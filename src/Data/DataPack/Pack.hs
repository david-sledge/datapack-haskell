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
  PackState(Root),
  PackCatchers(..),
  States,
  give,
  packNil,
  packFalse,
  packTrue,
  packInt,
  packFloat,
  packDouble,
  packString,
  packBin,
  packSequenceStart,
  packDictionaryStart,
  packObjectStart,
  packPropertyName,
  packCollectionEnd,
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
       -> (e -> Word32 -> d -> r) -- error handler
       -> (d -> r) -- success handler
       -> r -- result of error or success handler

data PackState = Root
  | Sequence PackState
  | Dictionary PackState
  | Object PackState
  | EntryValue PackState
  | PostNilEntryValue PackState
  deriving (Ord, Eq, Show)

data States d u = States {
    packState :: PackState,
    destination :: d,
    userState :: u
  }
  deriving (Show, Ord, Eq)

type Catch d c = PackState -> d -> c

type Handle d r = Catch d ((d -> r) -> r)

-- exception handlers
data PackCatchers e d u r = PackCatchers {
    stream :: e -> Word32 -> PackState -> d -> u -> (((d, u) -> r) -> r),
    tooBig :: C.ByteString -> PackState -> d -> u -> (((d, u) -> r) -> r),
    invalidPackType :: PackState -> d -> u -> (((d, u) -> r) -> r),
    flogTheDeveloper :: PackState -> d -> u -> r }

_putDestination d = modify (\states -> states { destination = d })

_modifyState f = modify (\states -> states { packState = f (packState states) })

_putState state = modify (\states -> states { packState = state })

_putUserState state = modify (\states -> states { userState = state })

_givePack dat =
  get >>= \s ->
  give dat (destination s) (
      \e n d' ->
        contAsk $ \catchers ->
        stateReaderContT (stream catchers e n (packState s) d' (userState s)) >>= \(d, u) ->
        _putDestination d >> _putUserState u)
    $ \d' -> _putDestination d'

_fromWord word n byteString =
  let n' = n - 1 in
  if n > 0
  then _fromWord (shiftR word 8) n' $
    C.cons (fromIntegral word) byteString
  else byteString

_orderTheDeveloperFlogged state s = contAsk $
  \catchers -> stateReaderContT . const . flogTheDeveloper catchers state (destination s) $ userState s

-- verify + transition = verisition
_verisitionValue pack = _verisitionValueStart (_verisitionValueEnd pack)

_packSingleByte byte = _givePack . C.cons byte $ C.empty

packNil :: (DataDestination d e) =>
  StateT (States d u) (ReaderT (PackCatchers e d u (m r)) (ContT r m)) ()
packNil = get >>= \s ->
  let
  pack = _packSingleByte nilByte
  state = packState s
  in
  case state of
    Dictionary _ -> pack >> _putState (EntryValue state)
    EntryValue parentState -> _putState (PostNilEntryValue parentState)
    PostNilEntryValue parentState -> pack >> pack >> case parentState of
      Dictionary _ -> _putState (EntryValue parentState)
      Object _ -> _putState (PostNilEntryValue parentState)
      _ -> _orderTheDeveloperFlogged state s
    Object _ -> pack >>
      _putState (PostNilEntryValue state)
    _ -> pack

packFalse :: (DataDestination d e) =>
  StateT (States d u) (ReaderT (PackCatchers e d u (m r)) (ContT r m)) ()
packFalse = _verisitionValue $ _packSingleByte falseByte

packTrue :: (DataDestination d e) =>
  StateT (States d u) (ReaderT (PackCatchers e d u (m r)) (ContT r m)) ()
packTrue = _verisitionValue $ _packSingleByte trueByte

_packPrefixedBytes byte word numBytes byteString =
    _givePack . C.cons byte $ _fromWord word numBytes byteString

_packNumberBytes byte word numBytes =
  _packPrefixedBytes byte word numBytes C.empty

packInt word = _verisitionValue (
    case word of
    word | 0 <= word && (0x7fffffffffffffff::Word64) < fromIntegral word ->
      _packNumberBytes uint64Byte (fromIntegral word::Word64) 8
    _ -> case fromIntegral word::Int64 of
      int64 | int64 < -0x7fffffff - 1 || 0xffffffff < int64 ->
        _packNumberBytes int64Byte int64 8
            | 0x7fffffff < int64 ->
        _packNumberBytes uint32Byte (fromIntegral word::Word32) 4
            | int64 < -0x7fff - 1 || 0xffff < int64 ->
        _packNumberBytes int32Byte (fromIntegral word::Int32) 4
            | 0x7fff < int64 ->
        _packNumberBytes uint16Byte (fromIntegral word::Word16) 2
            | int64 < -0x7f - 1 || 0xff < int64 ->
        _packNumberBytes int16Byte (fromIntegral word::Int16) 2
            | 0x7f < int64 ->
        _packNumberBytes uint8Byte (fromIntegral word::Word8) 1
            | int64 < -0x3f - 1 || 0x3f < int64 ->
        _packNumberBytes int8Byte (fromIntegral word::Int8) 1
      _ -> _givePack $ _fromWord (fromIntegral word::Int8) 1 C.empty )

packFloat float = _packNumberBytes floatByte (floatToWord float) 4

packDouble double = _packNumberBytes doubleByte (doubleToWord double) 8

_packBinaryData byteString fixbyte byte8 byte16 byte32 = let
    len = C.length byteString
  in
  case len of
  len | len <= 0x1f ->
        _givePack $ C.cons (fixbyte .|. (fromIntegral len::Word8)) byteString
      | len <= 0xff ->
        _packPrefixedBytes byte8 (fromIntegral len::Word8) 1 byteString
      | len <= 0xffff ->
        _packPrefixedBytes byte16 (fromIntegral len::Word16) 2 byteString
      | len <= 0xffffffff ->
        _packPrefixedBytes byte32 (fromIntegral len::Word32) 4 byteString
  _ -> contAsk $ \catchers ->
      get >>= \s ->
      (stateReaderContT . tooBig catchers byteString (packState s) (destination s) $ userState s) >>= \(d, u) ->
      _putDestination d >>
      _putUserState u

_packText text = _packBinaryData (encodeUtf8 text) fixstrMask str8Byte str16Byte str32Byte

_packString string = _packText $ T.pack string

packString string = _verisitionValue $ _packString string

_packBin byteString = _packBinaryData byteString fixbinMask bin8Byte bin16Byte bin32Byte

packBin byteString = _verisitionValue $ _packBin byteString

_packNsText nsName = _packBinaryData (encodeUtf8 nsName) fixnsMask ns8Byte ns16Byte ns32Byte

_packQName nsName localName = let
    packStr = _packString localName
  in
  case nsName of
  Just name -> _packNsText (T.pack name) >> packStr
  _ -> packStr

_packClassName nsName localName =
  _packSingleByte classNameByte >>
  _packQName nsName localName

_verisitionValueStart pack = get >>= \s ->
  let state = packState s in
  case state of
  PostNilEntryValue parentState -> _packSingleByte nilByte >> case parentState of
    Dictionary _ -> _putState parentState >> pack
    Object _ ->
      _packSingleByte nilByte >> _putState (EntryValue parentState) >> pack
    _ -> _orderTheDeveloperFlogged state s
  Object parentState ->
    _packSingleByte nilByte >> _putState (EntryValue state) >> pack
  _ -> pack

packSequenceStart className = _verisitionValueStart (
    _packSingleByte sequenceByte >>
    case className of
    Just (nsName, localName) ->
      _packClassName nsName localName >> _modifyState Sequence
    _ -> _modifyState Sequence )

packDictionaryStart :: (DataDestination d e) =>
  StateT (States d u) (ReaderT (PackCatchers e d u (m r)) (ContT r m)) ()
packDictionaryStart = _verisitionValueStart (
    _packSingleByte dictionaryByte >>
    _modifyState Dictionary )

packObjectStart className = _verisitionValueStart (
  _packSingleByte objectByte >>
  case className of
  Just (nsName, localName) ->
    _packClassName nsName localName >> _modifyState Object
  _ -> _modifyState Object )

_verisitionValueEnd pack =
  pack >>
  get >>= \s ->
    let state = packState s in
    case state of
    Root -> pure ()
    Sequence _ -> pure ()
    Dictionary _ -> _putState (EntryValue state)
    EntryValue parentState -> _putState parentState
    _ -> _orderTheDeveloperFlogged state s

packCollectionEnd :: (DataDestination d e) =>
      StateT (States d u) (ReaderT (PackCatchers e d u (m r)) (ContT r m)) ()
packCollectionEnd = _verisitionValueEnd (
    _packSingleByte collectionEndByte >>
    get >>= \s -> let
      state = packState s
      flog'em = _orderTheDeveloperFlogged state s
      verisition parentState = case parentState of
        Sequence grandParentState -> _putState grandParentState
        Dictionary grandParentState -> _putState grandParentState
        Object grandParentState -> _putState grandParentState
        _ -> flog'em
    in
    case state of
      Root -> flog'em
      Sequence parentState -> _putState parentState
      Dictionary parentState -> _putState parentState
      Object parentState -> _putState parentState
      EntryValue parentState -> verisition parentState
      PostNilEntryValue parentState -> verisition parentState )

packPropertyName nsName localName =
  get >>= \s -> let
  state = packState s
  in
  case state of
  Object _ ->
    _packQName nsName localName >>
    _putState (EntryValue state)
  PostNilEntryValue parentState@(Object _) ->
    _packSingleByte nilByte >>
    _packQName nsName localName >>
    _putState (EntryValue parentState)
  EntryValue (Object _) ->
    _packSingleByte nilByte >>
    _packQName nsName localName
  _ -> contAsk $ \catchers ->
      stateReaderContT (invalidPackType catchers state (destination s) $ userState s) >>= \(d, u) ->
      _putDestination d >>
      _putUserState u

packSequence className packContents = packSequenceStart className >>
  packContents >> packCollectionEnd

packDictionary packContents = packDictionaryStart >>
  packContents >> packCollectionEnd

packObject className packContents = packObjectStart className >>
  packContents >> packCollectionEnd

packProperty nsName localName packValue =
  packPropertyName nsName localName >> packValue

packDataT d u catchers packRoot g c = let
    f (_, states) = let
        d' = destination states
        s = packState states
        u' = userState states
      in
      case s of
        Root -> c d' u'
        _ -> g s d' u' $ \pack d'' u'' -> (runContT $ (runReaderT . runStateT pack $ States s d'' u'') catchers) f
  in
  (runContT $ (runReaderT . runStateT packRoot $ States Root d u) catchers) f
