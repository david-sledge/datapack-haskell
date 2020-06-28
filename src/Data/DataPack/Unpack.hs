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
    take,

    DataSource,
    UnpackState(..),
    UnpackCatchers(..),
    Callback,
    Callbacks(..),
    States(..),
    defaultCallbacks,
    unpackDataT,
  ) where

import Prelude hiding (take)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString.Lazy as C
import Data.Int
import qualified Data.Map.Lazy as Map
import Data.Word
import Data.DataPack

class DataSource s e | s -> e where
    take ::
      Word32
       -> s -- stream value
       -> (e -> (C.ByteString, s) -> r) -- error handler
       -> ((C.ByteString, s) -> r) -- success handler
       -> r -- result of error or success handler

data UnpackState =
  Root
  | Sequence UnpackState
  | Dictionary UnpackState
  | Object UnpackState
  | SequenceStart UnpackState
  | ObjectStart UnpackState
  | ClassName UnpackState
  | EntryValue UnpackState
  | LocalName UnpackState
  deriving (Show, Ord, Eq)

data States s = States {
    unpackState :: UnpackState,
    source :: s }
  deriving (Show, Ord, Eq)

type Handle s c = UnpackState -> s -> c

type Callback s r = Handle s ((s -> r) -> r)

-- exception handlers
data UnpackCatchers e s r = UnpackCatchers {
    stream :: e -> C.ByteString -> Handle s (((C.ByteString, s) -> r) -> r),
    invalidByte :: Word8 -> [(Word8, Word8)] -> Callback s r,
    unusedByte :: Word8 -> Callback s r,
    flogTheDeveloper :: Handle s r }

data Callbacks s r = Callbacks {
    nil :: Callback s r,
    collectionEnd :: Callback s r,
    boolean :: Bool -> Callback s r,
    uint64 :: Word64 -> Callback s r,
    int64 :: Int64 -> Callback s r,
    float :: Float -> Callback s r,
    double :: Double -> Callback s r,
    binStart :: Word32 -> Callback s r,
    strStart :: Word32 -> Callback s r,
    nsStart :: Word32 -> Callback s r,
    dat :: C.ByteString -> Callback s r,
    className :: Callback s r,
    sequenceD :: Callback s r,
    dictionary :: Callback s r,
    object :: Callback s r }

callbk _ s f = f s

callbk' = const callbk

defaultCallbacks = Callbacks {
    nil = callbk,
    collectionEnd = callbk,
    boolean = callbk',
    uint64 = callbk',
    int64 = callbk',
    float = callbk',
    double = callbk',
    binStart = callbk',
    strStart = callbk',
    nsStart = callbk',
    dat = callbk',
    className = callbk,
    sequenceD = callbk,
    dictionary = callbk,
    object = callbk }

data Env e s r = Env {
  catchers :: UnpackCatchers e s r,
  callbacks :: Callbacks s r }

catcherAsk f = contAsk $ f . catchers

callbackAsk f = contAsk $ f . callbacks

putSourceAnd s a = modifyAnd (\states -> states { source = s }) (const a)

putState state = modify (\states -> states { unpackState = state })

makeCallback cb = get >>= \s ->
  stateReaderContT (cb (unpackState s) $ source s) >>= \s' ->
  putSourceAnd s' ()

callInvalidByteErrorHandler byte allowedBytes =
    catcherAsk $ \catchers ->
      makeCallback (invalidByte catchers byte allowedBytes) >> unpackT

orderTheDeveloperFlogged = catcherAsk $ \catchers ->
    get >>= \s ->
    stateReaderContT . const . flogTheDeveloper catchers (unpackState s) $ source s

-- interprets byte as a signed 8-bit integer
fromByteToInt byte = fromIntegral (fromIntegral byte :: Int8)

validStateBytes state = let
    valueRanges = [
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
  case state of
  Sequence _ -> sequenceRanges
  SequenceStart _ -> (classNameByte, classNameByte):sequenceRanges
  Dictionary _ -> sequenceRanges
  Object _ -> propNameRanges
  ObjectStart _ -> (classNameByte, classNameByte):propNameRanges
  ClassName _ -> qnameRanges
  EntryValue _ -> sequenceRanges
  LocalName _ -> localnameRanges
  Root -> valueRanges

takeUnpack n =
  get >>= \s ->
  take n (source s) (
      \e (dat, s') ->
        catcherAsk $ \catchers ->
        stateReaderContT (stream catchers e dat (unpackState s) s') >>= \(dat', s'') ->
        putSourceAnd s'' dat' )
    $ \(dat, s') -> putSourceAnd s' dat

unpackT
  :: DataSource s e
     => StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) ()
unpackT =
  takeUnpack 1 >>= \dat ->
    case C.uncons dat of
    Just (byte, _) ->
      let value = byte .&. fixintMask in
      -- is fixint?
      if value == 0 || value == fixintMask
      -- yes:
      then
        callbackAsk $ \callbacks ->
        -- validate the byte against current state, read any additional data,
        -- and call corresponding callback after validation.
        validateValueState byte . makeCallback .
          int64 callbacks $ fromByteToInt byte
      else
        let mask = byte .&. fixMask in
        -- in fixDict?
        case Map.lookup mask fixDict of
        -- yes: call mapped function with length argument
        Just f -> f byte . fromIntegral $ byte .&. lenMask
        _ ->
        -- in formatDict?
          case Map.lookup byte formatDict of
          -- call mapped function
          Just g -> g
          -- unused byte
          _ -> catcherAsk (\catchers ->
              makeCallback $ unusedByte catchers byte) >> unpackT
    _ -> orderTheDeveloperFlogged

readByteString n c =
  takeUnpack n >>= \byteString ->
  c byteString >>
  let len = fromIntegral $ C.length byteString in
  if len < n
  then readByteString (n - len) c
  else pure ()

toWord n byteString =
  let n' = n - 1 in
  if n > 0
  then
    case C.uncons byteString of
    Just (byte, byteString') ->
      shiftL (fromIntegral byte) (fromIntegral $ n' * 8) .|.
        toWord n' byteString'
    _ -> 0
  else 0

readBytesToWord
  :: (Num b, Bits b, DataSource t e) =>
    Word32
    -> StateT (States t) (ReaderT (Env e t (m r1)) (ContT r1 m)) b
readBytesToWord n =
  takeUnpack n >>= \byteString ->
  let
    len = fromIntegral $ C.length byteString
    word = toWord n byteString
  in
  if len < n
  then (word .|.) <$> readBytesToWord (n - len)
  else pure word

readNum n f = f <$> readBytesToWord n

read64 :: (Num b, DataSource s e) =>
  StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) b
read64 = readNum 8 (fromIntegral::Num a => Word64 -> a)

readUint8 :: (Num b, DataSource s e) =>
  StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) b
readUint8 = readNum 1 (fromIntegral::Num a => Word8 -> a)

readUint16 :: (Num b, DataSource s e) =>
  StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) b
readUint16 = readNum 2 (fromIntegral::Num a => Word16 -> a)

readUint32 :: (Num b, DataSource s e) =>
  StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) b
readUint32 = readNum 4 (fromIntegral::Num a => Word32 -> a)

{-
More than just validation:
1. validate against current state
2. transition state (sometimes)
3. additional processing which includes
  a. reading additional data (sometimes)
  b. making callbacks (every time)
4. mutually recurse (unless the resulting state is Root)
-}
validateValueState byte procMore = let
    onward = procMore >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
    Root -> procMore
    Sequence _ -> onward
    Dictionary _ -> putState (EntryValue state) >> onward
    SequenceStart state' -> putState (Sequence state') >> onward
    EntryValue state' -> putState state' >> onward
    _ -> callInvalidByteErrorHandler byte (validStateBytes state)

validateCollectionEndState byte procMore = let
    parent state =
      case state of
      Root -> putState state
      Sequence _ -> putState state >> unpackT
      Dictionary _ -> putState (EntryValue state) >> unpackT
      EntryValue state' -> putState state' >> unpackT
      _ -> orderTheDeveloperFlogged >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
    Sequence state' -> procMore >> parent state'
    Dictionary state' -> procMore >> parent state'
    Object state' -> procMore >> parent state'
    SequenceStart state' -> procMore >> parent state'
    ObjectStart state' -> procMore >> parent state'
    EntryValue state'' -> let
        extraCall state = callbackAsk $ \callbacks ->
          makeCallback (nil callbacks) >> procMore >> parent state
      in
      case state'' of
      Dictionary state' -> extraCall state'
      Object state' -> extraCall state'
      -- flog the developer!
      _ -> orderTheDeveloperFlogged >> unpackT
    _ -> callInvalidByteErrorHandler byte (validStateBytes state)

validateStringState byte procMore = let
    onward = procMore >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
    Sequence _ -> onward
    Dictionary _ -> putState (EntryValue state) >> onward
    Object _ -> putState (EntryValue state) >> onward
    SequenceStart state' -> putState (Sequence state') >> onward
    ObjectStart state' ->
      putState (EntryValue $ Object state') >> onward
    ClassName state' -> putState state' >> onward
    EntryValue state' -> putState state' >> onward
    LocalName state' -> (
        case state' of
        -- transition to entry value (object) or
        Object _ -> putState (EntryValue state')
        -- pop ClassName (object/sequence) off the state
        ClassName ss -> putState ss
        -- flog the developer!
        _ -> orderTheDeveloperFlogged ) >> onward
    _ -> procMore

validateNamespaceState byte procMore = let
    onward = procMore >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
    Object _ -> putState (LocalName state) >> onward
    ObjectStart state' ->
      putState (LocalName $ Object state') >> onward
    ClassName _ -> putState (LocalName state) >> onward
    _ -> callInvalidByteErrorHandler byte (validStateBytes state)

validateClassNameState byte procMore = let
    onward = procMore >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
    SequenceStart state -> putState (ClassName $ Sequence state) >> onward
    ObjectStart state -> putState (ClassName $ Object state) >> onward
    _ -> callInvalidByteErrorHandler byte (validStateBytes state)

validateNilState byte procMore = let
    onward = procMore >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
      Root -> procMore
      Sequence _ -> onward
      Dictionary _ -> putState (EntryValue state) >> onward
      Object _ -> putState (EntryValue state) >> onward
      SequenceStart state' -> putState (Sequence state') >> onward
      ObjectStart state -> putState (EntryValue $ Object state) >> onward
      EntryValue state' -> putState state' >> onward
      _ -> callInvalidByteErrorHandler byte (validStateBytes state)

validateCollectionState nextState byte procMore = let
    onward = procMore >> unpackT
  in
  get >>= \s ->
    let state = unpackState s in
    case state of
    Root -> putState (nextState state) >> onward
    Sequence _ -> putState (nextState state) >> onward
    Dictionary _ -> putState (nextState state) >> onward
    SequenceStart state -> putState (nextState $ Sequence state) >> onward
    EntryValue state -> putState (nextState state) >> onward
    _ -> callInvalidByteErrorHandler byte (validStateBytes state)

formatDictEntry byte valid more = (
    byte, callbackAsk $ valid byte . more )

formatDict :: DataSource s e => Map.Map Word8
  (StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) ())
formatDict = Map.fromList [
    formatDictEntry nilByte validateNilState $
      \callbacks -> makeCallback $ nil callbacks,
    formatDictEntry collectionEndByte validateCollectionEndState $
      \callbacks -> makeCallback $ collectionEnd callbacks,
    formatDictEntry falseByte validateValueState $
      \callbacks -> makeCallback $ boolean callbacks False,
    formatDictEntry trueByte validateValueState $
      \callbacks -> makeCallback $ boolean callbacks True,
    formatDictEntry uint8Byte validateValueState $ \callbacks ->
      readUint8 >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry uint16Byte validateValueState $ \callbacks ->
      readUint16 >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry uint32Byte validateValueState $ \callbacks ->
      readUint32 >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry uint64Byte validateValueState $ \callbacks ->
      read64 >>= \integer ->
      makeCallback $ uint64 callbacks integer,
    formatDictEntry int8Byte validateValueState $ \callbacks ->
      readNum 1 (fromByteToInt::Num a => Word8 -> a) >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry int16Byte validateValueState $ \callbacks ->
      readNum 2 ((\byte ->
          fromIntegral (fromIntegral byte :: Int16))::Num a => Word16 -> a
        ) >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry int32Byte validateValueState $ \callbacks ->
      readNum 4 ((\byte ->
          fromIntegral (fromIntegral byte :: Int32))::Num a => Word32 -> a
        ) >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry int64Byte validateValueState $ \callbacks ->
      read64 >>= \integer ->
      makeCallback $ int64 callbacks integer,
    formatDictEntry floatByte validateValueState $ \callbacks ->
      readNum 4 wordToFloat >>= \num ->
      makeCallback $ float callbacks num,
    formatDictEntry doubleByte validateValueState $ \callbacks ->
      readNum 8 wordToDouble >>= \num ->
      makeCallback $ double callbacks num,
    formatDictEntry bin8Byte validateStringState $ \callbacks ->
      readUint8 >>= \length ->
      makeCallback (binStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry bin16Byte validateStringState $ \callbacks ->
      readUint16 >>= \length ->
      makeCallback (binStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry bin32Byte validateStringState $ \callbacks ->
      readUint32 >>= \length ->
      makeCallback (binStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry str8Byte validateStringState $ \callbacks ->
      readUint8 >>= \length ->
      makeCallback (strStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry str16Byte validateStringState $ \callbacks ->
      readUint16 >>= \length ->
      makeCallback (strStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry str32Byte validateStringState $ \callbacks ->
      readUint32 >>= \length ->
      makeCallback (strStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry ns8Byte validateNamespaceState $ \callbacks ->
      readUint8 >>= \length ->
      makeCallback (nsStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry ns16Byte validateNamespaceState $ \callbacks ->
      readUint16 >>= \length ->
      makeCallback (nsStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry ns32Byte validateNamespaceState $ \callbacks ->
      readUint32 >>= \length ->
      makeCallback (nsStart callbacks length) >>
      readByteString length (makeCallback . dat callbacks),
    formatDictEntry classNameByte validateClassNameState $
      \callback -> makeCallback $ className callback,
    formatDictEntry sequenceByte
      (validateCollectionState SequenceStart) $
      \callback -> makeCallback $ sequenceD callback,
    formatDictEntry dictionaryByte
      (validateCollectionState Dictionary) $
      \callback -> makeCallback $ dictionary callback,
    formatDictEntry objectByte
      (validateCollectionState ObjectStart) $
      \callback -> makeCallback $ object callback
 ]

fixDictEntry byteMask validator startCb = (byteMask, \byte length ->
      callbackAsk $ \callbacks ->
      validator byte $
        makeCallback (startCb callbacks length) >>
        readByteString length (makeCallback . dat callbacks)
    )

fixDict :: DataSource b e => Map.Map Word8 (
  Word8
  -> Word32
  -> StateT (States b) (ReaderT (Env e b (m r)) (ContT r m)) () )
fixDict = Map.fromList [
    fixDictEntry fixbinMask validateStringState binStart,
    fixDictEntry fixstrMask validateStringState strStart,
    fixDictEntry fixnsMask validateNamespaceState nsStart ]

unpackDataT s catchers callbacks c =
  (runContT . runReaderT (runStateT unpackT $ States Root s) $
    Env catchers callbacks) ( \(_, states) ->
    case unpackState states of
    Root -> c $ source states
    _ -> flogTheDeveloper catchers (unpackState states) $ source states )
