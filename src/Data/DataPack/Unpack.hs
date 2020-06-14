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
    classnameByte,
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
    Catchers(..),
    Callbacks(..),
    defaultCallbacks,
    unpackDataT,
  ) where

import Prelude hiding (take)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
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
   Sequence
   | Dictionary
   | Object
   | SequenceStart
   | ObjectStart
   | Classname
   | EntryValue
   | LocalName
   deriving (Show, Ord, Eq)

data States s = States {
    unpackStack :: [UnpackState],
    source :: s }
  deriving (Show, Ord, Eq)

type Callback s r = s -> (s -> r) -> r

-- exception handlers
data Catchers e s r = Catchers {
    stream :: e -> C.ByteString -> s -> ((C.ByteString, s) -> r) -> r,
    invalidByte :: Word8 -> [UnpackState] -> [(Word8, Word8)] -> Callback s r,
    unusedByte :: Word8 -> Callback s r,
    programmatic :: r }

data Callbacks s r = Callbacks {
    nil :: Callback s r,
    collectionEnd :: Callback s r,
    boolean :: Bool -> Callback s r,
    int :: Int -> Callback s r,
    uint32 :: Word32 -> Callback s r,
    uint64 :: Word64 -> Callback s r,
    int64 :: Int64 -> Callback s r,
    float :: Float -> Callback s r,
    double :: Double -> Callback s r,
    binStart :: Word32 -> Callback s r,
    strStart :: Word32 -> Callback s r,
    nsStart :: Word32 -> Callback s r,
    dat :: C.ByteString -> Callback s r,
    classname :: Callback s r,
    sequenceD :: Callback s r,
    dictionary :: Callback s r,
    object :: Callback s r }

callbk = \s f -> f s

callbk' = const callbk

defaultCallbacks = Callbacks {
    nil = callbk,
    collectionEnd = callbk,
    boolean = callbk',
    int = callbk',
    uint32 = callbk',
    uint64 = callbk',
    int64 = callbk',
    float = callbk',
    double = callbk',
    binStart = callbk',
    strStart = callbk',
    nsStart = callbk',
    dat = callbk',
    classname = callbk,
    sequenceD = callbk,
    dictionary = callbk,
    object = callbk }

data Env e s r = Env {
  catchers :: Catchers e s r,
  callbacks :: Callbacks s r }

modifyAnd f andf = StateT $ \s -> let s' = f s in pure (andf s', s')

stateReaderContT :: ((a -> m r1) -> m r1) -> StateT b (ReaderT r2 (ContT r1 m)) a
stateReaderContT = lift . lift . ContT

catcherAsk f = lift ask >>= f . catchers

callbackAsk f = lift ask >>= f . callbacks

putSourceAnd s a = modifyAnd (\states -> states { source = s }) (const a)

putSource s = putSourceAnd s ()

putStateStack stack = modify (\states -> states { unpackStack = stack })

getSource f = get >>= f . source

getStateStack f = get >>= f . unpackStack

callWithSource cb = getSource $ (\r -> stateReaderContT r >>= putSource) . cb

callInvalidByteErrorHandler byte allowedBytes = getStateStack $ \stack ->
    catcherAsk $ \catchers ->
    callWithSource $ invalidByte catchers byte stack allowedBytes

callUnusedByteErrorHandler byte = catcherAsk $ \catchers ->
    callWithSource $ unusedByte catchers byte

callProgrammaticErrorHandler = catcherAsk $
    stateReaderContT . const . programmatic

-- interprets byte as a signed 8-bit integer
fromByteToInt byte = fromIntegral (fromIntegral byte :: Int8)

validBytesDict = let
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
  Map.fromList [
    (Just Sequence, sequenceRanges),
    (Just SequenceStart, (classnameByte, classnameByte):sequenceRanges),
    (Just Dictionary, sequenceRanges),
    (Just Object, propNameRanges),
    (Just ObjectStart, (classnameByte, classnameByte):propNameRanges),
    (Just Classname, qnameRanges),
    (Just EntryValue, sequenceRanges),
    (Just LocalName, localnameRanges),
    (Nothing, valueRanges) ]

headSafe (x:xs) = Just x
headSafe _ = Nothing

takeUnpack n =
  getSource $ \s ->
  take n s (
      \e (dat, s') ->
        catcherAsk $ \catchers ->
        stateReaderContT (stream catchers e dat s') >>= \(dat', s'') ->
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
        validateValueState byte . callWithSource .
          int callbacks $ fromByteToInt byte
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
          _ -> callUnusedByteErrorHandler byte >> unpackT
    _ -> callProgrammaticErrorHandler

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
4. mutually recurse (unless the resulting state stack is empty)
-}
validateValueState byte procMore = let
    onward = procMore >> unpackT
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte =
        callInvalidByteErrorHandler byte validBytes >> unpackT
    in
    case stateStack of
    Sequence:_ -> onward
    Dictionary:_ -> putStateStack (EntryValue:stateStack) >> onward
    Object:_ -> callInvalidByte
    SequenceStart:stateStack' -> putStateStack (Sequence:stateStack') >> onward
    ObjectStart:_ -> callInvalidByte
    Classname:_ -> callInvalidByte
    EntryValue:stateStack' -> putStateStack stateStack' >> onward
    LocalName:_ -> callInvalidByte
    _ -> procMore
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

validateCollectionEndState byte procMore = let
    onward = procMore >> unpackT
    parent stateStack =
      case stateStack of
      Sequence:_ -> putStateStack stateStack >> onward
      Dictionary:_ -> putStateStack (EntryValue:stateStack) >> onward
      Object:_ -> callProgrammaticErrorHandler >> unpackT
      SequenceStart:_ -> callProgrammaticErrorHandler >> unpackT
      ObjectStart:_ -> callProgrammaticErrorHandler >> unpackT
      Classname:_ -> callProgrammaticErrorHandler >> unpackT
      EntryValue:stateStack' -> putStateStack stateStack' >> onward
      LocalName:_ -> callProgrammaticErrorHandler >> unpackT
      _ -> putStateStack [] >> procMore
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte =
        callInvalidByteErrorHandler byte validBytes >> unpackT
      pop stack = putStateStack stack >> onward
    in
    case stateStack of
    Sequence:stateStack' -> parent stateStack' >> onward
    Dictionary:stateStack' -> parent stateStack'
    Object:stateStack' -> parent stateStack'
    SequenceStart:stateStack' -> parent stateStack'
    ObjectStart:stateStack' -> parent stateStack'
    Classname:_ -> callInvalidByte
    EntryValue:stateStack'' -> (let
        extraCall stack = callbackAsk $ \callbacks ->
          (callWithSource $ nil callbacks) >> parent stack
      in
      case stateStack'' of
      Dictionary:stateStack' -> extraCall stateStack'
      Object:stateStack' -> extraCall stateStack'
      _ -> callProgrammaticErrorHandler >> unpackT )
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

validateStringState byte procMore = let
    onward = procMore >> unpackT
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> case stateStack of
    Sequence:_ -> onward
    Dictionary:_ -> putStateStack (EntryValue:stateStack) >> onward
    Object:_ -> putStateStack (EntryValue:stateStack) >> onward
    SequenceStart:stateStack' -> putStateStack (Sequence:stateStack') >> onward
    ObjectStart:stateStack' ->
      putStateStack (EntryValue:Object:stateStack') >> onward
    Classname:stateStack' -> putStateStack stateStack' >> onward
    EntryValue:stateStack' -> putStateStack stateStack' >> onward
    LocalName:stateStack' -> (
        case stateStack' of
        -- transition to entry value (object) or
        Object:_ -> putStateStack (EntryValue:stateStack')
        -- pop Classname (object/sequence) off the stack
        Classname:ss -> putStateStack ss
        -- flog the developer!
        _ -> callProgrammaticErrorHandler ) >> onward
    _ -> procMore
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

validateNamespaceState byte procMore = let
    onward = procMore >> unpackT
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >> unpackT
    in
    case stateStack of
    Sequence:_ -> callInvalidByte
    Dictionary:_ -> callInvalidByte
    Object:_ -> putStateStack (LocalName:stateStack) >> onward
    SequenceStart:_ -> callInvalidByte
    ObjectStart:stateStack' ->
      putStateStack (LocalName:Object:stateStack') >> onward
    Classname:_ -> putStateStack (LocalName:stateStack) >> onward
    EntryValue:_ -> callInvalidByte
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

validateClassnameState byte procMore = let
    onward = procMore >> unpackT
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >> unpackT
    in
    case stateStack of
    Sequence:_ -> callInvalidByte
    Dictionary:_ -> callInvalidByte
    Object:_ -> callInvalidByte
    SequenceStart:stack -> putStateStack (Classname:Sequence:stack) >> onward
    ObjectStart:stack -> putStateStack (Classname:Object:stack) >> onward
    Classname:_ -> callInvalidByte
    EntryValue:_ -> callInvalidByte
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

validateNilState byte procMore = let
    onward = procMore >> unpackT
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >> unpackT
    in
    case stateStack of
      Sequence:_ -> onward
      Dictionary:_ -> putStateStack (EntryValue:stateStack) >> onward
      Object:_ -> putStateStack (EntryValue:stateStack) >> onward
      SequenceStart:stateStack' -> putStateStack (Sequence:stateStack') >> onward
      ObjectStart:stack -> putStateStack (EntryValue:Object:stack) >> onward
      Classname:_ -> callInvalidByte
      EntryValue:stateStack' -> putStateStack stateStack' >> onward
      LocalName:_ -> callInvalidByte
      _ -> procMore
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

validateCollectionState unpackState byte procMore = let
    onward = procMore >> unpackT
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >>
        unpackT
    in
    case stateStack of
    Sequence:_ -> putStateStack (unpackState:stateStack) >> onward
    Dictionary:_ -> putStateStack (unpackState:stateStack) >> onward
    Object:_ -> callInvalidByte
    SequenceStart:stack -> putStateStack (unpackState:Sequence:stack) >>
      onward
    ObjectStart:stack -> callInvalidByte
    Classname:_ -> callInvalidByte
    EntryValue:stack -> putStateStack (unpackState:stateStack) >> onward
    LocalName:_ -> callInvalidByte
    _ -> putStateStack (unpackState:stateStack) >> onward
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpackT

formatDictEntry byte valid more = (
    byte, callbackAsk $ valid byte . more )

formatDict :: DataSource s e => Map.Map Word8
  (StateT (States s) (ReaderT (Env e s (m r)) (ContT r m)) ())
formatDict = Map.fromList [
    formatDictEntry nilByte validateNilState $
      \callback -> callWithSource $ nil callback,
    formatDictEntry collectionEndByte validateCollectionEndState $
      \callbacks -> callWithSource $ collectionEnd callbacks,
    formatDictEntry falseByte validateValueState $
      \callbacks -> callWithSource $ boolean callbacks False,
    formatDictEntry trueByte validateValueState $
      \callbacks -> callWithSource $ boolean callbacks True,
    formatDictEntry uint8Byte validateValueState $ \callbacks ->
      readUint8 >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry uint16Byte validateValueState $ \callbacks ->
      readUint16 >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry uint32Byte validateValueState $ \callbacks ->
      readUint32 >>= \integer ->
      callWithSource $ uint32 callbacks integer,
    formatDictEntry uint64Byte validateValueState $ \callbacks ->
      read64 >>= \integer ->
      callWithSource $ uint64 callbacks integer,
    formatDictEntry int8Byte validateValueState $ \callbacks ->
      readNum 1 (fromByteToInt::Num a => Word8 -> a) >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry int16Byte validateValueState $ \callbacks ->
      readNum 2 ((\byte ->
          fromIntegral (fromIntegral byte :: Int16))::Num a => Word16 -> a
        ) >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry int32Byte validateValueState $ \callbacks ->
      readNum 4 ((\byte ->
          fromIntegral (fromIntegral byte :: Int))::Num a => Word32 -> a
        ) >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry int64Byte validateValueState $ \callbacks ->
      read64 >>= \integer ->
      callWithSource $ int64 callbacks integer,
    formatDictEntry floatByte validateValueState $ \callbacks ->
      readNum 4 wordToFloat >>= \num ->
      callWithSource $ float callbacks num,
    formatDictEntry doubleByte validateValueState $ \callbacks ->
      readNum 8 wordToDouble >>= \num ->
      callWithSource $ double callbacks num,
    formatDictEntry bin8Byte validateStringState $ \callbacks ->
      readUint8 >>= \length ->
      (callWithSource $ binStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry bin16Byte validateStringState $ \callbacks ->
      readUint16 >>= \length ->
      (callWithSource $ binStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry bin32Byte validateStringState $ \callbacks ->
      readUint32 >>= \length ->
      (callWithSource $ binStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry str8Byte validateStringState $ \callbacks ->
      readUint8 >>= \length ->
      (callWithSource $ strStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry str16Byte validateStringState $ \callbacks ->
      readUint16 >>= \length ->
      (callWithSource $ strStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry str32Byte validateStringState $ \callbacks ->
      readUint32 >>= \length ->
      (callWithSource $ strStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry ns8Byte validateNamespaceState $ \callbacks ->
      readUint8 >>= \length ->
      (callWithSource $ nsStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry ns16Byte validateNamespaceState $ \callbacks ->
      readUint16 >>= \length ->
      (callWithSource $ nsStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry ns32Byte validateNamespaceState $ \callbacks ->
      readUint32 >>= \length ->
      (callWithSource $ nsStart callbacks length) >>
      readByteString length (callWithSource . dat callbacks),
    formatDictEntry classnameByte validateClassnameState $
      \callback -> callWithSource $ classname callback,
    formatDictEntry sequenceByte
      (validateCollectionState SequenceStart) $
      \callback -> callWithSource $ sequenceD callback,
    formatDictEntry dictionaryByte
      (validateCollectionState Dictionary) $
      \callback -> callWithSource $ dictionary callback,
    formatDictEntry objectByte
      (validateCollectionState ObjectStart) $
      \callback -> callWithSource $ object callback
 ]

fixDictEntry byteMask validator startCb = (byteMask, \byte length ->
      callbackAsk $ \callbacks ->
      validator byte $
        (callWithSource $ startCb callbacks length) >>
        readByteString length (callWithSource . dat callbacks)
    )

fixDict :: DataSource b e => Map.Map Word8 (
  Word8
  -> Word32
  -> StateT (States b) (ReaderT (Env e b (m r)) (ContT r m)) () )
fixDict = Map.fromList [
    fixDictEntry fixbinMask validateStringState binStart,
    fixDictEntry fixstrMask validateStringState strStart,
    fixDictEntry fixnsMask validateNamespaceState nsStart ]

unpackDataT s catchers callbacks = \c ->
  (runContT . runReaderT (runStateT unpackT $ States [] s) $
    Env catchers callbacks) ( \(_, states) ->
    case unpackStack states of
    [] -> c $ source states
    _ -> programmatic catchers )
