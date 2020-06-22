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
  PackState,
  Catchers(..),
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
       -> ((Word32, d) -> r) -- success handler
       -> r -- result of error or success handler

--{-
data PackState = Root
  | Sequence PackState
  | Dictionary PackState
  | Object PackState
  | ClassName PackState
  | EntryValue PackState
  | LocalName PackState
  deriving (Ord, Eq, Show)

data States d = States {
     packState :: PackState,
     destination :: d }
   deriving (Show, Ord, Eq)

type Catch d c = PackState -> d -> c

type Handle d r = Catch d ((d -> r) -> r)

-- exception handlers
data Catchers e d r = Catchers {
    stream :: e -> Word32 -> Catch d (((Word32, d) -> r) -> r),
    tooBig :: C.ByteString -> Handle d r,
    invalidPackType :: Handle d r,
    flogTheDeveloper :: Catch d r }

putDestination d = modify (\states -> states { destination = d })

putState state = modify (\states -> states { packState = state })

getState f = get >>= f . packState

callInvalidPackTypeHandler = get >>= \s ->
    contAsk $ \catchers ->
    stateReaderContT (invalidPackType catchers (packState s) $ destination s) >>= putDestination

givePack dat =
  get >>= \s ->
  give dat (destination s) (
      \e (n, d') ->
        contAsk $ \catchers ->
        stateReaderContT (stream catchers e n (packState s) d') >>= \(n', d'') ->
        putDestination d'' )
    $ \(n, d') -> putDestination d'

fromWord word n byteString =
  let n' = n - 1 in
  if n > 0
  then fromWord n' (shiftR word 8) $
    C.cons (fromIntegral $ word .&. 0xff) byteString
  else byteString

-- verify + transition = verisition
verisitionValue pack = getState $ \packState -> case packState of
  Sequence _ -> pack
  Dictionary _ -> pack >> putState (EntryValue packState)
  EntryValue parentState -> pack >> putState parentState
  Root -> pack
  _ -> callInvalidPackTypeHandler

packSingleByte byte = givePack . C.cons byte $ C.empty

packNil :: (DataDestination d e) =>
  StateT (States d) (ReaderT (Catchers e d (m r)) (ContT r m)) ()
packNil = let
    pack = verisitionValue $ packSingleByte nilByte
  in
  getState $ \packState -> case packState of
    Sequence _ -> pack
    Dictionary _ -> pack >> putState (EntryValue packState)
    Object _ -> pack >> putState (EntryValue packState)
    EntryValue parentState -> pack >> putState parentState
    Root -> pack
    _ -> callInvalidPackTypeHandler

packFalse :: (DataDestination d e) =>
  StateT (States d) (ReaderT (Catchers e d (m r)) (ContT r m)) ()
packFalse = verisitionValue $ packSingleByte nilByte

packTrue :: (DataDestination d e) =>
  StateT (States d) (ReaderT (Catchers e d (m r)) (ContT r m)) ()
packTrue = verisitionValue $ packSingleByte nilByte

packPrefixedBytes byte word numBytes byteString =
    givePack . C.cons byte $ fromWord word numBytes byteString

packNumberBytes byte word numBytes =
  packPrefixedBytes byte word numBytes C.empty

packInt :: (Num b, Bits b, Ord b, Integral b, DataDestination d e) =>
  b -> StateT (States d) (ReaderT (Catchers e d (m r)) (ContT r m)) ()
packInt word = let
    int64 = fromIntegral word::Int64
  in
  verisitionValue (
    case word of
    word | word >= 0 && (fromIntegral word) > (0x7fffffffffffffff::Word64) ->
      packNumberBytes uint64Byte (fromIntegral word::Word64) 8
    _ -> case int64 of
      int64 | int64 < -0x7fffffff - 1 ->
        packNumberBytes int64Byte int64 8
            | int64 > 0x7fffffff ->
        packNumberBytes uint32Byte (fromIntegral word::Word32) 4
            | int64 < -0x7fff - 1 ->
        packNumberBytes int32Byte (fromIntegral word::Int32) 4
            | int64 > 0x7fff ->
        packNumberBytes uint16Byte (fromIntegral word::Word16) 2
            | int64 < -0x7f - 1 ->
        packNumberBytes int16Byte (fromIntegral word::Int16) 2
            | int64 > 0x7f ->
        packNumberBytes uint8Byte (fromIntegral word::Word8) 1
            | int64 > 0x3f || int64 < -0x3f - 1 ->
        packNumberBytes int8Byte (fromIntegral word::Int8) 1
      _ -> givePack $ fromWord (fromIntegral word::Int8) 1 C.empty )
--}

packFloat float = packNumberBytes floatByte (floatToWord float) 4

packDouble double = packNumberBytes doubleByte (doubleToWord double) 8

packData byteString byte8 byte16 byte32 = let
    len = C.length byteString
  in
  case len of
  len | len <= 0xff ->
        packPrefixedBytes byte8 (fromIntegral len::Word8) 1 byteString
      | len <= 0xffff ->
        packPrefixedBytes byte16 (fromIntegral len::Word16) 2 byteString
      | len <= 0xffffffff ->
        packPrefixedBytes byte32 (fromIntegral len::Word32) 4 byteString
  _ -> contAsk $ \catchers ->
      get >>= \s -> (stateReaderContT . tooBig catchers byteString (packState s) $ destination s) >>= putDestination

packText text = packData (encodeUtf8 text) str8Byte str16Byte str32Byte

packString string = packText $ T.pack string

packStringValue string = verisitionValue $ packString string

packBin byteString = packData byteString bin8Byte bin16Byte bin32Byte

packBinValue byteString = verisitionValue $ packBin byteString

packNsText nsName = packData (encodeUtf8 nsName) ns8Byte ns16Byte ns32Byte

packNs nsName = packNsText $ T.pack nsName

packCollectionEnd :: (DataDestination d e) =>
  StateT (States d) (ReaderT (Catchers e d (m r)) (ContT r m)) ()
packCollectionEnd = getState $ \packState -> let
    packEnd = packSingleByte sequenceByte
  in
  case packState of
    Sequence _ -> packEnd
    Dictionary _ -> packEnd
    Object _ -> packEnd
    EntryValue _ -> packEnd
    _ -> callInvalidPackTypeHandler

packQName (nsName, localName) = let
    packStr = packString localName
  in
  case nsName of
  Just name -> packNs name >> packStr
  _ -> packStr

packClassName qname =
  packSingleByte classNameByte >>
  packQName qname

transitionToCollection state packContents = verisitionValue (
  getState $ \packState ->
    (putState $ state packState) >>
    packContents >>
    packCollectionEnd >>
    putState packState )

packSequence className packContents = let
    transition = transitionToCollection Sequence packContents
  in
  packSingleByte sequenceByte >>
  case className of
  Just qname -> packClassName qname >> transition
  _ -> transition

packDictionary packContents = packSingleByte dictionaryByte >>
  transitionToCollection Dictionary packContents

packObject className packContents = let
    transition = transitionToCollection Object packContents
  in
  packSingleByte objectByte >>
  case className of
  Just qname -> packClassName qname >> transition
  _ -> transition

packProperty qname packValue =
  getState ( \packState ->
    case packState of
    Object _ ->
      (case qname of
        Just name -> packQName name
        _ -> packNil) >>
      (putState $ EntryValue packState) >>
      (case packValue of
        Just pack -> pack
        _ -> packNil) >>
      putState packState
    _ -> callInvalidPackTypeHandler )

packDataT d catchers packRoot = \c ->
  (runContT $ (runReaderT . runStateT packRoot $ States Root d) catchers) (
    \(_, states) ->
      case packState states of
        Root -> c $ destination states
        _ -> flogTheDeveloper catchers (packState states) $ destination states
  )
