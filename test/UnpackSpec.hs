{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TupleSections,
    OverloadedStrings, Rank2Types #-}

module UnpackSpec where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Function (on)
import Data.Word
import Data.String
import Data.Int
import Data.Text.Lazy.Encoding
import Numeric (showHex)
import System.Exit (exitFailure)

import Data.DataPack
import Data.DataPack.Unpack
import Data.DataPack.Pack

import qualified Data.ByteString.Lazy as C
import qualified Data.Text.Lazy as T

import qualified Data.DataPack.Unpack as U (stream, flogTheDeveloper, UnpackState(..))
import qualified Data.DataPack.Pack as P (stream, flogTheDeveloper, PackState(..), States)

import Test

data ByteStringPos = ByteStringPos
  { byteString :: C.ByteString
  , pos :: Int64
  }
  deriving Show

instance DataSource ByteStringPos () where
  take n bstr bad good = let
      string = byteString bstr
    in
    if C.length string == 0
    then bad () (string, bstr)
    else
      let
        n' = fromIntegral n
        (body, tail) = C.splitAt n' string
      in
      good (body, ByteStringPos tail $ pos bstr + n')

type EnvState a m s t = StateT a m (s, MaybeCont t String)

data CatchCallStacks e s st t m = CatchCallStacks
  { catchStack :: [UnpackCatchers e s (EnvState (CatchCallStacks e s st t m) m s t)]
  , callStack :: [Callbacks s (EnvState (CatchCallStacks e s st t m) m s t)]
  }

createDefaultCallbacks
  :: IsString t =>
     ((Callbacks s1 r1 -> Callback s1 r1) -> t -> Callback s2 r2)
     -> Callbacks s2 r2
createDefaultCallbacks f =
  let g h a = f (`h` a)
  in
  Callbacks
    { nil = f nil "nil"
    , collectionEnd = f collectionEnd "collectionEnd"
    , boolean = \a -> g boolean a "boolean"
    , uint64 = \a -> g uint64 a "uint64"
    , int64 = \a -> g int64 a "int64"
    , float = \a -> g float a "float"
    , double = \a -> g double a "double"
    , binStart = \a -> g binStart a "binStart"
    , strStart = \a -> g strStart a "strStart"
    , nsStart = \a -> g nsStart a "nsStart"
    , dat = \a -> g dat a "dat"
    , className = f className "className"
    , sequenceD = f sequenceD "sequenceD"
    , dictionary = f dictionary "dictionary"
    , object = f object "object"
    }

unpackFailMessage :: (IsString t, Show a, Show b) =>
     b -> a -> String -> MaybeCont t String
unpackFailMessage u pos msg = Just $ \f -> f "Failed"
  ++ ": after reading " ++ show pos ++ " bytes with state of "
  ++ show u ++ "\x2014" ++ msg

type TestFailMessage f t = f (ByteStringPos, MaybeCont t String)

unpackSourceFailMessage :: (Applicative f, IsString t, Show a) =>
       a
       -> ByteStringPos
       -> String
       -> TestFailMessage f t
unpackSourceFailMessage u s msg = pure (s, unpackFailMessage u (pos s) msg)

failCallbacks :: (Applicative f, IsString t) =>
     String
     -> Callbacks
          ByteStringPos (TestFailMessage f t)
failCallbacks expectedStr = createDefaultCallbacks . const $ \str u s _ ->
    unpackSourceFailMessage u s ("Expected " ++ expectedStr ++ ", found " ++ str)

passCallbacks :: Callbacks s r
passCallbacks = createDefaultCallbacks . const . const $ \u s f -> f s

type UnderCallback e st m = EnvState
   (CatchCallStacks e ByteStringPos st String m)
   m
   ByteStringPos
   String

passThruCallbacks :: (Monad m) => Callbacks
       ByteStringPos
       (UnderCallback e st m)
passThruCallbacks = createDefaultCallbacks $ \c str u s f ->
  get >>= \ccs ->
    case callStack ccs of
      cs:css -> put (ccs { callStack = css }) >> c cs u s f
      _ -> unpackSourceFailMessage u s $ "unexpected " ++ str

failUnpackCatchers :: (Monad m) => UnpackCatchers () ByteStringPos
  (UnderCallback () st m)
failUnpackCatchers = UnpackCatchers {
  U.stream = \e _ u s _ -> unpackSourceFailMessage u s $ "stream fail " ++ show e,
  invalidByte = \byte allowedByteRanges u s f ->
    unpackSourceFailMessage u s $ "fail " ++ show byte ++
      " is not allowed with given state. Expected values within the ranges of "
      ++ show allowedByteRanges ++ ".",
  unusedByte = \byte u s f ->
    unpackSourceFailMessage u s $ "fail " ++ show byte ++ " is not a usable byte.",
  U.flogTheDeveloper = \u s ->
    unpackSourceFailMessage u s "Superfail! Flog the developer!" }

passThruUnpackCatchers :: (Monad m) => UnpackCatchers () ByteStringPos
  (UnderCallback () st m)
passThruUnpackCatchers = let
    feedUnpackCatchers f = get >>= \ccs ->
      case catchStack ccs of
        cs:css -> put (ccs { catchStack = css }) >> f cs
        _ -> f failUnpackCatchers in
  UnpackCatchers {
      U.stream = \e bStr u s f -> feedUnpackCatchers $ \cs -> U.stream cs e bStr u s f,
      invalidByte = \byte allowedByteRanges u s f -> feedUnpackCatchers $ \cs ->
          invalidByte cs byte allowedByteRanges u s f,
      unusedByte = \byte u s f -> feedUnpackCatchers $ \cs ->
          unusedByte cs byte u s f,
      U.flogTheDeveloper = U.flogTheDeveloper failUnpackCatchers }

unpackTestCase :: Monad m =>
     a
     -> C.ByteString
     -> [UnpackCatchers
           ()
           ByteStringPos
           (UnderCallback () st m)]
     -> [Callbacks
           ByteStringPos
           (UnderCallback () st m)]
     -> Int64
     -> m (a, MaybeCont String String)
unpackTestCase testName bytString catcherStack callbackStack expectedBytesRemaining =
  runStateT (
    unpackDataT (ByteStringPos bytString 0) passThruUnpackCatchers passThruCallbacks $
      \s ->
        let len = C.length $ byteString s in
        pure (s, if len == expectedBytesRemaining
          then Nothing
          else unpackFailMessage U.Root (pos s) $ "Expected "
            ++ show expectedBytesRemaining
            ++ " byte(s) remaining in byteString. Found "
            ++ show len ++ " byte(s)."))
    (CatchCallStacks catcherStack callbackStack) >>= \((s, passFail), catchCallStacks) ->
      pure (testName, case passFail of
        Just _ -> passFail
        _ -> let cs = catchStack catchCallStacks in
          case cs of
            _:_ -> unpackFailMessage U.Root (pos s) $ "Expected " ++ show (length cs) ++ " more exception(s) for negative testing"
            _ -> let cs' = callStack catchCallStacks in
              case cs' of
                _:_ -> unpackFailMessage U.Root (pos s) $ "Expected " ++ show (length cs') ++ " callback(s) in the root value."
                _ -> passFail)

assert :: (Eq a1, Applicative f, IsString t, Show a2, Show a1) =>
     a1
     -> a1
     -> a2
     -> ByteStringPos
     -> (ByteStringPos -> TestFailMessage f t)
     -> TestFailMessage f t
assert expect actual u s f =
  if actual == expect
  then f s
  else unpackSourceFailMessage u s $ "Expected " ++ show expect
      ++ ". Found " ++ show actual

expectNil :: (Applicative f) => Callbacks
                       ByteStringPos
                       (f (ByteStringPos, Maybe (([Char] -> [Char]) -> [Char])))
expectNil = (failCallbacks "nil") {nil = nil passCallbacks}

expectBool b = (failCallbacks (show b)) {boolean = assert b}

expectInt n = (failCallbacks "int") {int64 = assert n}

expectUint n = (failCallbacks "uint") {uint64 = assert n}

expectFloat n = (failCallbacks "float") {float = assert n}

expectDouble n = (failCallbacks "double") {double = assert n}

expectBin n = (failCallbacks "binStart") {binStart = assert n}

expectStr n = (failCallbacks "strStart") {strStart = assert n}

expectNs n = (failCallbacks "nsStart") {nsStart = assert n}

expectDat n = (failCallbacks "dat") {dat = assert n}

expectSequence :: (Applicative f0) => Callbacks
                            ByteStringPos
                            (f0 (ByteStringPos, Maybe (([Char] -> [Char]) -> [Char])))
expectSequence = (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks}

expectDictionary :: (Applicative f0) => Callbacks
                              ByteStringPos
                              (f0 (ByteStringPos, Maybe (([Char] -> [Char]) -> [Char])))
expectDictionary = (failCallbacks "dictionary") {dictionary = dictionary passCallbacks}

expectObject :: (Applicative f0) => Callbacks
                              ByteStringPos
                              (f0 (ByteStringPos, Maybe (([Char] -> [Char]) -> [Char])))
expectObject = (failCallbacks "object") {object = object passCallbacks}

expectClassName :: (Applicative f0) => Callbacks
                              ByteStringPos
                              (f0 (ByteStringPos, Maybe (([Char] -> [Char]) -> [Char])))
expectClassName = (failCallbacks "className") {className = className passCallbacks}

expectCollectionEnd :: (Applicative f0) => Callbacks
                              ByteStringPos
                              (f0 (ByteStringPos, Maybe (([Char] -> [Char]) -> [Char])))
expectCollectionEnd = (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}

unpackTests :: Monad m0 => [m0 (String, MaybeCont String String)]
unpackTests =
  let str2Bin = encodeUtf8 . T.pack
      fixstr = "fixstr"
      fixbin = str2Bin fixstr
      fixlen = C.length fixbin
  in
  [ unpackTestCase "nil" (C.pack [nilByte]) [] [expectNil] 0
  , unpackTestCase "empty content (negative test)" (C.pack [])
      [failUnpackCatchers {U.stream = \_ _ _ s _ -> pure (s, Nothing)}] [] 0
  , unpackTestCase "single nil with more data" (C.pack [nilByte, dictionaryByte])
      [] [expectNil] 1
  , unpackTestCase "unused (negative test)" (C.pack [objectByte + 1, nilByte])
      [failUnpackCatchers {unusedByte = \_ _ s f -> f s}] [expectNil] 0
  , unpackTestCase "true" (C.pack [trueByte]) []
      [expectBool True] 0
  , unpackTestCase "false" (C.pack [falseByte, dictionaryByte]) []
      [expectBool False] 1
  , unpackTestCase "collection end without matching start (negative test)"
      (C.pack [collectionEndByte])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}] [] 0
  , unpackTestCase "fixint 63" (C.pack [63]) [] [expectInt 63] 0
  , unpackTestCase "fixint -64" (C.pack [fixintMask]) [] [expectInt (-64)] 0
  , unpackTestCase "uint8 without data (negative test)" (C.pack [uint8Byte])
      [failUnpackCatchers {U.stream = \_ _ _ s _ -> pure (s, Nothing)}] [] 0
  , unpackTestCase "uint8" (C.pack [uint8Byte, 0xff]) [] [expectInt 255] 0
  , unpackTestCase "uint16" (C.pack [uint16Byte, 0xff, 0xff]) []
      [expectInt 65535] 0
  , unpackTestCase "uint32" (C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff]) []
      [expectInt 4294967295] 0
  , unpackTestCase "uint64"
      (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) []
      [expectUint 18446744073709551615] 0
  , unpackTestCase "int8" (C.pack [int8Byte, 0xff]) [] [expectInt (-1)] 0
  , unpackTestCase "int16" (C.pack [int16Byte, 0xff, 0xff]) []
      [expectInt (-1)] 0
  , unpackTestCase "int32" (C.pack [int32Byte, 0xff, 0xff, 0xff, 0xff]) []
      [expectInt (-1)] 0
  , unpackTestCase "int64"
      (C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) []
      [expectInt (-1)] 0
  , unpackTestCase "float" (C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]) []
      [expectFloat 2.625] 0
  , unpackTestCase "double"
      (C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [expectDouble (-2.625)] 0
  , unpackTestCase "fixbin"
      (C.pack
        [fixbinMask .|. 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      [] [expectBin 1, expectDat (C.pack [0x05])] 7
  , unpackTestCase "bin8"
      (C.pack [bin8Byte, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [expectBin 1, expectDat (C.pack [0x05])] 6
  , unpackTestCase "bin16"
      (C.pack [bin16Byte, 0, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [expectBin 1, expectDat (C.pack [0x05])] 5
  , unpackTestCase "bin32"
      (C.pack [bin32Byte, 0x00, 0x00, 0, 0x1, 0x05, 0x00, 0x00, 0x00]) []
      [expectBin 1, expectDat (C.pack [0x05])] 3
  , unpackTestCase fixstr
      (C.concat [C.pack [fixstrMask .|. fromIntegral fixlen], fixbin]) []
      [expectStr $ fromIntegral fixlen, expectDat fixbin] 0
  , let bStr = str2Bin "str8"
        len = C.length bStr
    in
    unpackTestCase "str8"
      (C.concat [
        C.pack [str8Byte, fromIntegral len], bStr,
        C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      ]) [] [expectStr $ fromIntegral len, expectDat bStr] 6
  , let bStr = str2Bin "str16"
        len = C.length bStr
    in
    unpackTestCase "str16"
      (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr]) []
      [expectStr $ fromIntegral len, expectDat bStr] 0
  , let bStr = str2Bin "str32"
        len = C.length bStr
    in
    unpackTestCase "str32"
      (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr]) []
      [expectStr $ fromIntegral len, expectDat bStr] 0
  , unpackTestCase "out of context namespace name (negative test)"
      (C.pack [fixnsMask])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}] [] 0
  , unpackTestCase "class name outside of sequences and objects (negative test)"
      (C.pack [classNameByte])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}] [] 0
  , unpackTestCase "empty sequence" (C.pack [sequenceByte, collectionEndByte])
      [] [expectSequence, expectCollectionEnd] 0
  , unpackTestCase "empty sequence with local class name"
      ( C.concat
          [ C.pack
              [sequenceByte, classNameByte, fixstrMask .|. fromIntegral fixlen]
          , fixbin
          , C.pack [collectionEndByte]
          ]
      ) []
      [ expectSequence
      , expectClassName
      , expectStr $ fromIntegral fixlen
      , expectDat fixbin
      , expectCollectionEnd
      ] 0
  , unpackTestCase "empty sequence with namespaced class name"
      ( C.concat
          [ C.pack
              [sequenceByte, classNameByte, fixnsMask .|. fromIntegral fixlen]
          , fixbin
          , C.pack [fixstrMask .|. fromIntegral fixlen]
          , fixbin
          , C.pack [collectionEndByte]
          ]
      ) []
      [ expectSequence
      , expectClassName
      , expectNs $ fromIntegral fixlen
      , expectDat fixbin
      , expectStr $ fromIntegral fixlen
      , expectDat fixbin
      , expectCollectionEnd
      ] 0
  , unpackTestCase "sequence" (
        C.concat [
          C.pack [ sequenceByte, nilByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ] ) []
      [
        expectSequence,
        expectNil,
        expectBool True,
        expectBool False,
        expectStr $ fromIntegral fixlen,
        expectDat fixbin,
        expectCollectionEnd
      ] 0,
      unpackTestCase "empty dictionary" (
          C.pack [ dictionaryByte, collectionEndByte ] ) []
        [
          expectDictionary,
          expectCollectionEnd
        ] 0,
      unpackTestCase "dictionary" (
        C.concat [
          C.pack [ dictionaryByte, nilByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ] ) []
        [
          expectDictionary,
          expectNil,
          expectBool True,
          expectBool False,
          expectStr $ fromIntegral fixlen,
          expectDat fixbin,
          expectCollectionEnd
        ] 0,
      unpackTestCase "dictionary with implied nil entry value" (
        C.concat [
          C.pack [ dictionaryByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ] ) []
      [
        expectDictionary,
        expectBool True,
        expectBool False,
        expectStr $ fromIntegral fixlen,
        expectDat fixbin,
        expectNil,
        expectCollectionEnd
      ] 0,
      unpackTestCase "empty object with local class name" (
        C.concat [
          C.pack [
            objectByte, classNameByte,
            fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]]) []
      [
        expectObject,
        expectClassName,
        expectStr $ fromIntegral fixlen,
        expectDat fixbin,
        expectCollectionEnd
      ] 0,
      unpackTestCase "empty object with namespaced class name" (
        C.concat [
          C.pack [
            objectByte, classNameByte,
            fixnsMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]]) []
      [
        expectObject,
        expectClassName,
        expectNs $ fromIntegral fixlen,
        expectDat fixbin,
        expectStr $ fromIntegral fixlen,
        expectDat fixbin,
        expectCollectionEnd
      ] 0,
      unpackTestCase "object with namespaced class name" (
        C.concat [
          C.pack [objectByte, classNameByte, ns8Byte, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, falseByte, fixstrMask, collectionEndByte]]) []
      [
        expectObject,
        expectClassName,
        -- namespace name
        expectNs $ fromIntegral fixlen,
        expectDat fixbin,
        -- local name
        expectStr $ fromIntegral fixlen,
        expectDat fixbin,
        -- property name
        expectNil,
        -- property value
        expectInt (-16),
        -- property name: namespace name
        expectNs $ fromIntegral fixlen,
        expectDat fixbin,
        -- property name: local name
        expectStr 0,
        expectDat C.empty,
        -- property value
        expectBool True,
        -- property name: namespace name
        expectNs $ fromIntegral fixlen,
        expectDat fixbin,
        -- property name: local name
        expectStr 0,
        expectDat C.empty,
        -- property value
        expectBool False,
        -- property name: local name
        expectStr 0,
        expectDat C.empty,
        -- property value
        expectNil,
        expectCollectionEnd
      ] 0
    ]
