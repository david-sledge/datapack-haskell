{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TupleSections,
    OverloadedStrings, Rank2Types #-}

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Data.Bits
import Data.Function (on)
import Data.Word
import Data.String
import Data.Int
import Data.Text.Lazy.Encoding
import System.Exit (exitFailure)

import Data.DataPack
import Data.DataPack.Unpack
import Data.DataPack.Pack

import qualified Data.ByteString.Lazy as C
import qualified Data.Text.Lazy as T

import qualified Data.DataPack.Unpack as U (stream, flogTheDeveloper, UnpackState(..))
import qualified Data.DataPack.Pack as P (stream, flogTheDeveloper, PackState(..))

data ByteStringPos = ByteStringPos {
    byteString :: C.ByteString,
    pos :: Int64
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

data CatchCallStacks e s st t m = CatchCallStacks {
    catchStack :: [UnpackCatchers e s (StateT (CatchCallStacks e s st t m) m (s, Maybe ((t -> String) -> String)))],
    callStack :: [Callbacks s (StateT (CatchCallStacks e s st t m) m (s, Maybe ((t -> String) -> String)))]
  }

createDefaultCallbacks f = let
    g h a = f (`h` a)
  in
  Callbacks {
      nil = f nil "nil",
      collectionEnd = f collectionEnd "collectionEnd",
      boolean = \a -> g boolean a "boolean",
      uint64 = \a -> g uint64 a "uint64",
      int64 = \a -> g int64 a "int64",
      float = \a -> g float a "float",
      double = \a -> g double a "double",
      binStart = \a -> g binStart a "binStart",
      strStart = \a -> g strStart a "strStart",
      nsStart = \a -> g nsStart a "nsStart",
      dat = \a -> g dat a "dat",
      className = f className "className",
      sequenceD = f sequenceD "sequenceD",
      dictionary = f dictionary "dictionary",
      object = f object "object" }

unpackFailMessage u pos msg = Just $ \f -> f "Failed"
  ++ ": after reading " ++ show pos ++ " bytes with state of "
  ++ show u ++ "\x2014" ++ msg

unpackSourceFailMessage u s msg = pure (s, unpackFailMessage u (pos s) msg)

failCallbacks expectedStr = createDefaultCallbacks . const $ \str u s _ ->
    unpackSourceFailMessage u s ("Expected " ++ expectedStr ++ ", found " ++ str)

passCallbacks = createDefaultCallbacks . const . const $ \u s f -> f s

passThruCallbacks = createDefaultCallbacks $ \c str u s f ->
  get >>= \ccs ->
    case callStack ccs of
      cs:css -> put (ccs { callStack = css }) >> c cs u s f
      _ -> unpackSourceFailMessage u s $ "unexpected " ++ str

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

testCase testName bytString catcherStack callbackStack expectedBytesRemaining =
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
        _ -> case catchStack catchCallStacks of
          _:_ -> unpackFailMessage U.Root (pos s) $ "Expected " ++ show (length $ catchStack catchCallStacks) ++ " more exceptions for negative testing"
          _ -> case callStack catchCallStacks of
            _:_ -> unpackFailMessage U.Root (pos s) "Expected more bytes in the root value."
            _ -> passFail)

runTests testCases =
  putStrLn "Running tests...\n" >> let
    (io, (runCount, failCount)) = foldl (
        \(ioAcc, (runCount, failCount)) fullResults ->
          let
            (testName, results) = runIdentity fullResults
            runCount' = runCount + 1
            prefix resultStr = '\t':show runCount' ++ ") " ++ resultStr ++ " "
              ++ testName
          in
          case results of
          Nothing -> (ioAcc >> putStrLn (prefix "Passed"),
            (runCount', failCount))
          Just f -> (ioAcc >> putStrLn (f prefix), (runCount', failCount + 1))
      ) (pure (), (0, 0)) testCases
  in
  io >>
  if failCount == 0
  then putStrLn $ "\nSuccess! All " ++ show runCount ++ " test(s) passed."
  else putStrLn ('\n':show failCount ++ " out of " ++ show runCount
      ++ " test(s) failed.") >> exitFailure

assert expect actual u s f =
  if actual == expect
  then f s
  else unpackSourceFailMessage u s $ "Expected " ++ show expect
      ++ ". Found " ++ show actual

instance DataDestination C.ByteString () where
    give bstr d _ good = good $ C.concat [d, bstr]

packFailMessage p msg = Just $ \f -> f "Failed" ++ ": with state of "
  ++ show p ++ "\x2014" ++ msg

packDestinationFailMessage p msg = pure (Just $ \f -> f "Failed" ++ ": with state of "
  ++ show p ++ "\x2014" ++ msg)

main = runTests [
    testCase "nil" (C.pack [nilByte]) []
      [(failCallbacks "nil") {nil = nil passCallbacks} ] 0,
    testCase "empty content (negative test)" (C.pack [])
      [failUnpackCatchers {U.stream = \_ _ _ s _ -> pure (s, Nothing)}] [] 0,
    testCase "single nil with more data" (C.pack [nilByte, dictionaryByte])
      [] [(failCallbacks "nil") {nil = nil passCallbacks} ] 1,
    testCase "unused (negative test)" (C.pack [objectByte + 1, nilByte])
      [failUnpackCatchers {unusedByte = \_ _ s f -> f s}]
      [(failCallbacks "nil") {nil = nil passCallbacks}] 0,
    testCase "true" (C.pack [trueByte]) []
      [(failCallbacks "true") {boolean = assert True} ] 0,
    testCase "false" (C.pack [falseByte, dictionaryByte]) []
      [(failCallbacks "false") {boolean = assert False} ] 1,
    testCase "collection end without matching start (negative test)"
      (C.pack [collectionEndByte])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}]
      [] 0,
    testCase "fixint 63" (C.pack [63]) []
      [(failCallbacks "int") {int64 = assert 63} ] 0,
    testCase "fixint -64" (C.pack [fixintMask]) []
      [(failCallbacks "int") {int64 = assert (-64)} ] 0,
    testCase "uint8 without data (negative test)" (C.pack [uint8Byte])
      [failUnpackCatchers {U.stream = \_ _ _ s _ -> pure (s, Nothing)}] [] 0,
    testCase "uint8" (C.pack [uint8Byte, 0xff]) []
      [(failCallbacks "uint8") { int64 = assert 255 } ] 0,
    testCase "uint16" (C.pack [uint16Byte, 0xff, 0xff]) []
      [(failCallbacks "uint16") {int64 = assert 65535} ] 0,
    testCase "uint32" (C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff]) []
      [(failCallbacks "uint32") {int64 = assert 4294967295}] 0,
    testCase "uint64"
      (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) []
      [(failCallbacks "uint64") {uint64 = assert 18446744073709551615} ] 0,
    testCase "int8" (C.pack [int8Byte, 0xff]) []
      [(failCallbacks "int8") {int64 = assert (-1)} ] 0,
    testCase "int16" (C.pack [int16Byte, 0xff, 0xff]) []
      [(failCallbacks "int16") {int64 = assert (-1)} ] 0,
    testCase "int32" (C.pack [int32Byte, 0xff, 0xff, 0xff, 0xff]) []
      [(failCallbacks "int32") {int64 = assert (-1)} ] 0,
    testCase "int64"
      (C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) []
      [(failCallbacks "int64") {int64 = assert (-1)} ] 0,
    testCase "float" (C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]) []
      [(failCallbacks "float") {float = assert 2.625} ] 0,
    testCase "double"
      (C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [(failCallbacks "double") {double = assert (-2.625)} ] 0,
    testCase "fixbin"
      (C.pack
        [fixbinMask .|. 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      [] [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 7,
    testCase "bin8"
      (C.pack [bin8Byte, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 6,
    testCase "bin16"
      (C.pack [bin16Byte, 0, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 5,
    testCase "bin32"
      (C.pack [bin32Byte, 0x00, 0x00, 0, 0x1, 0x05, 0x00, 0x00, 0x00]) []
      [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 3,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "fixstr"
      (C.concat [C.pack [fixstrMask .|. fromIntegral len], bStr]) []
      [
        (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
        (failCallbacks "dat") {dat = assert bStr}
      ] 0,
    let
      bStr = encodeUtf8 $ T.pack "str8"
      len = C.length bStr
    in
    testCase "str8"
      (C.concat [
        C.pack [str8Byte, fromIntegral len], bStr,
        C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      ]) [] [
        (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
        (failCallbacks "dat") {dat = assert bStr}
      ] 6,
    let
      bStr = encodeUtf8 $ T.pack "str16"
      len = C.length bStr
    in
    testCase "str16"
      (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr]) []
      [
        (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
        (failCallbacks "dat") {dat = assert bStr} ] 0,
    let
      bStr = encodeUtf8 $ T.pack "str32"
      len = C.length bStr
    in
    testCase "str32"
      (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr]) []
      [
        (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
        (failCallbacks "dat") {dat = assert bStr}
      ] 0,
    testCase "out of context namespace name (negative test)"
      (C.pack [fixnsMask])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}]
      [] 0,
    testCase "class name outside of sequences and objects (negative test)"
      (C.pack [classNameByte])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}]
      [] 0,
    testCase "empty sequence" (C.pack [sequenceByte, collectionEndByte]) []
      [
        (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks},
        (failCallbacks "collectionEnd")
          {collectionEnd = collectionEnd passCallbacks}
      ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "empty sequence with local class name" (
      C.concat [
        C.pack [
          sequenceByte, classNameByte,
          fixstrMask .|. fromIntegral len],
        bStr,
        C.pack [collectionEndByte]]) []
      [
      (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks},
      (failCallbacks "className") {className = className passCallbacks},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "collectionEnd")
        {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "empty sequence with namespaced class name" (
      C.concat [
        C.pack [
          sequenceByte, classNameByte,
          fixnsMask .|. fromIntegral len],
        bStr,
        C.pack [fixstrMask .|. fromIntegral len],
        bStr,
        C.pack [collectionEndByte]]) []
    [
      (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks},
      (failCallbacks "className") {className = className passCallbacks},
      (failCallbacks "nsStart") {nsStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "collectionEnd")
        {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "sequence" (
      C.concat [
        C.pack [ sequenceByte, nilByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral len ],
        bStr,
        C.pack [ collectionEndByte ] ] ) []
    [
      (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks},
      (failCallbacks "nil") {nil = nil passCallbacks},
      (failCallbacks "boolean") {boolean = assert True},
      (failCallbacks "boolean") {boolean = assert False},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    testCase "empty dictionary" (
        C.pack [ dictionaryByte, collectionEndByte ] ) []
      [
        (failCallbacks "dictionary") {dictionary = dictionary passCallbacks},
        (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}
      ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "dictionary" (
      C.concat [
        C.pack [ dictionaryByte, nilByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral len ],
        bStr,
        C.pack [ collectionEndByte ] ] ) []
      [
        (failCallbacks "dictionary") {dictionary = dictionary passCallbacks},
        (failCallbacks "nil") {nil = nil passCallbacks},
        (failCallbacks "boolean") {boolean = assert True},
        (failCallbacks "boolean") {boolean = assert False},
        (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
        (failCallbacks "dat") {dat = assert bStr},
        (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}
      ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "dictionary with implied nil entry value" (
      C.concat [
        C.pack [ dictionaryByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral len ],
        bStr,
        C.pack [ collectionEndByte ] ] ) []
    [
      (failCallbacks "dictionary") {dictionary = dictionary passCallbacks},
      (failCallbacks "boolean") {boolean = assert True},
      (failCallbacks "boolean") {boolean = assert False},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "nil") {nil = nil passCallbacks},
      (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "empty object with local class name" (
      C.concat [
        C.pack [
          objectByte, classNameByte,
          fixstrMask .|. fromIntegral len],
        bStr,
        C.pack [collectionEndByte]]) []
    [
      (failCallbacks "object") {object = object passCallbacks},
      (failCallbacks "className") {className = className passCallbacks},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "collectionEnd")
        {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "empty object with namespaced class name" (
      C.concat [
        C.pack [
          objectByte, classNameByte,
          fixnsMask .|. fromIntegral len],
        bStr,
        C.pack [fixstrMask .|. fromIntegral len],
        bStr,
        C.pack [collectionEndByte]]) []
    [
      (failCallbacks "object") {object = object passCallbacks},
      (failCallbacks "className") {className = className passCallbacks},
      (failCallbacks "nsStart") {nsStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "collectionEnd")
        {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    let
      bStr = encodeUtf8 $ T.pack "fixstr"
      len = C.length bStr
    in
    testCase "object with namespaced class name" (
      C.concat [
        C.pack [objectByte, classNameByte, ns8Byte, fromIntegral len],
        bStr,
        C.pack [fixstrMask .|. fromIntegral len],
        bStr,
        C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral len],
        bStr,
        C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, fromIntegral len],
        bStr,
        C.pack [fixstrMask, falseByte, fixstrMask, collectionEndByte]]) []
    [
      (failCallbacks "object") {object = object passCallbacks},
      (failCallbacks "className") {className = className passCallbacks},
      (failCallbacks "nsStart") {nsStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "nil") {nil = nil passCallbacks},
      (failCallbacks "int64") {int64 = assert (-16)},
      (failCallbacks "nsStart") {nsStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "strStart") {strStart = assert 0},
      (failCallbacks "dat") {dat = assert C.empty},
      (failCallbacks "true") {boolean = assert True},
      (failCallbacks "nsStart") {nsStart = assert $ fromIntegral len},
      (failCallbacks "dat") {dat = assert bStr},
      (failCallbacks "strStart") {strStart = assert 0},
      (failCallbacks "dat") {dat = assert C.empty},
      (failCallbacks "false") {boolean = assert False},
      (failCallbacks "strStart") {strStart = assert 0},
      (failCallbacks "dat") {dat = assert C.empty},
      (failCallbacks "nil") {nil = nil passCallbacks},
      (failCallbacks "collectionEnd")
        {collectionEnd = collectionEnd passCallbacks}
    ] 0,
    ("pack Nil", ) <$> packDataT C.empty PackCatchers {
        P.stream = \e _ p d _ -> packDestinationFailMessage p $ "stream fail " ++ show e,
        tooBig = \dat p d _ -> packDestinationFailMessage p $ "too big " ++ show (C.length dat),
        invalidPackType = \p d _ -> packDestinationFailMessage p "invalid pack type",
        P.flogTheDeveloper = \p d -> packDestinationFailMessage p "Superfail! Flog the developer!"
      } packNil (\d -> pure $ if d == C.pack [nilByte] then Nothing else packFailMessage P.Root ("expected nilByte. Found " ++ show d)) ]
