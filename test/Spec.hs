{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TupleSections,
    OverloadedStrings #-}

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Data.Bits
import Data.Function (on)
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as C
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import System.Exit (exitFailure)

import Data.DataPack
import Data.DataPack.Unpack

data ByteStringPos = ByteStringPos {
    byteString :: C.ByteString,
    pos :: Int64
  }

instance DataSource ByteStringPos () where
    take = \n bstr bad good -> let
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

data Results = Ok
  | Fail UnpackState Int64 String
  deriving Show

data CatchCallStacks e s st m = CatchCallStacks {
    catchStack :: [Catchers e s (StateT (CatchCallStacks e s st m) m Results)],
    callStack :: [Callbacks s (StateT (CatchCallStacks e s st m) m Results)]
  }

createDefaultCallbacks f = let
    g h a = f (\cb -> h cb a)
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

failCallbacks :: Applicative m => String -> Callbacks ByteStringPos (m Results)
failCallbacks expectedStr = createDefaultCallbacks . const $ \str u s _ ->
    pure $ Fail u (pos s) ("Expected " ++ expectedStr ++ ", found " ++ str)

passCallbacks = createDefaultCallbacks . const . const $ \u s f -> f s

passThruCallbacks :: Monad m =>
  Callbacks ByteStringPos (StateT (CatchCallStacks () ByteStringPos st m) m Results)
passThruCallbacks = createDefaultCallbacks $ \c str u s f ->
  get >>= \ccs ->
    case callStack ccs of
      cs:css' -> modify (\ccs -> ccs { callStack = css' }) >> c cs u s f
      _ -> pure $ Fail u (pos s) $ "unexpected " ++ str

failCatchers :: Applicative m => Catchers () ByteStringPos (m Results)
failCatchers = Catchers {
  stream = \e _ u s _ -> pure $ Fail u (pos s) $ "stream fail " ++ show e,
  invalidByte = \byte allowedByteRanges u s f ->
    pure $ Fail u (pos s) $ "fail " ++ show byte ++
      " is not allowed with given state. Expected values within the ranges of "
      ++ show allowedByteRanges ++ ".",
  unusedByte = \byte u s f ->
    pure $ Fail u (pos s) $ "fail " ++ show byte ++ " is not a usable byte.",
  flogTheDeveloper = \u s ->
    pure $ Fail u (pos s) "Superfail! Flog the developer!" }

passThruCatchers :: Monad m =>
  Catchers () ByteStringPos (StateT (CatchCallStacks () ByteStringPos st m) m Results)
passThruCatchers = let
    feedCatchers f = get >>= \ccs ->
      case catchStack ccs of
        cs:css' -> modify (\ccs -> ccs { catchStack = css' }) >> f cs
        _ -> f failCatchers in
  Catchers {
      stream = \e bStr u s f -> feedCatchers $ \cs -> stream cs e bStr u s f,
      invalidByte = \byte allowedByteRanges u s f -> feedCatchers $ \cs ->
          invalidByte cs byte allowedByteRanges u s f,
      unusedByte = \byte u s f -> feedCatchers $ \cs ->
          unusedByte cs byte u s f,
      flogTheDeveloper = flogTheDeveloper failCatchers }

testCase testName bytString catcherStack callbackStack expectedBytesRemaining =
  (testName, ) <$> (runStateT (
    unpackDataT (ByteStringPos bytString 0) passThruCatchers passThruCallbacks $
      \bStrPos ->
        let len = C.length $ byteString bStrPos in
        pure $ if len == expectedBytesRemaining
          then Ok
          else Fail Root (pos bStrPos) $ "Expected "
            ++ show expectedBytesRemaining
            ++ " byte(s) remaining in byteString. Found "
            ++ show len ++ " byte(s).")
    $ CatchCallStacks catcherStack callbackStack)

runTests testCases =
  putStrLn "Running tests...\n" >> let
      (io, (runCount, failCount)) = foldl (
          \(ioAcc, (runCount, failCount)) fullResults ->
            let
              (testName, (results, _)) = runIdentity fullResults
              runCount' = runCount + 1
              prefix resultStr = '\t':show runCount' ++ ") " ++ resultStr ++ " "
                ++ testName
            in
            case results of
            Ok -> (ioAcc >> putStrLn (prefix "Passed"),
              (runCount', failCount))
            Fail u pos msg -> (ioAcc >> putStrLn (prefix "Failed"
                  ++ ": after reading " ++ show pos ++ " bytes with state of "
                  ++ show u ++ "\x2014" ++ msg), (runCount', failCount + 1))
        ) (pure (), (0, 0)) testCases
  in
  io >>
  if failCount == 0
  then putStrLn $ "\nSuccess! All " ++ show runCount ++ " test(s) passed."
  else (putStrLn $ '\n':show failCount ++ " out of " ++ show runCount
      ++ " test(s) failed.") >> exitFailure

assert expect actual u s f =
  if actual == expect
  then f s
  else pure $ Fail u (pos s) $ "Expected " ++ show expect
      ++ ". Found " ++ show actual

main = runTests [
    testCase "nil" (C.pack [nilByte]) [failCatchers]
      [(failCallbacks "nil") {nil = nil passCallbacks} ] 0,
    testCase "empty content" (C.pack [])
      [failCatchers {stream = \_ _ _ _ _ -> pure Ok}]
      [(failCallbacks "nothing")] 0,
    testCase "single nil with more data" (C.pack [nilByte, dictionaryByte])
      [failCatchers] [(failCallbacks "nil") {nil = nil passCallbacks} ] 1,
    testCase "unused" (C.pack [objectByte + 1, dictionaryByte])
      [failCatchers {unusedByte = \_ _ _ _ -> pure Ok}]
      [(failCallbacks "unused")] 1,
    testCase "true" (C.pack [trueByte]) [failCatchers]
      [(failCallbacks "true") {boolean = assert True} ] 0,
    testCase "false" (C.pack [falseByte, dictionaryByte]) [failCatchers]
      [(failCallbacks "false") {boolean = assert False} ] 1,
    testCase "collection end without matching start"
      (C.pack [collectionEndByte])
      [failCatchers {invalidByte = \_ _ _ _ _ -> pure $ Ok}]
      [(failCallbacks "collectionEnd") ] 0,
    testCase "fixint 63" (C.pack [63]) [failCatchers]
      [(failCallbacks "int") {int64 = assert 63} ] 0,
    testCase "fixint -64" (C.pack [fixintMask]) [failCatchers]
      [(failCallbacks "int") {int64 = assert (-64)} ] 0,
    testCase "uint8 without data" (C.pack [uint8Byte])
      [failCatchers {stream = \_ _ _ _ _ -> pure Ok}] [failCallbacks "uint8"] 0,
    testCase "uint8" (C.pack [uint8Byte, 0xff]) [failCatchers]
      [(failCallbacks "uint8") { int64 = assert 255 } ] 0,
    testCase "uint16" (C.pack [uint16Byte, 0xff, 0xff]) [failCatchers]
      [(failCallbacks "uint16") {int64 = assert 65535} ] 0,
    testCase "uint32" (C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff])
      [failCatchers] [ (failCallbacks "uint32") {int64 = assert 4294967295} ] 0,
    testCase "uint64"
      (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])
      [failCatchers] [(failCallbacks "uint64")
        {uint64 = assert 18446744073709551615} ] 0,
    testCase "int8" (C.pack [int8Byte, 0xff]) [failCatchers]
      [(failCallbacks "int8") {int64 = assert (-1)} ] 0,
    testCase "int16" (C.pack [int16Byte, 0xff, 0xff]) [failCatchers]
      [(failCallbacks "int16") {int64 = assert (-1)} ] 0,
    testCase "int32" (C.pack [int32Byte, 0xff, 0xff, 0xff, 0xff])
      [failCatchers] [(failCallbacks "int32") {int64 = assert (-1)} ] 0,
    testCase "int64"
      (C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])
      [failCatchers] [(failCallbacks "int64") {int64 = assert (-1)} ] 0,
    testCase "float" (C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]) [failCatchers]
      [(failCallbacks "float") {float = assert 2.625} ] 0,
    testCase "double"
      (C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      [failCatchers] [(failCallbacks "double") {double = assert (-2.625)} ] 0,
    testCase "fixbin"
      (C.pack
        [fixbinMask .|. 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      [failCatchers] [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 7,
    testCase "bin8"
      (C.pack [bin8Byte, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      [failCatchers] [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 6,
    testCase "bin16"
      (C.pack [bin16Byte, 0, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00])
      [failCatchers] [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 5,
    testCase "bin32"
      (C.pack [bin32Byte, 0x00, 0x00, 0, 0x1, 0x05, 0x00, 0x00, 0x00])
      [failCatchers] [
        (failCallbacks "binStart") {binStart = assert 1},
        (failCallbacks "dat") {dat = assert (C.pack [0x05])}
      ] 3,
    ( let
         bStr = encodeUtf8 $ T.pack "str8"
         len = C.length bStr
      in
      testCase "fixstr"
        (C.concat [C.pack [fixstrMask .|. fromIntegral len], bStr])
        [failCatchers] [
          (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
          (failCallbacks "dat") {dat = assert bStr}
        ] 0 ),
    ( let
         bStr = encodeUtf8 $ T.pack "str8"
         len = C.length bStr
      in
      testCase "str8"
        (C.concat [
          C.pack [str8Byte, fromIntegral len], bStr,
          C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        ]) [failCatchers] [
          (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
          (failCallbacks "dat") {dat = assert bStr}
        ] 6 ),
    ( let
         bStr = encodeUtf8 $ T.pack "str16"
         len = C.length bStr
      in
      testCase "str16"
        (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr])
        [failCatchers] [
          (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
          (failCallbacks "dat") {dat = assert bStr} ] 0 ),
    ( let
         bStr = encodeUtf8 $ T.pack "str32"
         len = C.length bStr
      in
      testCase "str32"
        (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr])
        [failCatchers] [
          (failCallbacks "strStart") {strStart = assert $ fromIntegral len},
          (failCallbacks "dat") {dat = assert bStr}
        ] 0 ),
    testCase "out of context namespace name" (C.pack [fixnsMask])
      [failCatchers {invalidByte = \_ _ _ _ _ -> pure $ Ok}]
      [(failCallbacks "fixnsMask")] 0,
    testCase "class name outside of sequences and objects"
      (C.pack [classNameByte])
      [failCatchers {invalidByte = \_ _ _ _ _ -> pure $ Ok}]
      [(failCallbacks "className") ] 0,
    testCase "empty sequence" (C.pack [sequenceByte, collectionEndByte])
      [failCatchers] [
        (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks},
        (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}
      ] 0 ]

-- collectionEndByte
-- ns8Byte           = 0x54::Word8
-- ns16Byte          = 0x55::Word8
-- ns32Byte          = 0x56::Word8
-- classNameByte     = 0x57::Word8
-- sequenceByte      = 0x58::Word8
-- dictionaryByte    = 0x59::Word8
-- objectByte        = 0x5a::Word8
-- fixnsMask         = 0xa0::Word8
