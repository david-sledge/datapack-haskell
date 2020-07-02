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
import Numeric (showHex)
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

runTests unpackTestCases =
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
      ) (pure (), (0, 0)) unpackTestCases
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

packDestinationFailMessage p msg = pure (Just $ \f -> f "Failed"
  ++ ": with state of " ++ show p ++ "\x2014" ++ msg)

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

expectSequence = (failCallbacks "sequenceD") {sequenceD = sequenceD passCallbacks}

expectDictionary = (failCallbacks "dictionary") {dictionary = dictionary passCallbacks}

expectObject = (failCallbacks "object") {object = object passCallbacks}

expectClassName = (failCallbacks "className") {className = className passCallbacks}

expectCollectionEnd = (failCallbacks "collectionEnd") {collectionEnd = collectionEnd passCallbacks}

failPackCatchers = PackCatchers {
    P.stream = \e _ p _ _ -> packDestinationFailMessage p $ "stream fail " ++ show e,
    tooBig = \dat p _ _ -> packDestinationFailMessage p $ "too big " ++ show (C.length dat),
    invalidPackType = \p _ _ -> packDestinationFailMessage p "invalid pack type",
    P.flogTheDeveloper = \p _ -> packDestinationFailMessage p "Superfail! Flog the developer!"
  }

showValues d = case C.uncons d of
  Just (word, sTail) -> showHex word $ ':':showValues sTail
  _ -> []

packTestCase testName catchers pack expected = (testName, ) <$> packDataT C.empty catchers pack
  (\d -> pure $ if d == expected
    then Nothing
    else packFailMessage P.Root ("expected " ++ showValues expected ++ ". Found " ++ showValues d))

packIntTestCase n = packTestCase ("pack " ++ show n) failPackCatchers (packInt n)

testString = "Cake tootsie roll chocolate bar dragée cupcake carrot cake cotton\
  \ candy sesame snaps. Cake sweet roll oat cake I love cake oat cake liquorice\
  \ toffee halvah. Tootsie roll I love jelly fruitcake bear claw candy canes. I\
  \ love apple pie liquorice. Lemon drops cotton candy jelly tootsie roll jelly\
  \ caramels candy. Gingerbread I love sugar plum bonbon marshmallow I love \
  \jelly beans topping pastry. I love candy canes I love oat cake muffin candy \
  \canes cotton candy I love. Caramels pudding pastry pie sweet sweet I love \
  \pastry. Oat cake lollipop cake I love soufflé jelly beans. Tiramisu toffee \
  \oat cake icing pie chocolate bar apple pie. Dragée cookie caramels pastry \
  \wafer. Gummies I love jelly jelly beans. I love dragée jelly jelly beans. \
  \Topping ice cream jelly-o biscuit.\n\nSugar plum macaroon liquorice \
  \chocolate cake chocolate. Lemon drops powder cupcake. Marzipan pastry ice \
  \cream tiramisu gummies apple pie cheesecake chocolate bar lemon drops. \
  \Croissant caramels sweet roll jujubes dragée carrot cake. Jujubes jujubes \
  \candy ice cream I love. Tootsie roll liquorice cheesecake I love I love \
  \cotton candy sweet lollipop. Tootsie roll carrot cake pudding cake I love I \
  \love. Biscuit I love dessert gummi bears tootsie roll tart jujubes. Jelly \
  \marzipan I love marzipan candy oat cake biscuit. Powder wafer oat cake I \
  \love chocolate bear claw pie croissant. Candy candy soufflé powder biscuit. \
  \Topping I love I love sesame snaps.\n\nOat cake chocolate cake tiramisu \
  \danish cake. Pudding gummies marzipan. Gingerbread brownie tiramisu carrot \
  \cake ice cream I love donut danish. Icing toffee dessert croissant candy \
  \canes candy jujubes. Cake I love tiramisu tart jelly beans. Soufflé bonbon \
  \dessert I love icing. Powder chocolate I love apple pie. Croissant apple pie\
  \ dragée dragée chocolate bar candy canes jujubes oat cake. Dragée \
  \gingerbread chocolate cake lemon drops gummies biscuit. Marzipan chocolate \
  \cake bear claw. Carrot cake pudding jujubes icing sugar plum I love chupa \
  \chups. Ice cream ice cream I love.\n\nJelly beans cupcake dessert bear claw \
  \candy canes chupa chups gummi bears. Chocolate bar lemon drops toffee candy \
  \canes gingerbread. Pie toffee chocolate I love powder halvah apple pie. \
  \Gummies cheesecake apple pie. Marzipan pudding liquorice icing. Tootsie roll\
  \ cake I love jujubes. I love dessert oat cake sesame snaps biscuit. I love \
  \chupa chups jelly-o caramels cotton candy marzipan wafer. Powder gummies \
  \donut bear claw icing jelly beans dessert chocolate bar. Brownie jelly \
  \toffee pastry cake. Donut biscuit chocolate liquorice I love candy croissant\
  \ carrot cake caramels. Toffee chupa chups dessert chupa chups chocolate cake\
  \ macaroon. Cheesecake chupa chups dessert pie. Sesame snaps I love bear claw\
  \.\n\nCaramels oat cake tart cake biscuit I love cupcake. Croissant lemon \
  \drops I love I love. Jelly-o sweet roll oat cake I love. Chocolate ice cream\
  \ tart. Chocolate bar gummies sweet roll I love marshmallow danish chupa \
  \chups bear claw. Carrot cake I love cheesecake gummi bears bear claw \
  \cheesecake brownie. Sugar plum cake chupa chups chocolate bar. Lollipop \
  \sweet roll toffee lemon drops cotton candy candy canes chocolate cake \
  \chocolate cake jelly beans. I love chocolate I love. Jujubes jelly beans pie\
  \ candy. Jelly-o jelly-o chocolate cake soufflé macaroon I love dragée \
  \pudding cake. Lemon drops brownie jelly powder pudding. Jelly jelly-o apple\
  \ pie. Dragée candy lemon drops gummies sugar plum pastry donut lemon drops\
  \ croissant."

main = let
  str2Bin = encodeUtf8 . T.pack
  fixstr = "fixstr"
  fixbin = str2Bin fixstr
  fixlen = C.length fixbin
  str8 = Prelude.take 32 testString
  bin8 = str2Bin str8
  str8again = Prelude.take 128 testString
  bin8again = str2Bin str8again
  bin16 = str2Bin testString
  f bin = if C.length bin <= 0xffff
    then f $ C.concat [bin, bin]
    else bin
  bin32 = f bin16
  len32 = C.length bin32
  in
  runTests [
    unpackTestCase "nil" (C.pack [nilByte]) []
      [expectNil] 0,
    unpackTestCase "empty content (negative test)" (C.pack [])
      [failUnpackCatchers {U.stream = \_ _ _ s _ -> pure (s, Nothing)}] [] 0,
    unpackTestCase "single nil with more data" (C.pack [nilByte, dictionaryByte])
      [] [expectNil] 1,
    unpackTestCase "unused (negative test)" (C.pack [objectByte + 1, nilByte])
      [failUnpackCatchers {unusedByte = \_ _ s f -> f s}]
      [expectNil] 0,
    unpackTestCase "true" (C.pack [trueByte]) []
      [expectBool True] 0,
    unpackTestCase "false" (C.pack [falseByte, dictionaryByte]) []
      [expectBool False] 1,
    unpackTestCase "collection end without matching start (negative test)"
      (C.pack [collectionEndByte])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}]
      [] 0,
    unpackTestCase "fixint 63" (C.pack [63]) []
      [expectInt 63] 0,
    unpackTestCase "fixint -64" (C.pack [fixintMask]) []
      [expectInt (-64)] 0,
    unpackTestCase "uint8 without data (negative test)" (C.pack [uint8Byte])
      [failUnpackCatchers {U.stream = \_ _ _ s _ -> pure (s, Nothing)}] [] 0,
    unpackTestCase "uint8" (C.pack [uint8Byte, 0xff]) []
      [expectInt 255] 0,
    unpackTestCase "uint16" (C.pack [uint16Byte, 0xff, 0xff]) []
      [expectInt 65535] 0,
    unpackTestCase "uint32" (C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff]) []
      [expectInt 4294967295] 0,
    unpackTestCase "uint64"
      (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) []
      [expectUint 18446744073709551615] 0,
    unpackTestCase "int8" (C.pack [int8Byte, 0xff]) []
      [expectInt (-1)] 0,
    unpackTestCase "int16" (C.pack [int16Byte, 0xff, 0xff]) []
      [expectInt (-1)] 0,
    unpackTestCase "int32" (C.pack [int32Byte, 0xff, 0xff, 0xff, 0xff]) []
      [expectInt (-1)] 0,
    unpackTestCase "int64"
      (C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) []
      [expectInt (-1)] 0,
    unpackTestCase "float" (C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]) []
      [expectFloat 2.625] 0,
    unpackTestCase "double"
      (C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [expectDouble (-2.625)] 0,
    unpackTestCase "fixbin"
      (C.pack
        [fixbinMask .|. 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      [] [
        expectBin 1,
        expectDat (C.pack [0x05])
      ] 7,
    unpackTestCase "bin8"
      (C.pack [bin8Byte, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [
        expectBin 1,
        expectDat (C.pack [0x05])
      ] 6,
    unpackTestCase "bin16"
      (C.pack [bin16Byte, 0, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00]) []
      [
        expectBin 1,
        expectDat (C.pack [0x05])
      ] 5,
    unpackTestCase "bin32"
      (C.pack [bin32Byte, 0x00, 0x00, 0, 0x1, 0x05, 0x00, 0x00, 0x00]) []
      [
        expectBin 1,
        expectDat (C.pack [0x05])
      ] 3,
    unpackTestCase fixstr
      (C.concat [C.pack [fixstrMask .|. fromIntegral fixlen], fixbin]) []
      [
        expectStr $ fromIntegral fixlen,
        expectDat fixbin
      ] 0,
    let
      bStr = str2Bin "str8"
      len = C.length bStr
    in
    unpackTestCase "str8"
      (C.concat [
        C.pack [str8Byte, fromIntegral len], bStr,
        C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      ]) [] [
        expectStr $ fromIntegral len,
        expectDat bStr
      ] 6,
    let
      bStr = str2Bin "str16"
      len = C.length bStr
    in
    unpackTestCase "str16"
      (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr]) []
      [
        expectStr $ fromIntegral len,
        expectDat bStr ] 0,
    let
      bStr = str2Bin "str32"
      len = C.length bStr
    in
    unpackTestCase "str32"
      (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr]) []
      [
        expectStr $ fromIntegral len,
        expectDat bStr
      ] 0,
    unpackTestCase "out of context namespace name (negative test)"
      (C.pack [fixnsMask])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}]
      [] 0,
    unpackTestCase "class name outside of sequences and objects (negative test)"
      (C.pack [classNameByte])
      [failUnpackCatchers {invalidByte = \_ _ _ s _ -> pure (s, Nothing)}]
      [] 0,
    unpackTestCase "empty sequence" (C.pack [sequenceByte, collectionEndByte]) []
      [
        expectSequence,
        expectCollectionEnd
      ] 0,
    unpackTestCase "empty sequence with local class name" (
      C.concat [
        C.pack [
          sequenceByte, classNameByte,
          fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]]) []
      [
      expectSequence,
      expectClassName,
      expectStr $ fromIntegral fixlen,
      expectDat fixbin,
      expectCollectionEnd
    ] 0,
    unpackTestCase "empty sequence with namespaced class name" (
      C.concat [
        C.pack [
          sequenceByte, classNameByte,
          fixnsMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]]) []
    [
      expectSequence,
      expectClassName,
      expectNs $ fromIntegral fixlen,
      expectDat fixbin,
      expectStr $ fromIntegral fixlen,
      expectDat fixbin,
      expectCollectionEnd
    ] 0,
    unpackTestCase "sequence" (
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
    ] 0,

    packTestCase "pack Nil" failPackCatchers packNil $ C.pack [nilByte],
    packTestCase "pack True" failPackCatchers packTrue $ C.pack [trueByte],
    packTestCase "pack False" failPackCatchers packFalse $ C.pack [falseByte],

    packIntTestCase (0::Int64) $ C.pack [0],
    packIntTestCase (63::Word8) $ C.pack [0x3f],
    packIntTestCase (-64::Int) $ C.pack [fixintMask],

    packIntTestCase (-65::Int) $ C.pack [int8Byte, 0xbf],
    packIntTestCase (64::Int) $ C.pack [int8Byte, 0x40],
    packIntTestCase (127::Int) $ C.pack [int8Byte, 0x7f],
    packIntTestCase (-128::Int) $ C.pack [int8Byte, 0x80],

    packIntTestCase (128::Int) $ C.pack [uint8Byte, 0x80],
    packIntTestCase (255::Int) $ C.pack [uint8Byte, 0xff],

    packIntTestCase (-129::Int) $ C.pack [int16Byte, 0xff, 0x7f],
    packIntTestCase (256::Int) $ C.pack [int16Byte, 0x01, 0],
    packIntTestCase (32767::Int) $ C.pack [int16Byte, 0x7f, 0xff],
    packIntTestCase (-32768::Int) $ C.pack [int16Byte, 0x80, 0],

    packIntTestCase (32768::Int64) $ C.pack [uint16Byte, 0x80, 0],
    packIntTestCase (65535::Int) $ C.pack [uint16Byte, 0xff, 0xff],

    packIntTestCase (-32769::Int) $ C.pack [int32Byte, 0xff, 0xff, 0x7f, 0xff],
    packIntTestCase (65536::Int) $ C.pack [int32Byte, 0, 0x01, 0, 0],
    packIntTestCase (2147483647::Int) $ C.pack [int32Byte, 0x7f, 0xff, 0xff, 0xff],
    packIntTestCase (-2147483648::Int) $ C.pack [int32Byte, 0x80, 0, 0, 0],

    packIntTestCase (2147483648::Int64) $ C.pack [uint32Byte, 0x80, 0, 0, 0],
    packIntTestCase (4294967295::Int64) $ C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff],

    packIntTestCase (-2147483649::Int64) $ C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff],
    packIntTestCase (4294967296::Int64) $ C.pack [int64Byte, 0, 0, 0, 0x01, 0, 0, 0, 0],
    packIntTestCase (9223372036854775807::Int64) $ C.pack [int64Byte, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
    packIntTestCase (-9223372036854775808::Int64) $ C.pack [int64Byte, 0x80, 0, 0, 0, 0, 0, 0, 0],

    packIntTestCase (9223372036854775808::Word64) $ C.pack [uint64Byte, 0x80, 0, 0, 0, 0, 0, 0, 0],
    packIntTestCase (18446744073709551615::Word64) $ C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],

    packTestCase "pack float" failPackCatchers (packFloat 2.625) $ C.pack [floatByte, 0x40, 0x28, 0x00, 0x00],
    packTestCase "pack double" failPackCatchers (packDouble (-2.625)) $ C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],

    packTestCase "pack fixstr" failPackCatchers (packStringValue fixstr) $ C.cons (fixstrMask .|. 6) fixbin,
    packTestCase "pack str8" failPackCatchers (packStringValue str8) . C.cons str8Byte $ C.cons (fromIntegral $ C.length bin8) bin8,
    packTestCase "pack str8 again" failPackCatchers (packStringValue str8again) . C.cons str8Byte $ C.cons (fromIntegral $ C.length bin8again) bin8again,
    packTestCase "pack str16" failPackCatchers (packStringValue testString) . C.cons str16Byte $ C.cons (fromIntegral $ shiftR (C.length bin16) 8) (C.cons (fromIntegral $ C.length bin16) bin16),
    packTestCase "pack str32" failPackCatchers (packStringValue . T.unpack . decodeUtf8 $ bin32) $ C.cons str32Byte (
        C.cons (fromIntegral $ shiftR len32 24) (
          C.cons (fromIntegral $ shiftR len32 16) (
            C.cons (fromIntegral $ shiftR len32 8) (
              C.cons (fromIntegral len32) bin32 ) ) ) ),

    packTestCase "pack fixbin" failPackCatchers (packBinValue fixbin) $ C.cons (fixbinMask .|. 6) fixbin,
    packTestCase "pack bin8" failPackCatchers (packBinValue bin8) . C.cons bin8Byte $ C.cons (fromIntegral $ C.length bin8) bin8,
    packTestCase "pack bin8 again" failPackCatchers (packBinValue bin8again) . C.cons bin8Byte $ C.cons (fromIntegral $ C.length bin8again) bin8again,
    packTestCase "pack bin16" failPackCatchers (packBinValue bin16) . C.cons bin16Byte $ C.cons (fromIntegral $ shiftR (C.length bin16) 8) (C.cons (fromIntegral $ C.length bin16) bin16),
    packTestCase "pack bin32" failPackCatchers (packBinValue bin32) $ C.cons bin32Byte (
        C.cons (fromIntegral $ shiftR len32 24) (
          C.cons (fromIntegral $ shiftR len32 16) (
            C.cons (fromIntegral $ shiftR len32 8) (
              C.cons (fromIntegral len32) bin32 ) ) ) ),

    packTestCase "pack empty unclased sequence" failPackCatchers (
        packSequence Nothing (
            pure ()
          )
      ) $ C.pack [sequenceByte, collectionEndByte],
    packTestCase "pack empty local name clased sequence" failPackCatchers (
        packSequence (Just (Nothing, "fixns")) (
            pure ()
          )
      ) $ C.pack [
        sequenceByte, classNameByte, fixstrMask .|. 5, 102, 105, 120, 110, 115,
        collectionEndByte],
    packTestCase "pack empty qualified name clased sequence" failPackCatchers (
        packSequence (Just (Just "fixns", "fixns")) (
            pure ()
          )
      ) $ C.pack [
        sequenceByte, classNameByte, fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
        collectionEndByte],

    packTestCase "pack non-empty unclased sequence" failPackCatchers (
        packSequence Nothing packNil
      ) $ C.pack [sequenceByte, nilByte, collectionEndByte],
    packTestCase "pack non-empty local name clased sequence" failPackCatchers (
        packSequence (Just (Nothing, "fixns")) packNil
      ) $ C.pack [
        sequenceByte, classNameByte, fixstrMask .|. 5, 102, 105, 120, 110, 115,
          nilByte,
        collectionEndByte],
    packTestCase "pack non-empty qualified name clased sequence" failPackCatchers (
        packSequence (Just (Just "fixns", "fixns")) packNil
      ) $ C.pack [
        sequenceByte, classNameByte, fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
          nilByte,
        collectionEndByte],

    packTestCase "pack empty dictionary" failPackCatchers (
        packDictionary (
            pure ()
          )
      ) $ C.pack [dictionaryByte, collectionEndByte],
    packTestCase "pack non-empty dictionary" failPackCatchers (
        packDictionary (
            -- key
            packSequence (Just (Just "fixns", "fixns")) packNil >>
            -- value
            packNil >>
            -- key
            packTrue >>
            -- value
            packFalse
          )
      ) $ C.pack [
          dictionaryByte,
            sequenceByte, classNameByte, fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
              nilByte,
            collectionEndByte,
            nilByte,
            trueByte,
            falseByte,
          collectionEndByte],

    packTestCase "pack property as root (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packProperty Nothing "" packNil) C.empty,
    packTestCase "pack property in sequence (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packSequence Nothing (packProperty Nothing "" packNil)) $ C.pack [sequenceByte, collectionEndByte],
    packTestCase "pack property in dictionary as key (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packDictionary (packProperty Nothing "" packNil)) $ C.pack [dictionaryByte, collectionEndByte],
    packTestCase "pack property in dictionary as value (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packDictionary (packNil >> packProperty Nothing "" packNil)) $ C.pack [dictionaryByte, nilByte, collectionEndByte],

    packTestCase "pack empty unclased object" failPackCatchers (
        packObject Nothing (pure ())
      ) $ C.pack [objectByte, collectionEndByte],
    packTestCase "pack empty local name clased object" failPackCatchers (
        packObject (Just (Nothing, "fixns")) (pure ())
      ) $ C.pack [
        objectByte, classNameByte, fixstrMask .|. 5, 102, 105, 120, 110, 115,
        collectionEndByte],
    packTestCase "pack empty qualified name clased object" failPackCatchers (
        packObject (Just (Just "fixns", "fixns")) (pure ())
      ) $ C.pack [
        objectByte, classNameByte, fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
        collectionEndByte],

    packTestCase "pack non-empty unclased object" failPackCatchers (
        packObject Nothing packNil
      ) $ C.pack [objectByte, nilByte, collectionEndByte],
    packTestCase "pack non-empty local name clased object" failPackCatchers (
        packObject (Just (Nothing, "fixns")) packNil
      ) $ C.pack [
        objectByte, classNameByte, fixstrMask .|. 5, 102, 105, 120, 110, 115,
          nilByte,
        collectionEndByte],
    packTestCase "pack non-empty qualified name clased object" failPackCatchers (
        packObject (Just (Just "fixns", "fixns")) packNil
      ) $ C.pack [
        objectByte, classNameByte, fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
          nilByte,
        collectionEndByte],

    packTestCase "pack object" failPackCatchers (
        packObject Nothing (
            packProperty (Just "fixns") "fixns"
              packNil >>
            packFalse
          )
      ) $ C.pack [
        objectByte,
          fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
          nilByte,
          nilByte,
          falseByte,
        collectionEndByte],
    packTestCase "pack object with implicit nil value" failPackCatchers (
        packObject Nothing (packProperty (Just "fixns") "fixns" packNil)
      ) $ C.pack [
        objectByte, fixnsMask .|. 5, 102, 105, 120, 110, 115, fixstrMask .|. 5, 102, 105, 120, 110, 115,
        collectionEndByte],
    packTestCase "pack object with auto nil property name" failPackCatchers (
        packObject Nothing packTrue
      ) $ C.pack [
        objectByte, nilByte, trueByte,
        collectionEndByte],
    packTestCase "pack object with auto nil property name and implicit nil value" failPackCatchers (
        packObject Nothing packNil
      ) $ C.pack [objectByte, nilByte, collectionEndByte] ]
