{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TupleSections,
    OverloadedStrings, Rank2Types #-}

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

import qualified UnpackSpec as S

runTests :: (Foldable t) =>
     t (Identity (String, MaybeCont String String))
     -> IO ()
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

instance DataDestination C.ByteString () where
    give bstr d _ good = good $ C.concat [d, bstr]

packFailMessage p msg = Just $ \f -> f "Failed" ++ ": with state of "
  ++ show p ++ "\x2014" ++ msg

packDestinationFailMessage p msg = pure (Just $ \f -> f "Failed"
  ++ ": with state of " ++ show p ++ "\x2014" ++ msg)

-- showValues d = case C.uncons d of
--   Just (word, sTail) -> showHex word $ ':':showValues sTail
--   _ -> []

-- failPackCatchers :: (Show a0) => PackCatchers
--                               a0 C.ByteString u (Maybe (([Char] -> [Char]) -> [Char]))
-- failPackCatchers = PackCatchers {
--     P.stream = \e _ p _ _ -> packDestinationFailMessage p $ "stream fail " ++ show e,
--     tooBig = \dat p _ _ -> packDestinationFailMessage p $ "too big " ++ show (C.length dat),
--     invalidPackType = \p _ _ -> packDestinationFailMessage p "invalid pack type",
--     P.flogTheDeveloper = \p d -> packDestinationFailMessage p ("Superfail! Flog the developer! " ++ showValues d)
--   }
--
-- packTestCase testName catchers pack expected = (testName, ) <$>
--   packDataT C.empty catchers pack
--     (\p d' _ -> packDestinationFailMessage p
--         ("Invalid exit state of " ++ show p ++ "! " ++ showValues d'))
--     (\d -> pure $ if d == expected
--       then Nothing
--       else packFailMessage P.Root ("expected " ++ showValues expected
--           ++ ". Found " ++ showValues d))
--
-- packIntTestCase n = packTestCase ("pack " ++ show n) failPackCatchers (packInt n)

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
  runTests $ S.unpackTests ++ [
      {-,

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

      packTestCase "pack fixstr" failPackCatchers (packString fixstr) $ C.cons (fixstrMask .|. 6) fixbin,
      packTestCase "pack str8" failPackCatchers (packString str8) . C.cons str8Byte $ C.cons (fromIntegral $ C.length bin8) bin8,
      packTestCase "pack str8 again" failPackCatchers (packString str8again) . C.cons str8Byte $ C.cons (fromIntegral $ C.length bin8again) bin8again,
      packTestCase "pack str16" failPackCatchers (packString testString) . C.cons str16Byte $ C.cons (fromIntegral $ shiftR (C.length bin16) 8) (C.cons (fromIntegral $ C.length bin16) bin16),
      packTestCase "pack str32" failPackCatchers (packString . T.unpack . decodeUtf8 $ bin32) $ C.cons str32Byte (
          C.cons (fromIntegral $ shiftR len32 24) (
            C.cons (fromIntegral $ shiftR len32 16) (
              C.cons (fromIntegral $ shiftR len32 8) (
                C.cons (fromIntegral len32) bin32 ) ) ) ),

      packTestCase "pack fixbin" failPackCatchers (packBin fixbin) $ C.cons (fixbinMask .|. 6) fixbin,
      packTestCase "pack bin8" failPackCatchers (packBin bin8) . C.cons bin8Byte $ C.cons (fromIntegral $ C.length bin8) bin8,
      packTestCase "pack bin8 again" failPackCatchers (packBin bin8again) . C.cons bin8Byte $ C.cons (fromIntegral $ C.length bin8again) bin8again,
      packTestCase "pack bin16" failPackCatchers (packBin bin16) . C.cons bin16Byte $ C.cons (fromIntegral $ shiftR (C.length bin16) 8) (C.cons (fromIntegral $ C.length bin16) bin16),
      packTestCase "pack bin32" failPackCatchers (packBin bin32) $ C.cons bin32Byte (
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

      packTestCase "pack property as root (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packProperty Nothing "" packNil) $ C.pack [nilByte],
      packTestCase "pack property in sequence (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packSequence Nothing (packProperty Nothing "" packNil)) $ C.pack [sequenceByte, nilByte, collectionEndByte],
      packTestCase "pack property in dictionary as key (negative test)" failPackCatchers {invalidPackType = \p d f -> f d} (packDictionary (packProperty Nothing "" packNil)) $ C.pack [dictionaryByte, nilByte, collectionEndByte],
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
        ) $ C.pack [objectByte, nilByte, collectionEndByte],

      pure ("", Nothing)
--}
    ]

-- data WhereAreWe d e m r a = AwaitingValue
--   | AwaitingClassName (
--       Maybe (Maybe String, String)
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) a
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingNsOption (
--       Maybe String -> String
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingNsName (
--       String -> String
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingLocalName (
--       String
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingPropertyName (
--       Maybe String -> String
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingPropertyNsName (
--       String -> String
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingPropertyLocalName (
--       String
--       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--   | AwaitingPropertyValue
--   | AwaitingPropertyValueAfterNilName
--packSequenceStart
-- \nsName localName -> packSequenceStart (Just (nsName, localName))
-- \name localName -> packSequenceStart (Just (Just name, localName))
-- \localName -> packSequenceStart (Just (Nothing, localName))

-- packTheUnpacked unpackCatchers s =
--   unpackDataT s unpackCatchers Callbacks {
--       nil = \st s' f -> get >>= \w ->
--         let keepGoing = f s' in
--         case w of
--           AwaitingValue -> packNil >> keepGoing
--           -- AwaitingClassName g ->
--           --   | AwaitingNsOption (
--           --       Maybe String -> String
--           --       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--           --   | AwaitingNsName (
--           --       String -> String
--           --       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--           --   | AwaitingLocalName (
--           --       String
--           --       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--           --   | AwaitingPropertyName (
--           --       Maybe String -> String
--           --       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--           --   | AwaitingPropertyNsName (
--           --       String -> String
--           --       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--           --   | AwaitingPropertyLocalName (
--           --       String
--           --       -> StateT (P.States d) (ReaderT (PackCatchers e d (m r)) (ContT r m)) () )
--           --   | AwaitingPropertyValue
--           --   | AwaitingPropertyValueAfterNilName
--         ,
--       collectionEnd = \st s' f -> packCollectionEnd >> f s',
--       boolean = \b st s' f -> (if b then packTrue else packFalse) >> f s',
--       uint64 = \w st s' f -> packInt w >> f s',
--       int64 = \i st s' f -> packInt i >> f s',
--       float = \fl st s' f -> packFloat fl >> f s',
--       double = \d st s' f -> packDouble d >> f s',
--       binStart = \len st s' f -> f s',
--       strStart = \len st s' f -> f s',
--       nsStart = \len st s' f -> f s',
--       dat = \bytStr st s' f -> f s',
--       className = \st s' f -> f s',
--       sequenceD = \st s' f -> f s',
--       dictionary = \st s' f -> packDictionaryStart >> f s',
--       object = \st s' f -> f s'
--     } $ \s -> let len = C.length $ byteString s in
--     pure (s, if len == 0
--       then Nothing
--       else unpackFailMessage U.Root (pos s) $ "Expected no remaining bytes in\
--         \ byteString. Found " ++ show len ++ " byte(s).")

-- unpackThePacked packCatchers unpackCatchers callbacks pack =
--   packDataT C.empty packCatchers pack $ \d ->
--     unpackDataT (ByteStringPos d 0) unpackCatchers callbacks $ \s ->
--       let len = C.length $ byteString s in
--       pure (s, if len == 0
--         then Nothing
--         else unpackFailMessage U.Root (pos s) $ "Expected no remaining bytes in\
--           \ byteString. Found " ++ show len ++ " byte(s).")
