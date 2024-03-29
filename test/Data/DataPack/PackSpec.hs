{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Data.DataPack.PackSpec (
  packTests,
) where

import Prelude
import Data.Bits ( Bits((.|.)) )
import Data.DataPack.Pack (
  PackError (TooBig),
  Value,
  runPack,
  pkBin,
  pkDict,
  pkDouble,
  pkFalse,
  pkFloat,
  pkInt,
  pkLnStr,
  pkStr,
  pkTxt,
  pkTrue,
  pkUSeq,
  (~!),
  (~~),
  (|!),
  (|/),
  (~/),
  (~:))
import Data.ByteString.Lazy qualified as C
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Target (DataTarget)
import Data.DataPack (
  bin16Byte,
  bin32Byte,
  bin8Byte,
  classNameByte,
  collectionEndByte,
  dictionaryByte,
  doubleByte,
  falseByte,
  fixbinMask,
  fixintMask,
  fixnsMask,
  fixstrMask,
  floatByte,
  int16Byte,
  int32Byte,
  int64Byte,
  int8Byte,
  nilByte,
  objectByte,
  sequenceByte,
  str16Byte,
  str32Byte,
  str8Byte,
  trueByte,
  uint16Byte,
  uint32Byte,
  uint64Byte,
  uint8Byte)
import Control.Monad.Except (ExceptT)

packTestCase :: (Monad m, DataTarget ll C.ByteString (ExceptT e m), Eq e, Eq ll) =>
  Maybe (Value (ExceptT (PackError e, ll) m) ll)
  -> ll
  -> Either (PackError e, ll) ll
  -> m (Maybe
          (Either (PackError e, ll) ll,
          Either (PackError e, ll) ll))
packTestCase packInstructions target expected = do
  res <- runPack packInstructions target
  if expected == res
    then pure Nothing
    else pure $ Just (expected, res)

packTests :: Monad m => [(String, m (Maybe (Either (PackError (), C.ByteString) C.ByteString, Either (PackError (), C.ByteString) C.ByteString)))]
packTests =
  let str2Bin = encodeUtf8 . T.pack
      fixstr = "fixstr"
      fixbin = str2Bin fixstr
      fixlen = C.length fixbin
  in [
    ("nil" , packTestCase Nothing C.empty . Right $ C.pack [nilByte]),
    ("true", packTestCase pkTrue C.empty . Right $ C.pack [trueByte]),
    ("false", packTestCase pkFalse C.empty . Right $ C.pack [falseByte]),
    ("fixint 63", packTestCase (pkInt 63) C.empty . Right $ C.pack [63]),
    ("fixint -64", packTestCase (pkInt (-64)) C.empty . Right $ C.pack [fixintMask]),
    ("uint8", packTestCase (pkInt 255) C.empty . Right $ C.pack [uint8Byte, 0xff]),
    ("uint16", packTestCase (pkInt 65535) C.empty . Right $ C.pack [uint16Byte, 0xff, 0xff]),
    ("uint32", packTestCase (pkInt 4294967295) C.empty . Right $ C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff]),
    ("uint64", packTestCase (pkInt 18446744073709551615) C.empty . Right $ C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
    ("int8", packTestCase (pkInt (-128)) C.empty . Right $ C.pack [int8Byte, 0x80]),
    ("int16", packTestCase (pkInt (-32768)) C.empty . Right $ C.pack [int16Byte, 128, 0]),
    ("int32", packTestCase (pkInt (-2147483648)) C.empty . Right $ C.pack [int32Byte, 0x80, 0, 0, 0]),
    ("int64",
      packTestCase (pkInt (-9223372036854775808)) C.empty . Right $ C.pack [int64Byte, 128, 0, 0, 0, 0, 0, 0, 0]),
    ("float", packTestCase (pkFloat 2.625) C.empty . Right $ C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]),
    ("double", packTestCase
      (pkDouble (-2.625)) C.empty . Right $ C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("fixbin", packTestCase
      (pkBin $ C.pack [32]) C.empty . Right $ C.pack
        [fixbinMask .|. 0x1, 32]),
    ("bin8",
      let dat = C.replicate 32 32 in
      packTestCase (pkBin dat) C.empty . Right $ C.pack [bin8Byte, 0x20] <> dat),
    ("bin16",
      let dat = C.replicate 256 32 in
      packTestCase (pkBin dat) C.empty . Right $ C.pack [bin16Byte, 0x1, 0] <> dat),
    ("bin32",
      let dat = C.replicate 65536 32 in
      packTestCase (pkBin dat) C.empty . Right $ C.pack [bin32Byte, 0x00, 0x1, 0x00, 0x00] <> dat),
    ("bin too big (negative test)",
      let dat = C.replicate (32768 * 65536) 32
          tooDat = C.concat [dat, dat]
      in
      packTestCase (pkBin tooDat) C.empty $ Left (TooBig tooDat, C.empty)),
    ("fixstr from Text", packTestCase (pkTxt $ T.pack " ") C.empty . Right $ C.pack [fixstrMask .|. 0x1, 32]),
    ("fixstr from String", packTestCase (pkStr " ") C.empty . Right $ C.pack [fixstrMask .|. 0x1, 32]),
    ("str8 from Text",
      packTestCase (pkTxt $ T.replicate 32 " ") C.empty . Right $ C.pack [str8Byte, 32] <> C.replicate 32 32),
    ("str8 from String",
      packTestCase (pkStr $ replicate 32 ' ') C.empty . Right $ C.pack [str8Byte, 32] <> C.replicate 32 32),
    ("str16 from Text",
      packTestCase (pkTxt $ T.replicate 256 " ") C.empty . Right $ C.pack [str16Byte, 0x1, 0] <> C.replicate 256 32),
    ("str16 from String",
      packTestCase (pkStr $ replicate 256 ' ') C.empty . Right $ C.pack [str16Byte, 0x1, 0] <> C.replicate 256 32),
    ("str32 from Text",
      packTestCase (pkTxt $ T.replicate 65536 " ") C.empty . Right $ C.pack [str32Byte, 0, 0x1, 0x00, 0x00] <> C.replicate 65536 32),
    ("str32 from String",
      packTestCase (pkStr $ replicate 65536 ' ') C.empty . Right $ C.pack [str32Byte, 0, 0x1, 0x00, 0x00] <> C.replicate 65536 32),
    ("empty sequence", packTestCase (pkUSeq []) C.empty . Right $ C.pack [sequenceByte, collectionEndByte]),
    ("empty sequence with local class name",
      packTestCase (fixstr ~! []) C.empty . Right $ C.concat [
        C.pack [sequenceByte, classNameByte, fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]]),
    ("empty sequence with namespaced class name", packTestCase (fixstr ~~ fixstr |! []) C.empty . Right $ C.concat
        [ C.pack
            [sequenceByte, classNameByte, fixnsMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [fixstrMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [collectionEndByte]
        ]
      ),
    ("sequence", packTestCase (pkUSeq [Nothing, pkTrue, pkFalse, pkStr fixstr])
      C.empty . Right $ C.concat [
          C.pack [ sequenceByte, nilByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ] ),
    ("empty dictionary", packTestCase (pkDict [])
      C.empty . Right $ C.pack [ dictionaryByte, collectionEndByte ] ),
    ("dictionary", packTestCase (pkDict [(Nothing, pkTrue), (pkFalse, pkStr fixstr)])
      C.empty . Right $ C.concat [
        C.pack [ dictionaryByte, nilByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ] ),
    ("dictionary with implied nil entry value", packTestCase (pkDict [(pkTrue, pkFalse), (pkStr fixstr, Nothing)])
      C.empty . Right $ C.concat [
        C.pack [ dictionaryByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ] ),
    ("empty object with local class name", packTestCase (fixstr ~/ [])
      C.empty . Right $ C.concat [
        C.pack [
          objectByte, classNameByte,
          fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]]),
    ("empty object with namespaced class name", packTestCase (fixstr ~~ fixstr |/ [])
      C.empty . Right $ C.concat [
        C.pack [
          objectByte, classNameByte,
          fixnsMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]]),
    ("object with namespaced class name and implied nil property value", packTestCase
      (fixstr ~~ fixstr |/ [
        (Nothing, pkInt (-16)),
        (fixstr ~~ "", pkTrue),
        ("" ~~ fixstr, pkFalse),
        "" ~: Nothing])
      C.empty . Right $ C.concat [
        C.pack [objectByte, classNameByte, fixnsMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [nilByte, 0xf0, fixnsMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask, trueByte, fixnsMask],
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [falseByte, fixstrMask, collectionEndByte]])
    ]
