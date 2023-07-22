{-# LANGUAGE OverloadedStrings, Rank2Types #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Data.DataPack.RoundTripSpec (
  UnpackTestResult(..),
  unpackTests,
) where

import Prelude
import Data.ListLike (genericLength)
import Data.Bits ( Bits((.|.)) )
import Data.Int (Int64)
import Data.DataPack.Unpack
    ( UnpackState(Root, Number, SequenceStart, Sequence, Dictionary,
                  ObjectStart, ClassName, LocalName, EntryValue, EarlyCollectionEnd,
                  Object),
      UnpackError(InvalidByte, UnusedByte, SourceError),
      PackType(..),
      NumberType(NUInt),
      BitSize(Bit64, Bit8, Bit32),
      unwrap,
      unpackNext,
      wrapRoot,
      nilByte,
      dictionaryByte,
      objectByte,
      trueByte,
      falseByte,
      collectionEndByte,
      validBytesForState,
      fixintMask,
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
      fixbinMask,
      bin8Byte,
      bin16Byte,
      bin32Byte,
      fixstrMask,
      str8Byte,
      str16Byte,
      str32Byte,
      fixnsMask,
      classNameByte,
      sequenceByte,
      ns16Byte,
      ns32Byte,
      ns8Byte )
import Data.ByteString.Lazy qualified as C
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding ( encodeUtf8 )

{-
data UnpackTestResult n ll p a =
  Success |
  ResultMismatch n ll a p p |
  BytesRemainMismatch n ll a a |
  StateMismatch n ll a (UnpackState a) (UnpackState a)
  deriving Show

unpackTestCase :: (Monad m, Num n, Eq e, Integral a) =>
  C.ByteString -> [(Either (UnpackError e a (SourcePos C.ByteString a)) PackType, UnpackState a)] ->
  a -> m (UnpackTestResult n C.ByteString (Either (UnpackError e a (SourcePos C.ByteString a)) PackType) a)
unpackTestCase bytString expectStack expectedBytesRemaining =
  let f n src wSt expectStk = do
        let st = unwrap wSt
        case expectStk of
          (expectRes, expectSt) : stack -> do
            (res, (src', wSt')) <- unpackNext src wSt
            let st' = unwrap wSt'
                posi = pos src'
            if expectRes == res
            then
              if expectSt == st'
              then f (n + 1) src' wSt' stack
              else pure $ StateMismatch n bytString posi expectSt st'
            else pure $ ResultMismatch n bytString posi expectRes res
          _ ->
            let len = genericLength $ dataSource src in
            if len == expectedBytesRemaining
            then pure Success
            else pure $ BytesRemainMismatch n bytString len expectedBytesRemaining
  in
  f 0 (SourcePos bytString 0) wrapRoot expectStack

unpackTests :: Monad m =>
  [(String, m (UnpackTestResult Int C.ByteString (Either (UnpackError () Int64 (SourcePos C.ByteString Int64)) PackType) Int64))]
unpackTests =
  let str2Bin = encodeUtf8 . T.pack
      fixstr = "fixstr"
      fixbin = str2Bin fixstr
      fixlen = C.length fixbin
  in [
    ("nil" , unpackTestCase (C.pack [nilByte]) [(Right PNil, Root)] 0),
    ("empty content (negative test)", unpackTestCase (C.pack []) [(Left . SourceError $ MoreData 1, Root)] 0),
    ("single nil with more data", unpackTestCase (C.pack [nilByte, dictionaryByte]) [(Right PNil, Root)] 1),
    ("unused (negative test)", unpackTestCase (C.pack [objectByte + 1, nilByte]) [(Left . UnusedByte $ objectByte + 1, Root)] 1),
    ("true", unpackTestCase (C.pack [trueByte]) [(Right $ PBoolean True, Root)] 0),
    ("false", unpackTestCase (C.pack [falseByte, dictionaryByte]) [(Right $ PBoolean False, Root)] 1),
    (
      "collection end without matching start (negative test)",
      unpackTestCase
        (C.pack [collectionEndByte])
        [(Left . InvalidByte collectionEndByte $ validBytesForState Root, Root)] 0),
    ("fixint 63", unpackTestCase (C.pack [63]) [(Right $ PInt 63, Root)] 0),
    ("fixint -64", unpackTestCase (C.pack [fixintMask]) [(Right $ PInt (-64), Root)] 0),
    ("uint8 without data (negative test)", unpackTestCase (C.pack [uint8Byte])
      [(Left . SourceError $ MoreData 1, Number (NUInt Bit8) Root)] 0),
    ("uint8", unpackTestCase (C.pack [uint8Byte, 0xff]) [(Right $ PUInt 255, Root)] 0),
    ("uint16", unpackTestCase (C.pack [uint16Byte, 0xff, 0xff]) [(Right $ PUInt 65535, Root)] 0),
    ("uint32", unpackTestCase (C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff]) [(Right $ PUInt 4294967295, Root)] 0),
    ("uint32 missing one byte (negative test)",
      unpackTestCase (C.pack [uint32Byte, 0xff, 0xff, 0xff]) [(Left . SourceError $ MoreData 1, Number (NUInt Bit32) Root)] 3),
    ("uint64",
      unpackTestCase (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])
      [(Right $ PUInt 18446744073709551615, Root)] 0),
    ("uint64 missing three bytes (negative test)",
      unpackTestCase (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff])
      [(Left . SourceError $ MoreData 3, Number (NUInt Bit64) Root)] 5),
    ("int8", unpackTestCase (C.pack [int8Byte, 0xff]) [(Right $ PInt (-1), Root)] 0),
    ("int16", unpackTestCase (C.pack [int16Byte, 0xff, 0xff]) [(Right $ PInt (-1), Root)] 0),
    ("int32", unpackTestCase (C.pack [int32Byte, 0xff, 0xff, 0xff, 0xff]) [(Right $ PInt (-1), Root)] 0),
    ("int64",
      unpackTestCase (C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) [(Right $ PInt (-1), Root)] 0),
    ("float", unpackTestCase (C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]) [(Right $ PFloat 2.625, Root)] 0),
    ("double", unpackTestCase
      (C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right $ PDouble (-2.625), Root)] 0),
    ("fixbin", unpackTestCase
      (C.pack
        [fixbinMask .|. 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], Root)] 7),
    ("bin8", unpackTestCase
      (C.pack [bin8Byte, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], Root)] 6),
    ("bin16", unpackTestCase
      (C.pack [bin16Byte, 0, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], Root)] 5),
    ("bin32", unpackTestCase
      (C.pack [bin32Byte, 0x00, 0x00, 0, 0x1, 0x05, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], Root)] 3),
    (fixstr, unpackTestCase
      (C.concat [C.pack [fixstrMask .|. fromIntegral fixlen], fixbin])
      [(Right . PStr $ T.pack fixstr, Root)] 0),
    let str = "str8"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, unpackTestCase
      (C.concat [
        C.pack [str8Byte, fromIntegral len], bStr,
        C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      ]) [(Right $ PStr tStr, Root)] 6),
    let str = "str16"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, unpackTestCase
      (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr])
      [(Right $ PStr tStr, Root)] 0),
    let str = "str32"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, unpackTestCase
      (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr])
      [(Right $ PStr tStr, Root)] 0),
    ("out of context namespace name (negative test)", unpackTestCase
      (C.pack [fixnsMask])
      [(Left . InvalidByte fixnsMask $ validBytesForState Root, Root)] 0),
    ("class name outside of sequences and objects (negative test)", unpackTestCase
      (C.pack [classNameByte])
      [(Left . InvalidByte classNameByte $ validBytesForState Root, Root)] 0),
    ("empty sequence", unpackTestCase (C.pack [sequenceByte, collectionEndByte])
      [(Right PSequence, SequenceStart Root), (Right PCollectionEnd, Root)] 0),
    ("empty sequence with local class name", unpackTestCase
      ( C.concat
          [ C.pack
              [sequenceByte, classNameByte, fixstrMask .|. fromIntegral fixlen]
          , fixbin
          , C.pack [collectionEndByte]
          ]
      )
      [ (Right PSequence, SequenceStart Root)
      , (Right PClassName, ClassName $ Sequence Root)
      , (Right . PStr $ T.pack fixstr, Sequence Root)
      , (Right PCollectionEnd, Root)
      ] 0),
    ("empty sequence with namespaced class name", unpackTestCase
      ( C.concat
        [ C.pack
            [sequenceByte, classNameByte, fixnsMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [fixstrMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [collectionEndByte]
        ]
      )
      [ (Right PSequence, SequenceStart Root)
      , (Right PClassName, ClassName $ Sequence Root)
      , (Right . PNs $ T.pack fixstr, LocalName . ClassName $ Sequence Root)
      , (Right . PStr $ T.pack fixstr, Sequence Root)
      , (Right PCollectionEnd, Root)
      ] 0),
    ("sequence", unpackTestCase (
        C.concat [
          C.pack [ sequenceByte, nilByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ] )
      [
        (Right PSequence, SequenceStart Root),
        (Right PNil, Sequence Root),
        (Right $ PBoolean True, Sequence Root),
        (Right $ PBoolean False, Sequence Root),
        (Right . PStr $ T.pack fixstr, Sequence Root),
        (Right PCollectionEnd, Root)
      ] 0),
    ("empty dictionary", unpackTestCase (
        C.pack [ dictionaryByte, collectionEndByte ] )
      [
        (Right PDictionary, Dictionary Root),
        (Right PCollectionEnd, Root)
      ] 0),
    ("dictionary", unpackTestCase (
      C.concat [
        C.pack [ dictionaryByte, nilByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ] )
      [
        (Right PDictionary, Dictionary Root),
        (Right PNil, EntryValue $ Dictionary Root),
        (Right $ PBoolean True, Dictionary Root),
        (Right $ PBoolean False, EntryValue $ Dictionary Root),
        (Right . PStr $ T.pack fixstr, Dictionary Root),
        (Right PCollectionEnd, Root)
      ] 0),
    ("dictionary with implied nil entry value", unpackTestCase (
      C.concat [
        C.pack [ dictionaryByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ] )
      [
        (Right PDictionary, Dictionary Root),
        (Right $ PBoolean True, EntryValue $ Dictionary Root),
        (Right $ PBoolean False, Dictionary Root),
        (Right . PStr $ T.pack fixstr, EntryValue $ Dictionary Root),
        (Right PNil, EarlyCollectionEnd $ Dictionary Root),
        (Right PCollectionEnd, Root)
      ] 0),
    ("empty object with local class name", unpackTestCase (
      C.concat [
        C.pack [
          objectByte, classNameByte,
          fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]])
      [
        (Right PObject, ObjectStart Root),
        (Right PClassName, ClassName $ Object Root),
        (Right . PStr $ T.pack fixstr, Object Root),
        (Right PCollectionEnd, Root)
      ] 0),
    ("empty object with namespaced class name", unpackTestCase (
      C.concat [
        C.pack [
          objectByte, classNameByte,
          fixnsMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]])
      [
        (Right PObject, ObjectStart Root),
        (Right PClassName, ClassName $ Object Root),
        (Right . PNs $ T.pack fixstr, LocalName . ClassName $ Object Root),
        (Right . PStr $ T.pack fixstr, Object Root),
        (Right PCollectionEnd, Root)
      ] 0),
    ("object with namespaced class name", unpackTestCase (
      C.concat [
        C.pack [objectByte, classNameByte, ns8Byte, fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral fixlen],
        fixbin,
        C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
        C.pack [fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [falseByte, fixstrMask, collectionEndByte]])
      [
        (Right PObject, ObjectStart Root),
        (Right PClassName, ClassName $ Object Root),
        -- namespace name - "fixstr":"fixstr"
        (Right . PNs $ T.pack fixstr, LocalName . ClassName $ Object Root),
        (Right . PStr $ T.pack fixstr, Object Root),
        -- Nil property name
        (Right PNil, EntryValue $ Object Root),
        -- property value
        (Right $ PInt (-16), Object Root),
        -- property name: namespace name - "fixstr"
        (Right . PNs $ T.pack fixstr, LocalName $ Object Root),
        -- property name: local name - ""
        (Right . PStr $ T.pack "", EntryValue $ Object Root),
        -- property value
        (Right $ PBoolean True, Object Root),
        -- property name: namespace name - ""
        (Right . PNs $ T.pack "", LocalName $ Object Root),
        -- property name: local name - "fixstr"
        (Right . PStr $ T.pack fixstr, EntryValue $ Object Root),
        -- property value
        (Right $ PBoolean False, Object Root),
        -- property name: local name
        (Right . PStr $ T.pack "", EntryValue $ Object Root),
        -- property value
        (Right PNil, EarlyCollectionEnd $ Object Root),
        (Right PCollectionEnd, Root)
      ] 0)
    ]
--}