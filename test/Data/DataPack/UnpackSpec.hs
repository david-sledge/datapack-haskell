{-# LANGUAGE FlexibleContexts, OverloadedStrings, Rank2Types #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Data.DataPack.UnpackSpec (
  unpackTests,
) where

import Prelude
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Source ( DataSource, DataSourceError )
import Data.Bits ( Bits((.|.)) )
import Data.DataPack.Unpack
    ( UnpackState(Root, Number, SequenceStart, Sequence, Dictionary,
                  ObjectStart, ClassName, LocalName, EntryValue, EarlyCollectionEnd,
                  Object),
      UnpackError(InvalidByte, NeedMoreData, UnusedByte, SourceError),
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
import Data.Int (Int64)

unpackTestCase :: (
    Monad m, DataSource b C.ByteString n (ExceptT e (
      ReaderT (UnpackState n) (ExceptT (DataSourceError e b C.ByteString n) m))),
    Eq e, Eq b) =>
  b ->
  [(Either (UnpackError e n) PackType, (UnpackState n, b, C.ByteString))] ->
  m (Maybe ((Either (UnpackError e n) PackType, (UnpackState n, b, C.ByteString)),
            (Either (UnpackError e n) PackType, (UnpackState n, b, C.ByteString))))
unpackTestCase bytString expectStack =
  let f n src wSt buff expectStk = do
        case expectStk of
          expectRes : stack -> do
            (res, (wSt', src', buff')) <- unpackNext (wSt, src, buff)
            let st' = unwrap wSt'
            if expectRes == (res, (st', src', buff'))
            then f (n + 1) src' wSt' buff' stack
            else pure $ Just (expectRes, (res, (st', src', buff')))
          _ -> pure Nothing
  in
  f 0 bytString wrapRoot C.empty expectStack

unpackTests :: Monad m =>
  [(String, m (Maybe
          ((Either (UnpackError () Int64) PackType,
            (UnpackState Int64, C.ByteString, C.ByteString)),
          (Either (UnpackError () Int64) PackType,
            (UnpackState Int64, C.ByteString, C.ByteString)))))]
unpackTests =
  let str2Bin = encodeUtf8 . T.pack
      fixstr = "fixstr"
      fixbin = str2Bin fixstr
      fixlen = C.length fixbin
  in [
    ("nil" , unpackTestCase (C.pack [nilByte]) [(Right PNil, (Root, C.empty, C.empty))]),
    ("empty content (negative test)", unpackTestCase (C.pack []) [(Left $ NeedMoreData 1, (Root, C.empty, C.empty))]),
    ("single nil with more data", unpackTestCase (C.pack [nilByte, dictionaryByte]) [(Right PNil, (Root, C.pack [dictionaryByte], C.empty))]),
    ("unused (negative test)", unpackTestCase (C.pack [objectByte + 1, nilByte]) [(Left . UnusedByte $ objectByte + 1, (Root, C.pack [nilByte], C.empty))]),
    ("true", unpackTestCase (C.pack [trueByte]) [(Right $ PBoolean True, (Root, C.empty, C.empty))]),
    ("false", unpackTestCase (C.pack [falseByte, dictionaryByte]) [(Right $ PBoolean False, (Root, C.pack [dictionaryByte], C.empty))]),
    (
      "collection end without matching start (negative test)",
      unpackTestCase
        (C.pack [collectionEndByte])
        [(Left . InvalidByte collectionEndByte $ validBytesForState Root, (Root, C.empty, C.empty))]),
    ("fixint 63", unpackTestCase (C.pack [63]) [(Right $ PInt 63, (Root, C.empty, C.empty))]),
    ("fixint -64", unpackTestCase (C.pack [fixintMask]) [(Right $ PInt (-64), (Root, C.empty, C.empty))]),
    ("uint8 without data (negative test)", unpackTestCase (C.pack [uint8Byte])
      [(Left $ NeedMoreData 1, (Number (NUInt Bit8) Root, C.empty, C.empty))]),
    ("uint8", unpackTestCase (C.pack [uint8Byte, 0xff]) [(Right $ PUInt 255, (Root, C.empty, C.empty))]),
    ("uint16", unpackTestCase (C.pack [uint16Byte, 0xff, 0xff]) [(Right $ PUInt 65535, (Root, C.empty, C.empty))]),
    ("uint32", unpackTestCase (C.pack [uint32Byte, 0xff, 0xff, 0xff, 0xff]) [(Right $ PUInt 4294967295, (Root, C.empty, C.empty))]),
    ("uint32 missing one byte (negative test)",
      unpackTestCase (C.pack [uint32Byte, 0xff, 0xff, 0xff]) [(Left $ NeedMoreData 1, (Number (NUInt Bit32) Root, C.empty, C.pack [0xff, 0xff, 0xff]))]),
    ("uint64",
      unpackTestCase (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])
      [(Right $ PUInt 18446744073709551615, (Root, C.empty, C.empty))]),
    ("uint64 missing three bytes (negative test)",
      unpackTestCase (C.pack [uint64Byte, 0xff, 0xff, 0xff, 0xff, 0xff])
      [(Left $ NeedMoreData 3, (Number (NUInt Bit64) Root, C.empty, C.pack [0xff, 0xff, 0xff, 0xff, 0xff]))]),
    ("int8", unpackTestCase (C.pack [int8Byte, 0xff]) [(Right $ PInt (-1), (Root, C.empty, C.empty))]),
    ("int16", unpackTestCase (C.pack [int16Byte, 0xff, 0xff]) [(Right $ PInt (-1), (Root, C.empty, C.empty))]),
    ("int32", unpackTestCase (C.pack [int32Byte, 0xff, 0xff, 0xff, 0xff]) [(Right $ PInt (-1), (Root, C.empty, C.empty))]),
    ("int64",
      unpackTestCase (C.pack [int64Byte, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) [(Right $ PInt (-1), (Root, C.empty, C.empty))]),
    ("float", unpackTestCase (C.pack [floatByte, 0x40, 0x28, 0x00, 0x00]) [(Right $ PFloat 2.625, (Root, C.empty, C.empty))]),
    ("double", unpackTestCase
      (C.pack [doubleByte, 0xc0, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right $ PDouble (-2.625), (Root, C.empty, C.empty))]),
    ("fixbin", unpackTestCase
      (C.pack
        [fixbinMask .|. 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], (Root, C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], C.empty))]),
    ("bin8", unpackTestCase
      (C.pack [bin8Byte, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], (Root, C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00], C.empty))]),
    ("bin16", unpackTestCase
      (C.pack [bin16Byte, 0, 0x1, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], (Root, C.pack [0x00, 0x00, 0x00, 0x00, 0x00], C.empty))]),
    ("bin32", unpackTestCase
      (C.pack [bin32Byte, 0x00, 0x00, 0, 0x1, 0x05, 0x00, 0x00, 0x00]) [(Right . PBin $ C.pack [0x05], (Root, C.pack [0x00, 0x00, 0x00], C.empty))]),
    (fixstr, unpackTestCase
      (C.concat [C.pack [fixstrMask .|. fromIntegral fixlen], fixbin])
      [(Right . PStr $ T.pack fixstr, (Root, C.empty, C.empty))]),
    let str = "str8"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, unpackTestCase
      (C.concat [
        C.pack [str8Byte, fromIntegral len], bStr,
        C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      ]) [(Right $ PStr tStr, (Root, C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00], C.empty))]),
    let str = "str16"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, unpackTestCase
      (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr])
      [(Right $ PStr tStr, (Root, C.empty, C.empty))]),
    let str = "str32"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, unpackTestCase
      (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr])
      [(Right $ PStr tStr, (Root, C.empty, C.empty))]),
    ("out of context namespace name (negative test)", unpackTestCase
      (C.pack [fixnsMask])
      [(Left . InvalidByte fixnsMask $ validBytesForState Root, (Root, C.empty, C.empty))]),
    ("class name outside of sequences and objects (negative test)", unpackTestCase
      (C.pack [classNameByte])
      [(Left . InvalidByte classNameByte $ validBytesForState Root, (Root, C.empty, C.empty))]),
    ("empty sequence", unpackTestCase (C.pack [sequenceByte, collectionEndByte])
      [(Right PSequence, (SequenceStart Root, C.pack [collectionEndByte], C.empty)), (Right PCollectionEnd, (Root, C.empty, C.empty))]),
    ("empty sequence with local class name", unpackTestCase (
        C.concat [
          C.pack [sequenceByte, classNameByte,
            fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]])
      [ (Right PSequence, (SequenceStart Root, C.concat [
            C.pack [classNameByte, fixstrMask .|. fromIntegral fixlen], fixbin,
            C.pack [collectionEndByte]],
          C.empty)),
        (Right PClassName, (ClassName $ Sequence Root, C.concat [
            C.pack [fixstrMask .|. fromIntegral fixlen],
            fixbin,
            C.pack [collectionEndByte]],
          C.empty)),
        (Right . PStr $ T.pack fixstr, (Sequence Root,
          C.pack [collectionEndByte], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
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
      [ (Right PSequence, (SequenceStart Root, C.concat
        [ C.pack
            [classNameByte, fixnsMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [fixstrMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [collectionEndByte]
        ], C.empty))
      , (Right PClassName, (ClassName $ Sequence Root, C.concat
        [ C.pack
            [fixnsMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [fixstrMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [collectionEndByte]
        ], C.empty))
      , (Right . PNs $ T.pack fixstr, (LocalName . ClassName $ Sequence Root, C.concat
        [ C.pack [fixstrMask .|. fromIntegral fixlen]
        , fixbin
        , C.pack [collectionEndByte]
        ], C.empty))
      , (Right . PStr $ T.pack fixstr, (Sequence Root, C.pack [collectionEndByte], C.empty))
      , (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
    ("sequence", unpackTestCase (
        C.concat [
          C.pack [ sequenceByte, nilByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ] )
      [
        (Right PSequence, (SequenceStart Root, C.concat [
          C.pack [ nilByte, trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right PNil, (Sequence Root, C.concat [
          C.pack [ trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right $ PBoolean True, (Sequence Root, C.concat [
          C.pack [ falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right $ PBoolean False, (Sequence Root, C.concat [
          C.pack [ fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right . PStr $ T.pack fixstr, (Sequence Root,
          C.pack [ collectionEndByte ], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
    ("empty dictionary", unpackTestCase (
        C.pack [ dictionaryByte, collectionEndByte ] )
      [
        (Right PDictionary, (Dictionary Root, C.pack [ collectionEndByte ], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
    ("dictionary", unpackTestCase (
      C.concat [
        C.pack [ dictionaryByte, nilByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ] )
      [
        (Right PDictionary, (Dictionary Root, C.concat [
        C.pack [ nilByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ], C.empty)),
        (Right PNil, (EntryValue $ Dictionary Root, C.concat [
        C.pack [ trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ], C.empty)),
        (Right $ PBoolean True, (Dictionary Root, C.concat [
        C.pack [ falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ], C.empty)),
        (Right $ PBoolean False, (EntryValue $ Dictionary Root, C.concat [
        C.pack [ fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ], C.empty)),
        (Right . PStr $ T.pack fixstr, (Dictionary Root,
        C.pack [ collectionEndByte ], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
    ("dictionary with implied nil entry value", unpackTestCase (
      C.concat [
        C.pack [ dictionaryByte, trueByte, falseByte,
            fixstrMask .|. fromIntegral fixlen ],
        fixbin,
        C.pack [ collectionEndByte ] ] )
      [
        (Right PDictionary, (Dictionary Root, C.concat [
          C.pack [ trueByte, falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right $ PBoolean True, (EntryValue $ Dictionary Root, C.concat [
          C.pack [ falseByte,
              fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right $ PBoolean False, (Dictionary Root, C.concat [
          C.pack [ fixstrMask .|. fromIntegral fixlen ],
          fixbin,
          C.pack [ collectionEndByte ] ], C.empty)),
        (Right . PStr $ T.pack fixstr, (EntryValue $ Dictionary Root, C.pack [ collectionEndByte ], C.empty)),
        (Right PNil, (EarlyCollectionEnd $ Dictionary Root, C.empty, C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
    ("empty object with local class name", unpackTestCase (
      C.concat [
        C.pack [
          objectByte, classNameByte,
          fixstrMask .|. fromIntegral fixlen],
        fixbin,
        C.pack [collectionEndByte]])
      [
        (Right PObject, (ObjectStart Root, C.concat [
          C.pack [
            classNameByte,
            fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]], C.empty)),
        (Right PClassName, (ClassName $ Object Root, C.concat [
          C.pack [
            fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]], C.empty)),
        (Right . PStr $ T.pack fixstr, (Object Root,
          C.pack [collectionEndByte], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
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
        (Right PObject, (ObjectStart Root, C.concat [
          C.pack [
            classNameByte,
            fixnsMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]], C.empty)),
        (Right PClassName, (ClassName $ Object Root, C.concat [
          C.pack [
            fixnsMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]], C.empty)),
        (Right . PNs $ T.pack fixstr, (LocalName . ClassName $ Object Root, C.concat [
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [collectionEndByte]], C.empty)),
        (Right . PStr $ T.pack fixstr, (Object Root, C.pack [collectionEndByte], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
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
        (Right PObject, (ObjectStart Root, C.concat [
          C.pack [classNameByte, ns8Byte, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        (Right PClassName, (ClassName $ Object Root, C.concat [
          C.pack [ns8Byte, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- namespace name - "fixstr":"fixstr"
        (Right . PNs $ T.pack fixstr, (LocalName . ClassName $ Object Root, C.concat [
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        (Right . PStr $ T.pack fixstr, (Object Root, C.concat [
          C.pack [nilByte, 0xf0, ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- Nil property name
        (Right PNil, (EntryValue $ Object Root, C.concat [
          C.pack [0xf0, ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- property value
        (Right $ PInt (-16), (Object Root, C.concat [
          C.pack [ns16Byte, 0, fromIntegral fixlen],
          fixbin,
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- property name: namespace name - "fixstr"
        (Right . PNs $ T.pack fixstr, (LocalName $ Object Root, C.concat [
          C.pack [fixstrMask, trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- property name: local name - ""
        (Right . PStr $ T.pack "", (EntryValue $ Object Root, C.concat [
          C.pack [trueByte, ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- property value
        (Right $ PBoolean True, (Object Root, C.concat [
          C.pack [ns32Byte, 0, 0, 0, 0],
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- property name: namespace name - ""
        (Right . PNs $ T.pack "", (LocalName $ Object Root, C.concat [
          C.pack [fixstrMask .|. fromIntegral fixlen],
          fixbin,
          C.pack [falseByte, fixstrMask, collectionEndByte]], C.empty)),
        -- property name: local name - "fixstr"
        (Right . PStr $ T.pack fixstr, (EntryValue $ Object Root,
          C.pack [falseByte, fixstrMask, collectionEndByte], C.empty)),
        -- property value
        (Right $ PBoolean False, (Object Root,
          C.pack [fixstrMask, collectionEndByte], C.empty)),
        -- property name: local name
        (Right . PStr $ T.pack "", (EntryValue $ Object Root,
          C.pack [collectionEndByte], C.empty)),
        -- property value
        (Right PNil, (EarlyCollectionEnd $ Object Root,
          C.empty, C.empty)),
        (Right PCollectionEnd, (Root,
          C.empty, C.empty))
      ]){-
--}
    ]
