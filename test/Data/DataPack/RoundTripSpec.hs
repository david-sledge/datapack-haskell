{-# LANGUAGE OverloadedStrings, Rank2Types #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.DataPack.RoundTripSpec (
  -- unpackPackTests,
  packUnpackTests,
) where

import Prelude
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
import Data.DataPack.Unpack
    ( UnpackState(Root, Number, SequenceStart, Sequence, Dictionary,
                  ObjectStart, ClassName, LocalName, EntryValue, EarlyCollectionEnd,
                  Object),
      UnpackError(InvalidByte, UnusedByte, SourceError, FlogTheDeveloper),
      PackType(..),
      NumberType(NUInt),
      BitSize(Bit64, Bit8, Bit32),
      unwrap,
      unpack,
      runUnpack,
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
import Control.Monad.Except (ExceptT)
import Data.Source (DataSource, DataSourceError)
import Control.Monad.Reader (ReaderT)

packUnpackTestCase :: (
    Monad m,
    DataSource
      C.ByteString
      C.ByteString
      n
      (ExceptT
        e1
        (ReaderT
            (UnpackState n)
            (ExceptT (DataSourceError e1 C.ByteString C.ByteString n) m))),
    Eq e1) =>
  Maybe (Value (ExceptT (PackError e2, C.ByteString) m) C.ByteString) ->
  [(Either (UnpackError e1 n) PackType, (UnpackState n, C.ByteString, C.ByteString))] ->
  m (Maybe
          (Either
              (PackError e2, C.ByteString)
              ((Either (UnpackError e1 n) PackType,
                (UnpackState n, C.ByteString, C.ByteString)),
              (Either (UnpackError e1 n) PackType,
                (UnpackState n, C.ByteString, C.ByteString)))))
packUnpackTestCase packInstructions expectStack = do
  mTarget <- runPack packInstructions C.empty
  case mTarget of
    Left e -> pure . Just $ Left e
    Right target ->
      let f n src wSt buff expectStk = do
            case expectStk of
              expectRes : stack -> do
                (res, (wSt', src', buff')) <- runUnpack (wSt, src, buff)
                let st' = unwrap wSt'
                if expectRes == (res, (st', src', buff'))
                then f (n + 1) src' wSt' buff' stack
                else pure . Just $ Right (expectRes, (res, (st', src', buff')))
              _ -> pure Nothing
      in
      f 0 target wrapRoot C.empty expectStack

packUnpackTests :: (DataSource
                          C.ByteString
                          C.ByteString
                          n
                          (ExceptT
                             eu
                             (ReaderT
                                (UnpackState n)
                                (ExceptT (DataSourceError eu C.ByteString C.ByteString n) m))), Monad m, Eq eu, Eq ep) => [(String,
                             m
                               (Maybe
                                  (Either
                                     (PackError ep, C.ByteString)
                                     ((Either (UnpackError eu n) PackType,
                                       (UnpackState n, C.ByteString, C.ByteString)),
                                      (Either (UnpackError eu n) PackType,
                                       (UnpackState n, C.ByteString, C.ByteString))))))]
packUnpackTests =
  let str2Bin = encodeUtf8 . T.pack
      fixstr = "fixstr"
      fixbin = str2Bin fixstr
      fixlen = C.length fixbin
  in [
    ("nil", packUnpackTestCase Nothing [(Right PNil, (Root, C.empty, C.empty))]),
    ("true", packUnpackTestCase pkTrue [(Right $ PBoolean True, (Root, C.empty, C.empty))]),
    ("false", packUnpackTestCase pkFalse [(Right $ PBoolean False, (Root, C.empty, C.empty))]),
    ("fixint 63", packUnpackTestCase (pkInt 63) [(Right $ PInt 63, (Root, C.empty, C.empty))]),
    ("fixint -64", packUnpackTestCase (pkInt (-64)) [(Right $ PInt (-64), (Root, C.empty, C.empty))]),
    ("uint8", packUnpackTestCase (pkInt 255) [(Right $ PUInt 255, (Root, C.empty, C.empty))]),
    ("uint16", packUnpackTestCase (pkInt 65535) [(Right $ PUInt 65535, (Root, C.empty, C.empty))]),
    ("uint32", packUnpackTestCase (pkInt 4294967295) [(Right $ PUInt 4294967295, (Root, C.empty, C.empty))]),
    ("uint64",
      packUnpackTestCase (pkInt 18446744073709551615)
      [(Right $ PUInt 18446744073709551615, (Root, C.empty, C.empty))]),
    ("int8", packUnpackTestCase (pkInt (-128)) [(Right $ PInt (-128), (Root, C.empty, C.empty))]),
    ("int16", packUnpackTestCase (pkInt (-32768)) [(Right $ PInt (-32768), (Root, C.empty, C.empty))]),
    ("int32", packUnpackTestCase (pkInt (-2147483648)) [(Right $ PInt (-2147483648), (Root, C.empty, C.empty))]),
    ("int64",
      packUnpackTestCase (pkInt (-9223372036854775808)) [(Right $ PInt (-9223372036854775808), (Root, C.empty, C.empty))]),
    ("float", packUnpackTestCase (pkFloat 2.625) [(Right $ PFloat 2.625, (Root, C.empty, C.empty))]),
    ("double", packUnpackTestCase
      (pkDouble (-2.625)) [(Right $ PDouble (-2.625), (Root, C.empty, C.empty))]),
    ("fixbin", packUnpackTestCase
      (pkBin $ C.pack [32]) [(Right . PBin $ C.pack [0x20], (Root, C.empty, C.empty))]),
    ("bin8",
      let dat = C.replicate 32 32 in
      packUnpackTestCase (pkBin dat) [(Right $ PBin dat, (Root, C.empty, C.empty))]),
    ("bin16",
      let dat = C.replicate 256 32 in
      packUnpackTestCase
        (pkBin dat) [(Right $ PBin dat, (Root, C.empty, C.empty))]),
    ("bin32",
      let dat = C.replicate 65536 32 in
      packUnpackTestCase
        (pkBin dat) [(Right $ PBin dat, (Root, C.empty, C.empty))]),
    (fixstr,
      let txt = T.pack " " in
      packUnpackTestCase
        (pkTxt txt)
        [(Right $ PStr txt, (Root, C.empty, C.empty))]){-,
    let str = "str8"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, packUnpackTestCase
      (C.concat [
        C.pack [str8Byte, fromIntegral len], bStr,
        C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      ]) [(Right $ PStr tStr, (Root, C.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00], C.empty))]),
    let str = "str16"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, packUnpackTestCase
      (C.concat [C.pack [str16Byte, 0, fromIntegral len], bStr])
      [(Right $ PStr tStr, (Root, C.empty, C.empty))]),
    let str = "str32"
        tStr = T.pack str
        bStr = encodeUtf8 tStr
        len = C.length bStr
    in
    (str, packUnpackTestCase
      (C.concat [C.pack [str32Byte, 0, 0, 0, fromIntegral len], bStr])
      [(Right $ PStr tStr, (Root, C.empty, C.empty))]),
    ("out of context namespace name (negative test)", packUnpackTestCase
      (C.pack [fixnsMask])
      [(Left . InvalidByte fixnsMask $ validBytesForState Root, (Root, C.empty, C.empty))]),
    ("class name outside of sequences and objects (negative test)", packUnpackTestCase
      (C.pack [classNameByte])
      [(Left . InvalidByte classNameByte $ validBytesForState Root, (Root, C.empty, C.empty))]),
    ("empty sequence", packUnpackTestCase (C.pack [sequenceByte, collectionEndByte])
      [(Right PSequence, (SequenceStart Root, C.pack [collectionEndByte], C.empty)), (Right PCollectionEnd, (Root, C.empty, C.empty))]),
    ("empty sequence with local class name", packUnpackTestCase (
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
    ("empty sequence with namespaced class name", packUnpackTestCase
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
    ("sequence", packUnpackTestCase (
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
    ("empty dictionary", packUnpackTestCase (
        C.pack [ dictionaryByte, collectionEndByte ] )
      [
        (Right PDictionary, (Dictionary Root, C.pack [ collectionEndByte ], C.empty)),
        (Right PCollectionEnd, (Root, C.empty, C.empty))
      ]),
    ("dictionary", packUnpackTestCase (
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
    ("dictionary with implied nil entry value", packUnpackTestCase (
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
    ("empty object with local class name", packUnpackTestCase (
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
    ("empty object with namespaced class name", packUnpackTestCase (
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
    ("object with namespaced class name", packUnpackTestCase (
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
--}
      ]
--}