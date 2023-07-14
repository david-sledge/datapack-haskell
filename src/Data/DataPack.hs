module Data.DataPack (
  fixintMask, nilByte, collectionEndByte, falseByte, trueByte, uint8Byte,
  uint16Byte, uint32Byte, uint64Byte, int8Byte, int16Byte, int32Byte, int64Byte,
  floatByte, doubleByte, bin8Byte, bin16Byte, bin32Byte, str8Byte, str16Byte,
  str32Byte, ns8Byte, ns16Byte, ns32Byte, classNameByte, sequenceByte,
  dictionaryByte, objectByte, fixbinMask, fixstrMask, fixnsMask, fixMask,
  lenMask,
) where

import Prelude ()
import Data.Word ( Word8 )

fixintMask, nilByte, collectionEndByte, falseByte, trueByte, uint8Byte,
  uint16Byte, uint32Byte, uint64Byte, int8Byte, int16Byte, int32Byte, int64Byte,
  floatByte, doubleByte, bin8Byte, bin16Byte, bin32Byte, str8Byte, str16Byte,
  str32Byte, ns8Byte, ns16Byte, ns32Byte, classNameByte, sequenceByte,
  dictionaryByte, objectByte, fixbinMask, fixstrMask, fixnsMask, fixMask,
  lenMask :: Word8
fixintMask        = 0xc0
nilByte           = 0x40
collectionEndByte = 0x41
falseByte         = 0x42
trueByte          = 0x43
uint8Byte         = 0x44
uint16Byte        = 0x45
uint32Byte        = 0x46
uint64Byte        = 0x47
int8Byte          = 0x48
int16Byte         = 0x49
int32Byte         = 0x4a
int64Byte         = 0x4b
floatByte         = 0x4c
doubleByte        = 0x4d
bin8Byte          = 0x4e
bin16Byte         = 0x4f
bin32Byte         = 0x50
str8Byte          = 0x51
str16Byte         = 0x52
str32Byte         = 0x53
ns8Byte           = 0x54
ns16Byte          = 0x55
ns32Byte          = 0x56
classNameByte     = 0x57
sequenceByte      = 0x58
dictionaryByte    = 0x59
objectByte        = 0x5a
--unusedBytes = [0x5b..0x5f]
fixbinMask        = 0x60
fixstrMask        = 0x80
fixnsMask         = 0xa0
fixMask           = 0xe0
lenMask           = 0x1f
