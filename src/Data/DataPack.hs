module Data.DataPack where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Word

fixintMask        = 0xc0::Word8
nilByte           = 0x40::Word8
collectionEndByte = 0x41::Word8
falseByte         = 0x42::Word8
trueByte          = 0x43::Word8
uint8Byte         = 0x44::Word8
uint16Byte        = 0x45::Word8
uint32Byte        = 0x46::Word8
uint64Byte        = 0x47::Word8
int8Byte          = 0x48::Word8
int16Byte         = 0x49::Word8
int32Byte         = 0x4a::Word8
int64Byte         = 0x4b::Word8
floatByte         = 0x4c::Word8
doubleByte        = 0x4d::Word8
bin8Byte          = 0x4e::Word8
bin16Byte         = 0x4f::Word8
bin32Byte         = 0x50::Word8
str8Byte          = 0x51::Word8
str16Byte         = 0x52::Word8
str32Byte         = 0x53::Word8
ns8Byte           = 0x54::Word8
ns16Byte          = 0x55::Word8
ns32Byte          = 0x56::Word8
classNameByte     = 0x57::Word8
sequenceByte      = 0x58::Word8
dictionaryByte    = 0x59::Word8
objectByte        = 0x5a::Word8
--unusedBytes = [0x5b..0x5f]
fixbinMask        = 0x60::Word8
fixstrMask        = 0x80::Word8
fixnsMask         = 0xa0::Word8
fixMask           = 0xe0::Word8
lenMask           = 0x1f::Word8

modifyAnd f andf = StateT $ \s -> let s' = f s in pure (andf s', s')

stateReaderContT :: ((a -> m r) -> m r) -> StateT b (ReaderT c (ContT r m)) a
stateReaderContT = lift . lift . ContT

contAsk f = lift ask >>= f
