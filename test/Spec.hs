{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Data.Function (on)
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as C
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
  | Fail Int64 String
  deriving Show

createDefaultCallbacks f = let
    g str = const $ f str
  in
  Callbacks {
      nil = f "nil",
      collectionEnd = f "collectionEnd",
      boolean = g "boolean",
      int = g "int",
      uint32 = g "uint32",
      uint64 = g "uint64",
      int64 = g "int64",
      float = g "float",
      double = g "double",
      binStart = g "binStart",
      strStart = g "strStart",
      nsStart = g "nsStart",
      dat = g "dat",
      className = f "className",
      sequenceD = f "sequence",
      dictionary = f "dictionary",
      object = f "object" }

failCallbacks :: Applicative m => String -> Callbacks ByteStringPos (m Results)
failCallbacks expectedStr = createDefaultCallbacks $ \str s _ ->
    pure $ Fail (pos s) ("Expected " ++ expectedStr ++ ", found " ++ str)

passCallbacks = createDefaultCallbacks $ \_ s f -> f s

newtype CallbackStack s st m = CallbackStack {
    getStack :: [Callbacks s (StateT (CallbackStack s st m) m Results)] }

passThruCallbacks :: Monad m =>
  Callbacks ByteStringPos (StateT (CallbackStack ByteStringPos st m) m Results)
passThruCallbacks = createDefaultCallbacks $ \str s f ->
  get >>= \cbs ->
    case getStack cbs of
      cb:cbs' -> put (CallbackStack cbs') >> nil cb s f
      _ -> pure $ Fail (pos s) $ "unexpected " ++ str

catchers :: Monad m =>
  Catchers () ByteStringPos (StateT (CallbackStack ByteStringPos st m) m Results)
catchers = Catchers {
  stream = \e _ s _ -> pure $ Fail (pos s) $ "stream fail " ++ show e,
  invalidByte = \byte stack allowedByteRanges s f ->
    pure $ Fail (pos s) $ "fail " ++
      show byte ++ " is not allowed with state of " ++ show stack ++
      ". Expected values within the ranges of "
        ++ show allowedByteRanges ++ ".",
  unusedByte = \byte s f ->
    pure $ Fail (pos s) $ "fail " ++ show byte ++ " is not a usable byte.",
  programmatic = undefined }

testCase bytString callbackStack catchers expectedBytesRemaining =
  runStateT (unpackDataT (ByteStringPos bytString 0) catchers passThruCallbacks $ \bStrPos ->
    let len = C.length $ byteString bStrPos in
    pure $ if len == expectedBytesRemaining
      then Ok
      else Fail (pos bStrPos) $ "Expected " ++ show expectedBytesRemaining ++ " byte(s) remaining in byteString. Found " ++ show len ++ " byte(s).")
    $ CallbackStack callbackStack

runTests testCases =
  putStrLn "Running tests...\n" >> let
      (io, (runCount, failCount)) = foldl (
          \(ioAcc, (runCount, failCount)) (testName, resultsAndRemainBytes) ->
            let
              (results, _) = runIdentity resultsAndRemainBytes
              runCount' = runCount + 1
              prefix = '\t':show runCount' ++ ") test " ++ testName ++ ": "
            in
            case results of
            Ok -> (ioAcc >> putStrLn (prefix ++ "passed"), (runCount', failCount))
            Fail pos msg -> (ioAcc >> putStrLn (prefix ++ "failed at position " ++ show pos ++ "\x2014" ++ msg), (runCount', failCount + 1))
        ) (pure (), (0, 0)) testCases
  in
  io >>
  if failCount == 0
  then putStrLn $ "\nSuccess! All " ++ show runCount ++ " test(s) passed."
  else (putStrLn $ '\n':show failCount ++ " out of " ++ show runCount ++ " test(s) failed.") >> exitFailure

testUnpackT :: IO ()
testUnpackT = runTests [
        ("single nil", testCase (C.pack [nilByte]) [(failCallbacks "nil") {nil = nil passCallbacks}] catchers 0),
        ("single nil", testCase (C.pack []) [(failCallbacks "nil") {nil = nil passCallbacks}] catchers 0),
        ("single nil", testCase (C.pack [nilByte]) [(failCallbacks "dictionary") {dictionary = dictionary passCallbacks}] catchers 0),
        ("single nil", testCase (C.pack [nilByte]) [] catchers 0),
        ("single nil", testCase (C.pack [nilByte]) [(failCallbacks "nil") {nil = nil passCallbacks}] catchers 1),
        ("single nil", testCase (C.pack [nilByte, dictionaryByte]) [(failCallbacks "nil") {nil = nil passCallbacks}] catchers 1)
      ]

main = testUnpackT
