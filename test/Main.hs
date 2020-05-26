{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HexDump

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.ByteString (ByteString, useAsCStringLen)
import Data.Foldable (for_)
import Data.List (intersperse)
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Array (advancePtr, copyArray, peekArray)
import System.Exit
import System.IO (hClose, hGetContents)
import System.Process (CreateProcess(..), StdStream(..), proc, withCreateProcess)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

testString :: ByteString
testString = "\1\0\0\1*\0\0\2\3def\6pasta\7program\7program\4uuid\4uuid\f?\
             \\0\20\0\0\0\376\205P\0\0\0002\0\0\3\3def\6pasta\7program\7program\
             \\4name\4name\f!\0\375\377\2\0\374\25P\0\0\0002\0\0\4\3def\6pasta\7\
             \program\7program\4data\4data\f?\0\377\377\377\377\374\221\20\0\0\0.\
             \\0\0\5\3def\6pasta\7program\7program\2id\2id\f?\0\24\0\0\0\10\3B\0\
             \\0\0\5\0\0\6\376\0\0\3\0\5\0\0\7\376\0\0\3\0\255"

testResult :: [String]
testResult =
  [ "0000:  01 00 00 01  2a 00 00 02  03 64 65 66  06 70 61 73  ....*....def.pas"
  , "0010:  74 61 07 70  72 6f 67 72  61 6d 07 70  72 6f 67 72  ta.program.progr"
  , "0020:  61 6d 04 75  75 69 64 04  75 75 69 64  0c 3f 00 14  am.uuid.uuid.?.."
  , "0030:  00 00 00 78  cd 50 00 00  02 00 00 03  03 64 65 66  ...x.P.......def"
  , "0040:  06 70 61 73  74 61 07 70  72 6f 67 72  61 6d 07 70  .pasta.program.p"
  , "0050:  72 6f 67 72  61 6d 04 6e  61 6d 65 04  6e 61 6d 65  rogram.name.name"
  , "0060:  0c 21 00 77  79 02 00 76  19 50 00 00  02 00 00 04  .!.wy..v.P......"
  , "0070:  03 64 65 66  06 70 61 73  74 61 07 70  72 6f 67 72  .def.pasta.progr"
  , "0080:  61 6d 07 70  72 6f 67 72  61 6d 04 64  61 74 61 04  am.program.data."
  , "0090:  64 61 74 61  0c 3f 00 79  79 79 79 76  dd 14 00 00  data.?.yyyyv...."
  , "00a0:  00 2e 00 00  05 03 64 65  66 06 70 61  73 74 61 07  ......def.pasta."
  , "00b0:  70 72 6f 67  72 61 6d 07  70 72 6f 67  72 61 6d 02  program.program."
  , "00c0:  69 64 02 69  64 0c 3f 00  18 00 00 00  0a 03 42 00  id.id.?.......B."
  , "00d0:  00 00 05 00  00 06 78 00  00 03 00 05  00 00 07 78  ......x........x"
  , "00e0:  00 00 03 00  ff                                     ....."
  ]

makeBuffer :: Int -> IO (ForeignPtr Word8)
makeBuffer bufferSize = do
  buffer <- mallocForeignPtrArray bufferSize :: IO (ForeignPtr Word8)
  useAsCStringLen testString $ \(inputPtr, inputSize) ->
    withForeignPtr buffer $ \ptr ->
      for_ [0, inputSize .. bufferSize - 1] $ \offset ->
        copyArray
          (ptr `advancePtr` offset)
          (castPtr inputPtr)
          (min inputSize (bufferSize - offset))
  pure buffer

main :: IO ()
main = do
  unless (hexDumpByteString testString == testResult)
    exitFailure

  dump <- useAsCStringLen testString $
    uncurry hexDumpPtr

  unless (dump == testResult)
    exitFailure

  external <- modelDump testString

  unless (dump == external) $
    die . unlines $ [""] ++ dump ++ ["vs"] ++ external

  let bufferSize = 1 * 1024 + 19

  buffer <- makeBuffer bufferSize

  let dump0 = hexDumpWords . BL.unpack . BL.take (fromIntegral bufferSize) . BL.cycle
        $ BL.fromStrict testString

  dump1 <- withForeignPtr buffer $ \ptr ->
    hexDumpPtr ptr bufferSize

  dump2 <- withForeignPtr buffer $
    fmap hexDumpWords . peekArray bufferSize

  unless (dump0 == dump1 && dump1 == dump2)
    exitFailure

modelDump :: ByteString -> IO [String]
modelDump bs =
  fmap lines . withCreateProcess
    (proc "hexdump" $ intersperse "-e"
        [ "-v"
        , "\"%04.4_ax:  \""
        , "4/1 \"%02x \" \"  \" 4/1 \"%02x \" \"  \" 4/1 \"%02x \" \"  \" 4/1 \"%02x \"\"  \""
        , "\"%_p\""
        , "\"\\n\""
        ])
      { std_out = CreatePipe
      , std_in = CreatePipe } $
        \mhin mhout _ _ -> do
          maybe (pure ()) (\h -> BS.hPut h bs >> hClose h) mhin
          evaluate . force =<< maybe (pure "") hGetContents mhout
