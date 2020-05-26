{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HexDump

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bifunctor (second)
import Data.ByteString (ByteString, useAsCStringLen)
import Data.Char (isAscii, isHexDigit, isPrint, isSpace)
import Data.Foldable (find, for_, traverse_)
import Data.List (intersperse)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Array (advancePtr, copyArray, peekArray)
import Foreign.Ptr (castPtr)
import GHC.Stack (HasCallStack)
import System.IO (hClose, hGetContents)
import System.Process (CreateProcess(..), StdStream(..), proc, withCreateProcess)
import Test.Hspec (Expectation, describe, expectationFailure, hspec, it, runIO, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

shouldAllSatisfy :: (HasCallStack, Show a) => [a] -> (a -> Bool) -> Expectation
shouldAllSatisfy v p = case find (not . p) v of
  Just x -> expectationFailure $ "predicate failed on " ++ show x ++ " in " ++ show v
  _ -> pure ()

infix 1 `shouldAllSatisfy`

testString :: ByteString
testString = "\1\0\0\1*\0\0\2\3def\6pasta\7program\7program\4uuid\4uuid\f?\
             \\0\20\0\0\0\376\205P\0\0\0002\0\0\3\3def\6pasta\7program\7program\
             \\4name\4name\f!\0\375\377\2\0\374\25P\0\0\0002\0\0\4\3def\6pasta\7\
             \program\7program\4data\4data\f?\0\377\377\377\377\374\221\20\0\0\0.\
             \\0\0\5\3def\6pasta\7program\7program\2id\2id\f?\0\24\0\0\0\10\3B\0\
             \\0\0\5\0\0\6\376\0\0\3\0\5\0\0\7\376\0\0\3\0\255"

testResult :: [String]
testResult =
  [ "0000:  01 00 00 01  2a 00 00 02  03 64 65 66  06 70 61 73  |....*....def.pas"
  , "0010:  74 61 07 70  72 6f 67 72  61 6d 07 70  72 6f 67 72  |ta.program.progr"
  , "0020:  61 6d 04 75  75 69 64 04  75 75 69 64  0c 3f 00 14  |am.uuid.uuid.?.."
  , "0030:  00 00 00 78  cd 50 00 00  02 00 00 03  03 64 65 66  |...x.P.......def"
  , "0040:  06 70 61 73  74 61 07 70  72 6f 67 72  61 6d 07 70  |.pasta.program.p"
  , "0050:  72 6f 67 72  61 6d 04 6e  61 6d 65 04  6e 61 6d 65  |rogram.name.name"
  , "0060:  0c 21 00 77  79 02 00 76  19 50 00 00  02 00 00 04  |.!.wy..v.P......"
  , "0070:  03 64 65 66  06 70 61 73  74 61 07 70  72 6f 67 72  |.def.pasta.progr"
  , "0080:  61 6d 07 70  72 6f 67 72  61 6d 04 64  61 74 61 04  |am.program.data."
  , "0090:  64 61 74 61  0c 3f 00 79  79 79 79 76  dd 14 00 00  |data.?.yyyyv...."
  , "00a0:  00 2e 00 00  05 03 64 65  66 06 70 61  73 74 61 07  |......def.pasta."
  , "00b0:  70 72 6f 67  72 61 6d 07  70 72 6f 67  72 61 6d 02  |program.program."
  , "00c0:  69 64 02 69  64 0c 3f 00  18 00 00 00  0a 03 42 00  |id.id.?.......B."
  , "00d0:  00 00 05 00  00 06 78 00  00 03 00 05  00 00 07 78  |......x........x"
  , "00e0:  00 00 03 00  ff                                     |....."
  ]

lineLengthFor :: Int -> Int
lineLengthFor n = offsetLength + 3 + groups * (groupSize * byteWidth + 1) + 1 + n
  where
    offsetLength = 4
    groups = 4
    groupSize = 4
    byteWidth = 3

splitLine :: String -> (String, String, String)
splitLine l = (offset, bytes, chars)
  where
    (offset, rest) = splitOn ':' l
    (bytes, chars) = splitOn '|' rest
    splitOn c = second (drop 1) . span (/= c)

extractLineOffset, extractLineBytes, extractLineChars :: String -> String
extractLineOffset l = let (o, _, _) = splitLine l in o
extractLineBytes  l = let (_, b, _) = splitLine l in b
extractLineChars  l = let (_, _, c) = splitLine l in c

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
main = hspec $ modifyMaxSize (*20) $ describe "Text.HexDump" $ do

  describe "Example tests" $ do

    describe "Empty input" $ do

      it "hexDumpWords has empty output" $
        hexDumpWords [] `shouldBe` []

    describe "A test string" $ do

      it "hexDumpByteString matches the expected result" $
        hexDumpByteString testString `shouldBe` testResult

      it "hexDumpPtr matches the expected result" $
        useAsCStringLen testString (uncurry hexDumpPtr)
          `shouldReturn` testResult

      it "modelDump matches the expected result" $
        modelDump testString `shouldReturn` testResult

    describe "A buffer" $ do
      let
        bufferSize = 1 * 1024 + 19

        expected = BL.unpack . BL.take (fromIntegral bufferSize) . BL.cycle
          $ BL.fromStrict testString

      buffer <- runIO $ makeBuffer bufferSize

      content <- runIO $ withForeignPtr buffer $ peekArray bufferSize

      it "has the correct content" $
        content `shouldBe` expected

      it "hexDumpPtr matches hexDumpWords" $
        withForeignPtr buffer (`hexDumpPtr` bufferSize)
          `shouldReturn` hexDumpWords content

      it "modelDump matches hexDumpWords" $
        modelDump (BS.pack content)
        `shouldReturn` hexDumpWords content

  describe "Property tests" $ do

    describe "hexDumpWords" $ do

      prop "has empty output iff the input is empty" $ \ws ->
        null (hexDumpWords ws) `shouldBe` null ws

      prop "has the right number of lines" $ \ws ->
        let ls = hexDumpWords ws
            m = length ws
            n = length ls
        in n `shouldBe` (m + 15) `div` 16

      prop "has lines with the right lengths" $ \ws ->
        let ls = hexDumpWords ws
            m = length ws
            ms = replicate (m `div` 16) 16 ++ filter (/= 0) [m `mod` 16]
            ns = map length ls
        in ns `shouldBe` map lineLengthFor ms

      prop "contains only hex digits in the offset" $
        traverse_ (\l -> extractLineOffset l `shouldAllSatisfy` isHexDigit) . hexDumpWords

      let isHexOrSpace c = isHexDigit c || isSpace c
      prop "contains only hex digits and spaces in the bytes" $
        traverse_ (\l -> extractLineBytes l `shouldAllSatisfy` isHexOrSpace) . hexDumpWords

      let isAsciiPrint c = isAscii c && isPrint c
      prop "contains only printable ascii characters in the chars" $
        traverse_ (\l -> extractLineChars l `shouldAllSatisfy` isAsciiPrint) . hexDumpWords

      prop "matches the model" $ \ws ->
        (hexDumpWords ws `shouldBe`) =<< modelDump (BS.pack ws)

modelDump :: ByteString -> IO [String]
modelDump bs =
  fmap lines . withCreateProcess
    (proc "hexdump" $ intersperse "-e"
        [ "-v"
        , "\"%04.4_ax:  \""
        , "4/1 \"%02x \" \"  \" 4/1 \"%02x \" \"  \" 4/1 \"%02x \" \"  \" 4/1 \"%02x \"\"  \""
        , "\"|\""
        , "\"%_p\""
        , "\"\\n\""
        ])
      { std_out = CreatePipe
      , std_in = CreatePipe } $
        \mhin mhout _ _ -> do
          maybe (pure ()) (\h -> BS.hPut h bs >> hClose h) mhin
          evaluate . force =<< maybe (pure "") hGetContents mhout
