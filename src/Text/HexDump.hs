{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Text.HexDump where

import Data.Bits
import Data.ByteString (ByteString, unpack)
import Data.Char (chr, ord, intToDigit)
import Data.Traversable (for)
import Data.Word (Word8)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr)

lineLength :: Int
lineLength = 16

offsetWidth :: Int
offsetWidth = 4

groupWidth :: Int
groupWidth = 4

hexDumpLineS :: (Integral a, Bits a) => a -> [Word8] -> ShowS
hexDumpLineS offset ws =
  let
    showBytes = foldS showByteGroup
      . chunksOf groupWidth
      . take lineLength
      $ map Just ws ++ repeat Nothing
    showByteGroup mws = foldS showByte mws . showChar ' '
    showByte mw = maybe (showString "  ") (showHex 2) mw . showChar ' '

    showChars = foldS (showChar . toPrintable) ws
    toPrintable (fromIntegral -> b) = if ord ' ' <= b && b <= ord '~' then chr b else '.'
  in
    showHex offsetWidth offset . showString ":  " . showBytes . showString "|" . showChars

hexDumpLine :: (Integral a, Bits a) => a -> [Word8] -> String
hexDumpLine offset ws = hexDumpLineS offset ws ""

hexDumpLines :: [[Word8]] -> [String]
hexDumpLines = zipWith hexDumpLine =<< scanl (+) 0 . map length

hexDumpWords :: [Word8] -> [String]
hexDumpWords = hexDumpLines . chunksOf lineLength

hexDumpByteString :: ByteString -> [String]
hexDumpByteString = hexDumpWords . unpack

hexDumpPtr :: Integral b => Ptr a -> b -> IO [String]
hexDumpPtr (castPtr -> buffer) (fromIntegral -> size) =
  fmap hexDumpLines . for [0, lineLength .. size - 1] $ \offset ->
    peekArray (min lineLength (size - offset)) (buffer `advancePtr` offset)

foldS :: Foldable t => (a -> ShowS) -> t a -> ShowS
foldS = flip . foldr

showHex :: (Integral a, Bits a) => Int -> a -> ShowS
showHex k n = foldS
  (\i -> showChar . intToDigit . fromIntegral $ n `shiftR` (i `shiftL` 2) .&. 15)
  [k-1, k-2 .. 0]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest
