# hexdump-lazy #

A Haskell library that produces hex dumps lazily from various types of source.

The output is a list of lines with type `[String]`. Each line represents 16 bytes in the input.

Possible inputs include `[Word8]`, `ByteString` and `Ptr a`.

The library itself has no dependencies beyond `base` and `bytestring`.

## Example Output ##

```
$ cabal repl
λ> import qualified Data.ByteString as BS
λ> fp = "dist-newstyle/packagedb/ghc-8.10.2/package.cache"
λ> bs <- BS.readFile fp
λ> putStr . unlines . take 16 $ hexDumpByteString bs
0000:  00 67 68 63  70 6b 67 00  00 00 00 01  00 00 00 00  |.ghcpkg.........
0010:  00 00 00 00  00 00 04 95  00 00 00 00  00 00 00 01  |................
0020:  00 00 00 00  00 00 00 14  68 65 78 64  75 6d 70 2d  |........hexdump-
0030:  6c 61 7a 79  2d 30 2e 31  2e 30 2e 30  00 00 00 00  |lazy-0.1.0.0....
0040:  00 00 00 0c  68 65 78 64  75 6d 70 2d  6c 61 7a 79  |....hexdump-lazy
0050:  00 00 00 00  00 00 00 04  00 00 00 00  00 00 00 00  |................
0060:  00 00 00 00  00 00 00 01  00 00 00 00  00 00 00 00  |................
0070:  00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  |................
0080:  00 00 00 00  00 00 00 00  1c 68 65 78  64 75 6d 70  |.........hexdump
0090:  2d 6c 61 7a  79 2d 30 2e  31 2e 30 2e  30 2d 69 6e  |-lazy-0.1.0.0-in
00a0:  70 6c 61 63  65 00 00 00  00 00 00 00  1c 68 65 78  |place........hex
00b0:  64 75 6d 70  2d 6c 61 7a  79 2d 30 2e  31 2e 30 2e  |dump-lazy-0.1.0.
00c0:  30 2d 69 6e  70 6c 61 63  65 00 00 00  00 00 00 00  |0-inplace.......
00d0:  00 00 00 00  00 00 00 00  07 69 6e 70  6c 61 63 65  |.........inplace
00e0:  00 00 00 00  00 00 00 02  00 00 00 00  00 00 00 0d  |................
00f0:  62 61 73 65  2d 34 2e 31  34 2e 31 2e  30 00 00 00  |base-4.14.1.0...
```
