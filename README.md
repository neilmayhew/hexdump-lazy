# hexdump-lazy #

A Haskell library that produces hex dumps lazily from various types of source.

The output is a list of lines with type `[String]`. Each line represents 16 bytes in the input.

Possible inputs include `[Word8]`, `ByteString` and `Ptr a`.

The library itself has no dependencies beyond `base` and `bytestring`.
