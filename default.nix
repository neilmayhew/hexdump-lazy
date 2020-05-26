{ mkDerivation, stdenv, utillinux
, base, bytestring, deepseq, hspec, process, QuickCheck }:
mkDerivation {
  pname = "hexdump-lazy";
  version = "0.1.0.0";
  src = ./.;
  buildTools = [ utillinux ]; # hexdump is used in checks
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base bytestring deepseq hspec process QuickCheck ];
  homepage = "https://github.com/neilmayhew/hexdump-lazy";
  description = "Produce hex dumps lazily from various types of source";
  license = stdenv.lib.licenses.mit;
}
