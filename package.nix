{ mkDerivation, aeson, base, bytestring, cereal, containers
, exceptions, hspec, http-client, http-types, lens, lens-aeson
, liblastfm, libmpd, mtl, netwire, network, QuickCheck, semigroups
, stdenv, text, time
}:
mkDerivation {
  pname = "scrobblers";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cereal containers exceptions http-client
    http-types lens lens-aeson liblastfm libmpd mtl netwire network
    semigroups text time
  ];
  testHaskellDepends = [
    base bytestring hspec lens netwire network QuickCheck
  ];
  description = "Lastfm scrobblers";
  license = stdenv.lib.licenses.bsd3;
}
