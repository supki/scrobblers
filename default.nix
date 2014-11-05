{ cabal, aeson, cereal, exceptions, hspec, httpClient
, httpTypes, lens, lensAeson, liblastfm, libmpd, mtl, netwire, network
, QuickCheck, semigroups, text, time
}:

cabal.mkDerivation (self: rec {
  pname = "scrobblers";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    aeson cereal exceptions httpClient httpTypes
	lens lensAeson liblastfm libmpd mtl netwire network semigroups
	text time
  ];
  testDepends = buildDepends ++ [ hspec QuickCheck ];
  meta = {
    description = "Lastfm scrobblers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
