{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "scrobblers";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = with haskellPackages; [
    aeson cereal exceptions httpClient httpTypes lens lensAeson
    liblastfm libmpd mtl netwire network semigroups text time
  ];
  testDepends = with haskellPackages; [
    hspec lens netwire network QuickCheck
  ];
  meta = {
    description = "Lastfm scrobblers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
