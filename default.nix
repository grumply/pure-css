{ mkDerivation, base, containers, ef, pure-core, pure-default, pure-txt, template-haskell, stdenv
, noUseTemplateHaskell ? false
}:
mkDerivation {
  pname = "pure-css";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ef pure-core pure-default pure-txt ] ++ (if noUseTemplateHaskell then [ ] else [ template-haskell ] );
  configureFlags = (if noUseTemplateHaskell then [ ] else [ "-f-use-template-haskell" ] );
  homepage = "github.com/grumply/pure-css";
  license = stdenv.lib.licenses.bsd3;
}
