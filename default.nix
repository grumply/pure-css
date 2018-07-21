{ mkDerivation, base, containers, ef, pure-core, pure-default, pure-txt, template-haskell, stdenv
, useTemplateHaskell ? true
}:
mkDerivation {
  pname = "pure-css";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ef pure-core pure-default pure-txt ] ++ (if useTemplateHaskell then [ template-haskell ] else []);
  configureFlags = (if useTemplateHaskell then [ "-f-use-template-haskell" ] else [ ] );
  homepage = "github.com/grumply/pure-css";
  license = stdenv.lib.licenses.bsd3;
}
