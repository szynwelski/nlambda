#!/bin/sh
cabal haddock --hyperlink-source
sed -i 's/file\:\/\/\/usr\/local\/haskell\/ghc\-7\.8\.3\-x86_64\/share\/doc\/ghc\/html\/libraries\/\(base\-4\.7\.0\.1\)/http:\/\/hackage.haskell.org\/package\/\1\/docs/g' dist/doc/html/NLambda/NLambda.html
sed -i 's/Nominal.html/NLambda.html/g' dist/doc/html/NLambda/NLambda.html
