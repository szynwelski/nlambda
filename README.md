# Nλ

Nλ is a simple functional programming language aimed at manipulating infinite, but first-order definable structures, such as the countably infinite clique graph or the set of all intervals with rational endpoints. Internally, such sets are represented by logical formulas that define them, and an external satisfiability modulo theories (SMT) solver is regularly run by the interpreter to check their basic properties.

For more information, visit [NLambda website](http://www.mimuw.edu.pl/~szynwelski/nlambda/).

# Installation guide

1. There are two ways to get a source code:
  * download and unpack the package file [http://www.mimuw.edu.pl/~szynwelski/nlambda/nlambda-1.1.tar.gz](http://www.mimuw.edu.pl/~szynwelski/nlambda/nlambda-1.1.tar.gz),
  * check out the source code from git:

    `$ git clone https://github.com/szynwelski/nlambda.git`

2. The language is implemented in a Haskell and installation of GHC (at least 7.10) is required.

3. Move into nlambda directory and perform the following commands:
   ```
   cabal v2-configure -fTOTAL_ORDER
   cabal v2-build
   cabal v2-install
   ```
   The flag `TOTAL_ORDER` is required to install the package with ordered atoms (otherwise equality atoms will be used).
   
   More information on how to install a Cabal package can be found [here](https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package).

4. You can also use dedicated scripts:
  * for ordered atoms

    `$ ./intall-total-order.sh`
  * for equality atoms

    `$ ./install-equality.sh`

5. Additionally, you should install [the Z3 Theorem Prover](https://github.com/Z3Prover/z3) and add it to the PATH environment variable.

# Interactive environment

Nλ expressions can be interpreted and evaluated on the fly using the interactive [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) environment. For easier use of the environment, one can use the `.ghci` configuration file, which imports the module with the language, hides Prelude functions that conflict with Nλ, and sets useful options.
```
:set -XNoImplicitPrelude
:set -XRebindableSyntax
:m NLambda
:set prompt "Nλ> "
import Prelude hiding (or, and, not, sum, map, filter, maybe)
let [a,b,c,d,e] = fmap atom ["a","b","c","d","e"]
```
