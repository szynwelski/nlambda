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
   runhaskell Setup configure --user -fTOTAL_ORDER
   runhaskell Setup build
   runhaskell Setup install
   ```
  The flag `TOTAL_ORDER` is required to install the package with ordered atoms (otherwise equality atoms will be used).
  
  To install the package globally (not only for the user account) skip `user` parameter (see: [how to install a Cabal package](https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package)).

4. You can also use dedicated scripts:
  * for ordered atoms

    `$ ./intall-total-order.sh`
  * for equality atoms

    `$ ./install-equality.sh`

5. Additionally, you should install [the Z3 Theorem Prover](https://github.com/Z3Prover/z3) and add it to the PATH environment variable.
