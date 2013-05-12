cabal-dev configure --enable-benchmarks
cabal-dev build --ghc-option="-fsimpl-tick-factor=150"
unbuffer cabal-dev bench --benchmark-options="-G -o bench.html -u bench.summary"  | tee bench.out

