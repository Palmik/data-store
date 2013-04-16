cabal-dev configure --enable-benchmarks
cabal-dev build --ghc-option=-fsimpl-tick-factor=130
cabal-dev bench --benchmark-options="-o bench.html -r bench_cmp.out"

