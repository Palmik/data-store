cabal-dev configure --enable-benchmarks
cabal-dev build 
cabal-dev bench --benchmark-option=-o$bench.html

