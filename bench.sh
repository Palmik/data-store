cabal-dev configure --enable-benchmarks
cabal-dev build 
cabal-dev bench --verbose=0 --benchmark-option=-obenchmark.html
