cabal-dev configure --enable-benchmarks
cabal-dev -DBENCH_SHALLOW=1 build 
cabal-dev -DBENCH_SHALLOW=1 bench --benchmark-option=-o$bench.html

