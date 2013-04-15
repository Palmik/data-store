cabal-dev configure --enable-benchmarks -fbench_shallow -fbench_ds
cabal-dev build 
cabal-dev bench --benchmark-options="-o bench.html -r bench_cmp.out"

