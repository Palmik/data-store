GHC=~/.local/applications/ghc-7.4.1/bin/ghc
cd benchmarks/src
$GHC --make -fforce-recomp 01HS.hs
unbuffer ./01HS -G -o ../../bench_HS.html -u ../../bench_HS.summary | tee ../../bench_HS.out

