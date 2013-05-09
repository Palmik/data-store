GHC=~/.local/applications/ghc-7.4.1/bin/ghc
cd benchmarks/src
$GHC --make -fforce-recomp 01HS.hs
unbuffer ./01HS -g -o ../../bench_HS.html | tee ../../bench_HS.out

