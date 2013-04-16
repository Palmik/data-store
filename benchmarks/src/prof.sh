ghc --make -package-db=../../cabal-dev/packages-7.6.2.conf/ DSPInsert.hs -O2 -rtsopts -prof -auto-all -caf-all -fforce-recomp
./DSPInsert +RTS -i0.00001 -hc -p
hp2ps -e8in -c DSPInsert.hp
ps2pdf DSPInsert.ps
./DSPInsert +RTS -sstderr

