ghc -outputdir='.ghc_garbage' --make -package-db=../../cabal-dev/packages-7.6.2.conf/ DSPInsert2.hs -O2 -rtsopts -prof -auto-all -caf-all -fforce-recomp
./DSPInsert2 +RTS -p

mv DSPInsert2.prof DSPInsert/02/prof.prof

./DSPInsert2 +RTS -i0.00002 -hc -p
mv DSPInsert2.hp DSPInsert/02/hc.hp
hp2ps -e8in -c DSPInsert/02/hc.hp 
mv hc.ps DSPInsert/02/hc.ps

./DSPInsert2 +RTS -i0.00002 -hd -p
mv DSPInsert2.hp DSPInsert/02/hd.hp
hp2ps -e8in -c DSPInsert/02/hd.hp
mv hd.ps DSPInsert/02/hd.ps

./DSPInsert2 +RTS -i0.00002 -hy -p
mv DSPInsert2.hp DSPInsert/02/hy.hp
hp2ps -e8in -c DSPInsert/02/hy.hp
mv hy.ps DSPInsert/02/hy.ps

rm *.aux

./DSPInsert2 +RTS -sstderr &> DSPInsert/02/s.out

