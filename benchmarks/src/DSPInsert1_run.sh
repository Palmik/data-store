ghc -outputdir='.ghc_garbage' --make -package-db=../../cabal-dev/packages-7.6.2.conf/ DSPInsert1.hs -O2 -rtsopts -prof -auto-all -caf-all -fforce-recomp
./DSPInsert1 +RTS -p

mv DSPInsert1.prof DSPInsert/01/prof.prof

./DSPInsert1 +RTS -i0.00001 -hc -p
mv DSPInsert1.hp DSPInsert/01/hc.hp
hp2ps -e8in -c DSPInsert/01/hc.hp 
mv hc.ps DSPInsert/01/hc.ps

./DSPInsert1 +RTS -i0.00001 -hd -p
mv DSPInsert1.hp DSPInsert/01/hd.hp
hp2ps -e8in -c DSPInsert/01/hd.hp
mv hd.ps DSPInsert/01/hd.ps

./DSPInsert1 +RTS -i0.00001 -hy -p
mv DSPInsert1.hp DSPInsert/01/hy.hp
hp2ps -e8in -c DSPInsert/01/hy.hp
mv hy.ps DSPInsert/01/hy.ps

rm *.aux

./DSPInsert1 +RTS -sstderr &> DSPInsert/01/s.out

