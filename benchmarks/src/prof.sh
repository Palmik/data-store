ghc --make -package-db=../../cabal-dev/packages-7.6.2.conf/ DSPInsert.hs -O2 -rtsopts -prof -auto-all -caf-all -fforce-recomp
./DSPInsert +RTS -p
cat ./DSPInsert.prof
