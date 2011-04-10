echo "Compiling Octi for kids"

ghc --make -O Main.hs -o ofk.exe -O -fvia-C -O2-for-C

#rm *.o *.hi;
#/usr/local/ghc/bin/ghc -c -O thtools.hs;
#/usr/local/ghc/bin/ghc -c tree.hs
#/usr/local/ghc/bin/ghc -c minimax.hs;
#/usr/local/ghc/bin/ghc -c game.hs;
#/usr/local/ghc/bin/ghc -c ofk.hs;
#/usr/local/ghc/bin/ghc -c main.hs;
#/usr/local/ghc/bin/ghc *.o
