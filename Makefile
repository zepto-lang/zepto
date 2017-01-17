override CARGS+=-j2
#Installs
all: install

force: CARGS+=--force-reinstalls
force: install

install:
	cabal install $(CARGS)
	if [ ! -f ~/.zeptorc ]; then touch ~/.zeptorc; fi

debug:
	cabal configure --enable-executable-profiling
	cabal build -auto-all -caf-all -fforce-recomp && cabal test && cabal install $(CARGS)

#Runs all tests
test:
	for i in tests/test-*; do echo""; echo "Running test $$i"; echo "---"; zepto $$i; echo "---"; done

clean:
	rm -r dist
