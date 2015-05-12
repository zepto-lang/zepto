override CARGS+=-j2
#Installs
all:
	cabal install $(CARGS)
	rm -r dist

#Runs all tests
test:
	for i in zepto-tests/test-*; do echo""; echo "Running test $$i"; echo "---"; zepto $$i; echo "---"; done

