#Installs
all:
	cabal install
	rm -r dist

#Runs all tests
test:
	for i in scm-tests/test-*; do echo""; echo "Running test $$i"; echo "---"; zepto $$i; echo "---"; done

