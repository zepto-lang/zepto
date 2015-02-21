#Installs
all:
	cabal install

#Runs all tests
test:
	for i in scm-tests/t-*; do echo""; echo "Running test $$i"; echo "---"; r5rs $$i; echo "---"; done
