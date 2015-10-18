override CARGS+=-j2
#Installs
all:
	git submodule foreach git checkout master
	git submodule foreach git pull
	make install

install:
	cabal install $(CARGS)

#Runs all tests
test:
	for i in zepto-tests/test-*; do echo""; echo "Running test $$i"; echo "---"; zepto $$i; echo "---"; done

clean:
	rm -r dist
