override HFLAGS+=-O -XExistentialQuantification
PREFIX=/usr/bin/
BUILDDIR=bin/
DEBUGDIR=debug/
LIBS=parsec

CC=ghc

TARGET=r5rs
SOURCES=$(filter-out src/primitivies.hs, $(wildcard src/*.hs))

#Makes everything
all:
	mkdir -p $(BUILDDIR)  2> /dev/null
	$(CC) $(HFLAGS) -package $(LIBS) $(SOURCES) -o $(BUILDDIR)$(TARGET)
	rm src/*.hi src/*.o

#Uses picky extensions and makes everything(Extensions may break compiling)
dev:
	make all HFLAGS+="-dcore-lint"

#Stops after each step and saves every intermediate file generated
debug: pp

#Stops after preprocessing
pp:
	mkdir -p $(DEBUGDIR) 
	$(CC) $(HFLAGS) -E $(SOURCES)
	mv src/*.hspp $(DEBUGDIR)
#Cleans directory(no uninstall!)
clean: 
	rm -rf $(BUILDDIR) $(DEBUGDIR)
	rm src/*.hi src/*.o 2> /dev/null || true

#Installs into specified(or default) directory
install:
	install -d $(PREFIX)$(TARGET)
	install $(BUILDDIR)$(TARGET) $(PREFIX)$(TARGET)

#Uninstalls from specified(or default)directory
uninstall: 
	rm -rf $(PREFIX)$(TARGET)
