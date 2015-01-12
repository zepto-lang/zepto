override HFLAGS+=-O -XSafe -XExistentialQuantification -fwarn-unsafe
PREFIX=/usr/bin/
BUILDDIR=bin/
DEBUGDIR=debug/
LIBS=parsec

CC=ghc

TARGET=r5rs
SOURCES=$(wildcard src/*.hs)

#Makes everything
all:
	mkdir -p $(BUILDDIR)  2> /dev/null
	$(CC) $(HFLAGS) -package $(LIBS) $(SOURCES) -o $(BUILDDIR)$(TARGET)
	rm src/*.hi src/*.o

#Uses picky extensions and makes everything(Extensions may break compiling)
dev:
	make all HFLAGS+=""

#Stops after each step and saves every intermediate file generated
debug: pp

#Stops after preprocessing
pp:
	mkdir -p $(DEBUGDIR)  2> /dev/null
	$(CC) $(HFLAGS) -E $(SOURCES)
	mv src/*.hspp $(DEBUGDIR)
#Cleans directory(no uninstall!)
clean: 
	rm -rf $(BUILDDIR) $(DEBUGDIR)
	rm src/*.hi src/*.o

#Installs into specified(or default) directory
install:
	install -d $(PREFIX)$(TARGET)
	install $(BUILDDIR)$(TARGET) $(PREFIX)$(TARGET)

#Uninstalls from specified(or default)directory
uninstall: 
	rm -rf $(PREFIX)$(TARGET)
