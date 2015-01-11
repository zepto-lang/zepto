override HFLAGS+=-O -XSafe -fwarn-unsafe
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

#Uses picky extensions and makes everything(Extensions may break compiling)
dev:
	make all HFLAGS+="-fwarn-safe -fpackage-trust"

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
