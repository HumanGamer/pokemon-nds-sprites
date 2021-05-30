
CC=gcc
mingwCC=i486-mingw32-gcc

# Warnings are good! Enable all of them. -Wall -Wextra
#
# Except rip.c has a lot of unused functions (TODO: command-line
# interface), so don't warn about them. -Wno-unused-function.
#
# gcc complains about my multi-character char constants (clang doesn't,
# however). -Wno-multichar
#
# Most of my structs are layed out to mirror the file format, so i can be
# lazy and use fread(). If the compiler adds padding, things will probably
# blow up. -Wpadded
warnings:=-Wall -Wextra -Wno-unused-function -Wpadded

ifeq (gcc,$(CC))
warnings+=-Wno-multichar
endif

# _POSIX_C_SOURCE>=200809 is needed for fmemopen(3)
CFLAGS=-g -O2 -std=c99 -D_POSIX_C_SOURCE=200809L -fwrapv $(warnings)
LDFLAGS=-lpng -lm -lz -lgif

# Tell the C compiler where to find <libguile.h>
CFLAGS+=`guile-config compile`

# Tell the linker what libraries to use and where to find them.
LIBS=`guile-config link`

sources=./src/common.c ./src/lzss.c ./src/image.c ./src/nitro.c ./src/narc.c ./src/ncgr.c ./src/nclr.c ./src/ncer.c ./src/nanr.c ./src/nmcr.c
objects=$(sources:.c=.o)

rip: ./src/rip.o $(objects)
	$(CC) -o $@ $< $(objects) $(CFLAGS) $(LDFLAGS)

rip.exe: ./src/rip.o $(objects)
	$(mingwCC) -o $@ $< $(objects) $(CFLAGS) $(LDFLAGS)

ripscript: ./src/ripscript.o $(objects)
	$(CC) -o $@ $< $(objects) $(LDFLAGS) -lguile-2.2 -pthread

rip.o: ./src/rip.c ./src/common.h ./src/lzss.h ./src/image.h ./src/nitro.h ./src/narc.h ./src/ncgr.h ./src/nclr.h ./src/ncer.h Makefile
ripscript.o: ./src/ripscript.c ./src/common.h ./src/image.h ./src/nitro.h ./src/narc.h ./src/ncgr.h ./src/nclr.h ./src/nanr.h ./src/nmcr.h ./src/nmar.h Makefile
clean:
	rm ./src/rip.o ./src/ripscript.o $(objects)
