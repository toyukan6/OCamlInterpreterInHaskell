GHC=ghc
INCLUDES=
GHCFLAGS=$(INCLUDES)
GHCOPTFLAGS=$(INCLUDES)

PROGNAME=miniml

OBJS=syntax.o main.o

all: $(OBJS)
	$(GHC) -o $(PROGNAME) $(GHCFLAGS) $(OBJS)

.SUFFIXES: .hs .o

.hs.o:
	$(GHC) $(GHCFLAGS) -c $<

clean:
	$(RM) $(PROGNAME)
	$(RM) *.hi *.o


