GHC=ghc
INCLUDES=
GHCFLAGS=$(INCLUDES)
GHCOPTFLAGS=$(INCLUDES)

PROGNAME=miniml

OBJS=parser.hs environment.hs syntax.hs main.hs

all: $(OBJS)
	$(GHC) -o $(PROGNAME) $(GHCFLAGS) $(OBJS)

clean:
	$(RM) $(PROGNAME)
	$(RM) *.hi *.o


