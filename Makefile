FLAGS = -fPIC -O2 -g
CFLAGS = $(FLAGS)
HFLAGS = $(FLAGS) -threaded -rtsopts -v0
DONEHS = $(wildcard [0-9]*.hs)
DONEC = $(wildcard [0-9]*.c)

all: $(sort $(DONEHS:%.hs=%h.output) $(DONEC:%.c=%c.output))
	@for output in $^; do /bin/echo -n "$$output: "; cat "$$output"; done

clean:
	@rm -f -- [0-9][0-9][ab][hc] [0-9][ab][hc] *.o *.hi *.so *.a

distclean: clean
	@rm -f -- *.output

%ah.output: %ah %.input
	@./$< +RTS -N8 < $*.input > $@

%bh.output: %bh %.input
	@./$< +RTS -N8 < $*.input > $@

%ac.output: %ac %.input
	@./$< < $*.input > $@

%bc.output: %bc %.input
	@./$< < $*.input > $@

%h:: %.hs
	ghc $(HFLAGS) -o $@ $^
	@rm -f -- $*.hi $*.o

%c:: %.c
	gcc $(CFLAGS) -o $@ $^
	@rm -rf -- $@.dSYM

intcode.o: intcode.c
	gcc $(CFLAGS) -c -o $@ $<

2ah 2bh 2ac 2bc: intcode.o
5ah 5bh 5ac 5bc: intcode.o

.PRECIOUS: %h %c

.PHONY: all clean distclean
