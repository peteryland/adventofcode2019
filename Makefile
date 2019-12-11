FLAGS = -fPIC -O2 -g
CFLAGS = $(FLAGS)
HFLAGS = $(FLAGS) -threaded -rtsopts -v0
DONEHS1 = $(wildcard [0-9][a-z].hs)
DONEHS2 = $(wildcard [0-9][0-9][a-z].hs)
DONEC1 = $(wildcard [0-9][a-z].c)
DONEC2 = $(wildcard [0-9][0-9][a-z].c)

all: $(sort $(DONEHS1:%.hs=%h.output) $(DONEC1:%.c=%c.output)) $(sort $(DONEHS2:%.hs=%h.output) $(DONEC2:%.c=%c.output))
	@for output in $^; do /bin/echo -n "$$output: "; cat "$$output"; done

clean:
	@rm -f -- [0-9][0-9][ab][hc] [0-9][ab][hc] *.o *.hi *.so *.a
	@rm -rf -- *.dSYM

distclean: clean
	@rm -f -- *.output

test:
	@make > .output
	@diff -u .expected_output .output

updatetest:
	@make > /dev/null
	@make > .expected_output

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

2ah 2bh 2ac 2bc 5ac 5bc 7ac 7bc 9ac 9bc 11ac 11bc: intcode.o
2ah 2bh: IntCodeHS.hs

.PRECIOUS: %h %c

.PHONY: all clean distclean test updatetest
