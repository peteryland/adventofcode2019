FLAGS = -fPIC -O2 -g
CFLAGS = $(FLAGS)
HFLAGS = $(FLAGS) -threaded -rtsopts -v0
DONEHS1 = $(wildcard [0-9][ab].hs)
DONEHS2 = $(wildcard [0-9][0-9][ab].hs)
DONEC1 = $(wildcard [0-9][ab].c)
DONEC2 = $(wildcard [0-9][0-9][ab].c)

all: $(sort $(DONEHS1:%.hs=%h.output) $(DONEC1:%.c=%c.output)) $(sort $(DONEHS2:%.hs=%h.output) $(DONEC2:%.c=%c.output)) | 13ic 15ic 17ic
	@for output in $^; do /bin/echo -n "$$output: "; cat "$$output"; done

clean:
	@rm -f -- [0-9][0-9][abi][hc] [0-9][abi][hc] *.o *.hi *.so *.a
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

21%c.output: 21ic 21.input 21%.ss
	@./$< < 21$*.ss > $@

%h:: %.hs
	ghc $(HFLAGS) -o $@ $^
	@rm -f -- $*.hi $*.o

%c:: %.c
	gcc $(CFLAGS) -o $@ $^
	@rm -rf -- $@.dSYM

13ic: 13b.c intcode.o
	gcc $(CFLAGS) -D_interactive -o $@ $^
	@rm -rf -- $@.dSYM

intcode.o: intcode.c
	gcc $(CFLAGS) -c -o $@ $<

2ac 2bc 5ac 5bc 7ac 7bc 9ac 9bc 11ac 11bc 13ac 13bc 15ac 15bc 15ic 17ac 17bc 17ic 19ac 19bc 21ic 23ac 23bc: intcode.o
11bc: p.o

.PRECIOUS: %h %c

.PHONY: all clean distclean test updatetest
