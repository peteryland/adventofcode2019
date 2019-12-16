#ifndef _intcode_h_
#define _intcode_h_

#include <stdio.h>

typedef long long word;

typedef struct state_t {
  enum { SYNC, ASYNC } mode;
  enum { NORMAL, ERROR, HAVEOUTPUT, NEEDINPUT, INPUTGIVEN } state;
  word output, *prog;
  size_t len;
  word ip, relbase, input;
  word useroutputstate;
  word userstateword;
  void *userstate;
  size_t userlen;
  void (*oninput)(struct state_t *s);
  void (*onoutput)(struct state_t *s);
} state;

typedef void (*rccb)(state *s);

state *readprogf(FILE *in);
state *readprog();
state *copystate(state *s);
void runcode(state *s);
void runinput(state *s, word input);
void printcb(state *s);
void freestate(state *s);

#endif
