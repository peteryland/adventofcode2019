#include <stdlib.h>
#include <string.h>
#include "intcode.h"

#define MAXOPERANDS 3

state *copystate(state *s0) {
  state *s = calloc(1, sizeof(state));
  s->mode = s0->mode;
  s->state = s0->state;
  s->output = s0->output;
  s->prog = calloc(4 * s0->len, sizeof(word));
  memcpy(s->prog, s0->prog, s0->len * sizeof(word));
  s->len = s0->len;
  s->ip = s0->ip;
  s->relbase = s0->relbase;
  s->input = s0->input;
  s->useroutputstate = s0->useroutputstate;
  s->userstateword = s0->userstateword;
  if (s0->userlen) {
    s->userstate = calloc(1, s0->userlen);
    memcpy(s->userstate, s0->userstate, s0->userlen);
  } else {
    s->userstate = s0->userstate;
  }
  s->userlen = s0->userlen;
  return s;
}

void printstate(state *s) {
  printf("%d %lld %lld %lld %lld\n", s->state, s->ip, s->input, s->output, s->prog[s->ip]);
}

// Doesn't free user state
void freestate(state *s) {
  if (s) {
    if (s->prog)
      free(s->prog);
    free(s);
  }
}

void printcb(state *s) {
  printf("%s%lld", s->useroutputstate?",":"", s->output);
  s->useroutputstate = 1;
  s->state = NORMAL;
}

//word getoutput(state *s) {
  //return s->output;
//}

void runinput(state *s, word input) {
  s->input = input;
  s->state = INPUTGIVEN;
  runcode(s);
}

void runcode(state *s) {
  for (word *a = s->prog; a[s->ip] != 99; ) {
    //printstate(s);
    word opcode = a[s->ip] % 100;
    word *ops[MAXOPERANDS];

    for (word i = 0, j = 100; i < MAXOPERANDS; i++, j*=10) {
      switch (a[s->ip] / j % 10) {
        case 0: ops[i] = a + a[s->ip + i + 1]; break;
        case 1: ops[i] = a + s->ip + i + 1; break;
        case 2: ops[i] = a + s->relbase + a[s->ip + i + 1]; break;
      }
    }

    switch(opcode) {
      case 1: *ops[2] = *ops[0] + *ops[1]; s->ip += 4; break; // add
      case 2: *ops[2] = *ops[0] * *ops[1]; s->ip += 4; break; // mul
      case 3: // in
        if (s->mode == SYNC) {
          if (s->state != INPUTGIVEN) {
            s->state = NEEDINPUT;
            return;
          }
          s->state = NORMAL;
        } else {
          if (s->oninput) {
            s->oninput(s);
          }
        }
        *ops[0] = s->input;
        s->ip += 2;
        break;
      case 4: // out
        s->output = *ops[0];
        s->ip += 2;
        if (s->mode == SYNC) {
          s->state = HAVEOUTPUT;
          return;
        }
        if (s->onoutput) {
          s->onoutput(s);
        }
        break;
      case 5: s->ip = *ops[0]? *ops[1] : s->ip + 3; break; // ifn
      case 6: s->ip = *ops[0]? s->ip + 3 : *ops[1]; break; // ifz
      case 7: *ops[2] = *ops[0] < *ops[1]; s->ip += 4; break; // lt
      case 8: *ops[2] = *ops[0] == *ops[1]; s->ip += 4; break; // eq
      case 9: s->relbase += *ops[0]; s->ip += 2; break; // arb
      default: s->state = ERROR; return;
    }
  }
  s->state = NORMAL;
}

state *readprog() {
  return readprogf(stdin);
}

state *readprogf(FILE *in) {
  state *s = calloc(1, sizeof(state));
  word a[10240];
  size_t len = 0;
  word *p = a;

  char line[1048576], *linep;
  char *token;

  while (fgets(line, sizeof line, in) != 0) {
    for (linep = line; (token = strsep(&linep, ", \n")) != 0; ) {
      if (*token != '\0') {
        *p++ = atol(token);
        len++;
      }
    }
  }

  s->mode = SYNC;
  s->state = NORMAL;
  s->output = 0;
  s->prog = calloc(sizeof(word), 4 * len);
  memcpy(s->prog, a, sizeof(word) * len);
  s->len = len;
  s->ip = 0;
  s->relbase = 0;
  s->input = 0;
  s->useroutputstate = 0;
  s->userstateword = 0;
  s->userstate = 0;
  s->userlen = 0;
  s->oninput = 0;
  s->onoutput = 0;
  return s;
}
