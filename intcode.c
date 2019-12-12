#include <stdlib.h>
#include <string.h>
#include "intcode.h"

#define MAXOPERANDS 3

word runcode_basic(word *a, size_t len, word noun, word verb) {
  state *s = calloc(1, sizeof(state));
  s->ip = 0;
  s->prog = calloc(len, sizeof(word));
  memcpy(s->prog, a, len * sizeof(word));
  s->relbase = 0;
  s->prog[1] = noun;
  s->prog[2] = verb;

  for (state *p = s; p; p = resume(s, 0));
  return s->prog[0];
}

word runcode(word *a, size_t len, word *input) {
  int isfirst = 1;
  state *s = runcode_new(a, len, input);

  for (state *p = s; p; p = resume(s, input)) {
    printf("%s%lld", isfirst?"":",", s->output);
    isfirst = 0;
  }
  printf("\n");
  return s->output;
}

state *runcode_new(word *a, size_t len, word *input) {
  state *s = calloc(1, sizeof(state));
  s->ip = 0;
  s->prog = calloc(len, sizeof(word));
  memcpy(s->prog, a, len * sizeof(word));
  s->relbase = 0;

  return resume(s, input);
}

state *resume(state *s, word *input) {
  for (word *a = s->prog; a[s->ip] != 99; ) {
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
      case 3: *ops[0] = *input++; s->ip += 2; break; // in
      case 4: s->output = *ops[0]; s->ip += 2; return s; break; // out
      case 5: s->ip = *ops[0]? *ops[1] : s->ip + 3; break; // ifn
      case 6: s->ip = *ops[0]? s->ip + 3 : *ops[1]; break; // ifz
      case 7: *ops[2] = *ops[0] < *ops[1]; s->ip += 4; break; // lt
      case 8: *ops[2] = *ops[0] == *ops[1]; s->ip += 4; break; // eq
      case 9: s->relbase += *ops[0]; s->ip += 2; break; // arb
    }
  }
  return 0;
}

size_t readprog(FILE *in, word *a) {
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
  return len;
}

size_t readprogs(word **prog) {
  if (prog == 0) return -1;
  word a[10240];
  size_t len = readprog(stdin, a);

  *prog = calloc(sizeof(word), 4 * len);
  if (*prog == 0) return -2;
  memcpy(*prog, a, sizeof(word) * len);
  return 4 * len;
}
