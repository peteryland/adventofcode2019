#include <stdlib.h>
#include <string.h>
#include "intcode.h"

#define NUMOPS 3

int runcode_basic(word *a, size_t len, word noun, word verb) {
  if (len > 1048576) { printf("Array too big\n"); return 0; }
  state *s = calloc(1, sizeof(state));
  s->ip = 0;
  s->prog = calloc(len, sizeof(word));
  memcpy(s->prog, a, len * sizeof(word));
  s->prog[1] = noun;
  s->prog[2] = verb;

  for (state *p = s; p; p = resume(s, 0));
  return s->prog[0];
}

int runcode(word *a, size_t len, word *input) {
  state *s = calloc(1, sizeof(state));
  s->ip = 0;
  s->prog = a;

  for (state *p = s; p; p = resume(s, input));
  return s->output;
}

state *runcode_new(word *a, size_t len, word *input) {
  if (len > 1048576) { printf("Array too big\n"); return 0; }
  state *s = calloc(1, sizeof(state));
  s->ip = 0;
  s->prog = calloc(len, sizeof(word));
  memcpy(s->prog, a, len * sizeof(word));

  return resume(s, input);
}

state *resume(state *s, word *input) {
  word *a = s->prog;

  for (word ip = s->ip; a[ip] != 99; ) {
    word opcode = a[ip] % 100;
    word *ops[NUMOPS];

    //printf("%d %d %d %d %d\n", a[ip], opcode, a[ip] / 100 % 10, a[ip] / 1000 % 10, a[ip] / 10000 % 10);

    for (int i = 0, j = 100; i < NUMOPS; i++, j*=10) {
      switch (a[ip] / j % 10) {
        case 0: ops[i] = a + a[ip + i + 1]; break;
        case 1: ops[i] = a + ip + i + 1; break;
        default: printf("huh? %d [%d, %d, %d, %d]\n", a[ip], a[ip+1], a[ip+2], a[ip+3], a[ip+4]); return 0;
      }
    }
    //printf("Processing opcode at %d (%d): %d (%d), %d (%d), %d\n", ip, a[ip], a[ip+1], *ops[0], a[ip+2], *ops[1], a[ip+3]);
    //printf("Processing opcode at %d (%d): %d %d %d\n", ip, a[ip], a[ip+1], a[ip+2], a[ip+3]);

    switch(opcode) {
      case 1: *ops[2] = *ops[0] + *ops[1]; ip += 4; break; // add
      case 2: *ops[2] = *ops[0] * *ops[1]; ip += 4; break; // mul
      case 3: *ops[0] = *input++; ip += 2; break; // in
      case 4: s->output = *ops[0]; s->ip = ip + 2; return s; break; // out
      case 5: ip = *ops[0]? *ops[1] : ip + 3; break; // ifn
      case 6: ip = *ops[0]? ip + 3 : *ops[1]; break; // ifz
      case 7: *ops[2] = *ops[0] < *ops[1]; ip += 4; break; // lt
      case 8: *ops[2] = *ops[0] == *ops[1]; ip += 4; break; // eq
      default: printf("Bad input at %d (%d)\n", ip, a[ip]); return 0;
    }
  }
  return 0;
}

int readprog(FILE *in, word *a) {
  size_t len = 0;
  word *p = a;

  char line[1048576], *linep;
  char *token;

  while (fgets(line, sizeof line, in) != 0) {
    for (linep = line; (token = strsep(&linep, ", \n")) != 0; ) {
      if (*token != '\0') {
        *p++ = atoi(token);
        len++;
      }
    }
  }
  return len;
}

int readprogs(word **prog) {
  if (prog == 0) return -1;
  word a[1048576];
  int len = readprog(stdin, a);

  *prog = calloc(sizeof(word), len);
  if (*prog == 0) return -2;
  memcpy(*prog, a, sizeof(word) * len);
  return len;
}
