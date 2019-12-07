#ifndef _intcode_h_
#define _intcode_h_

#include <stdio.h>

typedef int word;

typedef struct {
  word output, *prog, ip;
} state;

int runcode_basic(word *a, size_t len, word noun, word verb);
int runcode(word *a, size_t len, word *input);
state *runcode_new(word *a, size_t len, word *input);
state *resume(state *s, word *input);
int readprog(FILE *in, word *a);
int readprogs(word **a);

#endif
