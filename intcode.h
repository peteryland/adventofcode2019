#ifndef _intcode_h_
#define _intcode_h_

#include <stdio.h>

typedef long long word;

typedef struct {
  word output, *prog, ip, relbase;
} state;

word runcode_basic(word *a, size_t len, word noun, word verb);
word runcode(word *a, size_t len, word *input);
state *runcode_new(word *a, size_t len, word *input);
state *resume(state *s, word *input);
size_t readprog(FILE *in, word *a);
size_t readprogs(word **a);

#endif
