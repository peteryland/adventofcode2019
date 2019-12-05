#ifndef _intcode_h_
#define _intcode_h_

#include <stdio.h>

typedef int word;

int runcode(word *a, size_t len, word noun, word verb);
int runcode5(word *a, size_t len, word *input);
int readprog(FILE *in, word *a);
int readprogs(word **a);

#endif
