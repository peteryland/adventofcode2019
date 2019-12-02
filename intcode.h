#ifndef _intcode_h_
#define _intcode_h_

#include <stdio.h>

typedef int word;

int runcode(word *input, size_t len, word noun, word verb);
int readprog(FILE *in, word *a);
int readprogs(word **a);

#endif
