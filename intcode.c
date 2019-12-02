#include <stdlib.h>
#include <string.h>
#include "intcode.h"

int runcode(word *input, size_t len, word noun, word verb) {
  if (len > 1048576) {
    printf("Array too big\n");
    return input[0];
  }
  word a[1048576];
  memcpy(a, input, len * sizeof(*input));
  a[1] = noun;
  a[2] = verb;

  for (int ip = 0; a[ip] != 99; ) {
    word opcode = a[ip];
    word *op1 = a + a[ip+1];
    word *op2 = a + a[ip+2];
    word *op3 = a + a[ip+3];
    word *op4 = a + a[ip+4];
    word *op5 = a + a[ip+5];

    switch(opcode) {
      case 1:
        *op3 = *op1 + *op2;
        ip += 4;
        break;
      case 2:
        *op3 = *op1 * *op2;
        ip += 4;
        break;
      default:
        printf("Bad input: %d, %d\n", noun, verb);
        return a[0];
    }
  }
  return a[0];
}

int readprog(FILE *in, word *a) {
  size_t len = 0;
  word *p = a;

  char line[10485760], *linep;
  char *token;

  while (fgets(line, sizeof line, in) != 0) {
    for (linep = line; (token = strsep(&linep, ", \n")) != 0; ) {
      if (linep == 0 || *linep != '\0') {
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
