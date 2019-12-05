#include <stdlib.h>
#include <string.h>
#include "intcode.h"

#define NUMOPS 3

int runcode5(word *a, size_t len, word *input) {
  for (word ip = 0; a[ip] != 99; ) {
    word opcode = a[ip] % 100;
    word *ops[NUMOPS];

    for (int i = 0, j = 100; i < NUMOPS; i++, j*=10) {
      switch (a[ip] / j % 10) {
        case 0: ops[i] = a + a[ip + i + 1]; break;
        case 1: ops[i] = a + ip + i + 1; break;
        default: printf("huh? %d [%d, %d, %d, %d]\n", a[ip], a[ip+1], a[ip+2], a[ip+3], a[ip+4]); return 1;
      }
    }

    switch(opcode) {
      case 1: *ops[2] = *ops[0] + *ops[1]; ip += 4; break; // add
      case 2: *ops[2] = *ops[0] * *ops[1]; ip += 4; break; // mul
      case 3: *ops[0] = *input++; ip += 2; break; // in
      case 4: if (*ops[0]) printf("%d\n", *ops[0]); ip += 2; break; // out
      case 5: ip = *ops[0]? *ops[1] : ip + 3; break; // ifn
      case 6: ip = *ops[0]? ip + 3 : *ops[1]; break; // ifz
      case 7: *ops[2] = *ops[0] < *ops[1]; ip += 4; break; // lt
      case 8: *ops[2] = *ops[0] == *ops[1]; ip += 4; break; // eq
      default: printf("Bad input at %d (%d)\n", ip, a[ip]); return -1;
    }
  }
  return a[0];
}

int runcode(word *a, size_t len, word noun, word verb) {
  if (len > 1048576) {
    printf("Array too big\n");
    return a[0];
  }
  word a2[1048576];
  memcpy(a2, a, len * sizeof(*a));
  a2[1] = noun;
  a2[2] = verb;

  return runcode5(a2, len, 0);
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
