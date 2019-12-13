#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "intcode.h"

int main() {
  state *s = readprog();
  s->prog[1] = 12;
  s->prog[2] = 2;
  runcode(s);
  printf("%lld\n", s->prog[0]);
}
