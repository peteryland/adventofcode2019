#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "intcode.h"

int main() {
  state *s0 = readprog();

  for (word i = 0; i <= 100; i++) {
    for (word j = 0; j <= 100; j++) {
      state *s = copystate(s0);
      s->prog[1] = i;
      s->prog[2] = j;
      runcode(s);
      if (s->prog[0] == 19690720) {
        printf("%lld%lld\n", i, j);
        return 0;
      }
      freestate(s);
    }
  }
}

