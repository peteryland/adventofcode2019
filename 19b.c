#include "intcode.h"

#define SIZE 2000

int try(state *s0, int y) {
  state *s;
  int seenones = 0;
  for (int x = 0; x < SIZE; x++) {
    s = copystate(s0);
    runinput(s, x);
    runinput(s, y);
    if (s->output == 1) {
      if (!seenones) seenones = 1;
    } else {
      if (seenones) {
        freestate(s);
        s = copystate(s0);
        runinput(s, x - 100);
        runinput(s, y + 99);
        if (s->output)
          return (x-100) * 10000 + y;
        break;
      }
    }
    freestate(s);
  }
  return 0;
}

int main() {
  state *s0 = readprog();
  int r = binarysearch0((tryfunc)try, s0);
  printf("%d\n", r);
  return 0;
}
