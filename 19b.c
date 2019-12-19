#include "intcode.h"

#define SIZE 2000

int main() {
  char mygrid[SIZE][SIZE] = {0};
  state *s0 = readprog(), *s;

  for (int y = 0; y < SIZE; y++) {
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
          if (s->output) {
            printf("%d\n", (x-100) * 10000 + y);
            return 0;
          }
          break;
        }
      }
      freestate(s);
    }
  }
  return 0;
}
