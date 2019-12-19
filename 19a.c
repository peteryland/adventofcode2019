#include "intcode.h"

#define SIZE 50

int main() {
  char mygrid[SIZE][SIZE] = {0};
  state *s0 = readprog(), *s;

  int count = 0;
  for (int y = 0; y < SIZE; y++) {
    for (int x = 0; x < SIZE; x++) {
      s = copystate(s0);
      runinput(s, x);
      runinput(s, y);
      count += s->output;
    }
  }
  for (int i = 0; i < SIZE; i++)
    runcode(s);

  printf("%d\n", count);
  return 0;
}
