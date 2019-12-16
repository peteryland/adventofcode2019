#include "intcode.h"

int grid[1000][1000] = {0};

int main() {
  int blocks = 0;
  state *s = readprog();
  do {
    runcode(s);
    word x = s->output;
    runcode(s);
    word y = s->output;
    runcode(s);
    word t = s->output;
    if (grid[x][y] == 2)
      blocks--;
    if (t == 2)
      blocks++;
    grid[x][y] = t;
  } while (s->state != NORMAL);
  printf("%d\n", blocks);
  return 0;
}
