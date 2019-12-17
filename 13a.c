#include "intcode.h"

int mygrid[1000][1000] = {0};

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
    if (mygrid[x][y] == 2)
      blocks--;
    if (t == 2)
      blocks++;
    mygrid[x][y] = t;
  } while (s->state != NORMAL);
  printf("%d\n", blocks);
  return 0;
}
