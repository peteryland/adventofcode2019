#include "intcode.h"

int main() {
  int panelpainted[200][200] = {0};
  word panel[200][200] = {0};
  int x = 100, y = 100;
  state *s = readprog();
  s->mode = SYNC;
  int numpainted = 0;
  int direction = 0;

  runinput(s, panel[x][y]);
  do {
    if (panelpainted[x][y] == 0)
      numpainted++;
    panelpainted[x][y] = 1;
    panel[x][y] = s->output;
    runcode(s);
    if (s->state == NORMAL)
      break;
    if (s->output)
      direction++;
    else
      direction--;
    direction &= 3;

    switch (direction) {
      case 0: y--; break;
      case 1: x++; break;
      case 2: y++; break;
      case 3: x--; break;
    }

    runinput(s, panel[x][y]);
  } while (s->state != NORMAL);

  printf("%d\n", numpainted);
  return 0;
}
