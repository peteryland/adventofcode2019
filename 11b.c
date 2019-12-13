#include "intcode.h"

int main() {
  word panel[200][200] = {0};
  int x = 100, y = 100;
  state *s = readprog();
  s->mode = SYNC;
  int direction = 0;

  panel[x][y] = 1;
  runinput(s, panel[x][y]);
  do {
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
  } while (s);

  printf("\n");
  for (int i = 100; i<106; i++) {
    for (int j = 101; j<140; j++) {
      printf("%c", panel[j][i]? '#':' ');
    }
    printf("\n");
  }
  return 0;
}
