#include "intcode.h"

void *get(grid *grid, int x, int y) { return (word *)grid->data + grid->height * x + y; }
void put(grid *grid, int x, int y, void *val) { *((word *)grid->data + grid->height * x + y) = *(word *)val; }
bool word2bool(void *val) { return *(word *)val != 0; }

int main() {
  word panel[200][200] = {0};
  int x = 100, y = 100;
  state *s = readprog();
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

/*
  printf("\n");
  for (int j = 100; j < 106; j++) {
    for (int i = 101; i < 140; i++) {
      printf("%c", panel[i][j]? '#':' ');
    }
    printf("\n");
  }
*/

  grid grid = { 200, 200, get, put, panel, sizeof(panel) };
  char buf[1024];
  char *b = findletters(buf, &grid, word2bool);
  if (b)
    puts(b);
  return 0;
}
