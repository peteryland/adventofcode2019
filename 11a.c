#include "intcode.h"

int main() {
  int panelpainted[200][200] = {0};
  word panel[200][200] = {0};
  int x = 100, y = 100;
  word *a, *input;
  size_t len = readprogs(&a);
  int numpainted = 0;
  int direction = 0;

  input = &(panel[x][y]);
  state *s = runcode_new(a, len, input);
  do {
    if (panelpainted[x][y] == 0)
      numpainted++;
    panelpainted[x][y] = 1;
    panel[x][y] = s->output;
    s = resume(s, input);
    if (s == 0)
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
      default: printf("weird\n");
    }

    input = &(panel[x][y]);
    s = resume(s, input);
  } while (s);

  printf("%d\n", numpainted);
  return 0;
}
