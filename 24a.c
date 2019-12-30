#include "p.h"

int main() {
  char grid[30] = {0}, *p = grid;
  char nextgrid[25] = {0};
  static int seen[1<<25] = {0};

  for (int i = 0; i < 5; i++) {
    fgets(grid+i*5, 10, stdin);
  }

  while (1) {

    for (int j = 0; j < 5; j++) {
      for (int i = 0; i < 5; i++) {
        int numbugs = 0;
        if (i > 0 && grid[j*5+i-1] == '#') numbugs++;
        if (j > 0 && grid[(j-1)*5+i] == '#') numbugs++;
        if (i < 4 && grid[j*5+i+1] == '#') numbugs++;
        if (j < 4 && grid[(j+1)*5+i] == '#') numbugs++;
        nextgrid[j*5+i] = (numbugs == 1 || (grid[j*5+i] != '#' && numbugs == 2 ))? '#':'.';
      }
    }
    int points = 0;
    for (int i = 24; i >= 0; i--) {
      grid[i] = nextgrid[i];
      points *= 2;
      points += grid[i] == '#';
    }

    //drawgrid
//    for (int j = 0; j < 5; j++) {
//      for (int i = 0; i < 5; i++) {
//        putchar(grid[j*5+i]);
//      }
//      putchar('\n');
//    }

    if (seen[points]) {
      printf("%d\n", points);
      return 0;
    }
    seen[points] = 1;
  }
}
