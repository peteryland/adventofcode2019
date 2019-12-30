#include "p.h"

int main() {
  static char grid[402*25] = {0}, *p = grid;
  static char nextgrid[402*25] = {0};
  static int seen[1<<25] = {0};
  int totalbugs;

  for (int i = 0; i < 5; i++) {
    fgets(grid+200*25+i*5, 10, stdin);
  }

  for (int k = 0; k < 200; k++) {

    for (int n = 0; n < 402; n++) {
      for (int i = 0; i < 25; i++) {
        int numbugs = 0;
        switch (i) {
          case 0:  numbugs = (grid[n*25 + 1] == '#') + (grid[n*25 + 5] == '#') + (grid[(n-1)*25 + 11] == '#') + (grid[(n-1)*25 + 7] == '#'); break;
          case 1:  numbugs = (grid[n*25 + 0] == '#') + (grid[n*25 + 2] == '#') + (grid[n*25 + 6] == '#') + (grid[(n-1)*25 + 7] == '#'); break;
          case 2:  numbugs = (grid[n*25 + 1] == '#') + (grid[n*25 + 3] == '#') + (grid[n*25 + 7] == '#') + (grid[(n-1)*25 + 7] == '#'); break;
          case 3:  numbugs = (grid[n*25 + 2] == '#') + (grid[n*25 + 4] == '#') + (grid[n*25 + 8] == '#') + (grid[(n-1)*25 + 7] == '#'); break;
          case 4:  numbugs = (grid[n*25 + 3] == '#') + (grid[n*25 + 9] == '#') + (grid[(n-1)*25 + 13] == '#') + (grid[(n-1)*25 + 7] == '#'); break;
          case 5:  numbugs = (grid[n*25 + 0] == '#') + (grid[n*25 + 6] == '#') + (grid[n*25 + 10] == '#') + (grid[(n-1)*25 + 11] == '#'); break;
          case 6:  numbugs = (grid[n*25 + 1] == '#') + (grid[n*25 + 5] == '#') + (grid[n*25 + 7] == '#') + (grid[n*25 + 11] == '#'); break;
          case 7:  numbugs = (grid[n*25 + 2] == '#') + (grid[n*25 + 6] == '#') + (grid[n*25 + 8] == '#'); for (int k = 0; k < 5; k++) numbugs += (grid[(n+1)*25 + k] == '#'); break;
          case 8:  numbugs = (grid[n*25 + 3] == '#') + (grid[n*25 + 7] == '#') + (grid[n*25 + 9] == '#') + (grid[n*25 + 13] == '#'); break;
          case 9:  numbugs = (grid[n*25 + 4] == '#') + (grid[n*25 + 8] == '#') + (grid[n*25 + 14] == '#') + (grid[(n-1)*25 + 13] == '#'); break;
          case 10: numbugs = (grid[n*25 + 5] == '#') + (grid[n*25 + 11] == '#') + (grid[n*25 + 15] == '#') + (grid[(n-1)*25 + 11] == '#'); break;
          case 11: numbugs = (grid[n*25 + 6] == '#') + (grid[n*25 + 10] == '#') + (grid[n*25 + 16] == '#'); for (int k = 0; k < 5; k++) numbugs += (grid[(n+1)*25 + k*5] == '#'); break;
          case 12: break;
          case 13: numbugs = (grid[n*25 + 8] == '#') + (grid[n*25 + 14] == '#') + (grid[n*25 + 18] == '#'); for (int k = 0; k < 5; k++) numbugs += (grid[(n+1)*25 + k*5 + 4] == '#'); break;
          case 14: numbugs = (grid[n*25 + 9] == '#') + (grid[n*25 + 13] == '#') + (grid[n*25 + 19] == '#') + (grid[(n-1)*25 + 13] == '#'); break;
          case 15: numbugs = (grid[n*25 + 10] == '#') + (grid[n*25 + 16] == '#') + (grid[n*25 + 20] == '#') + (grid[(n-1)*25 + 11] == '#'); break;
          case 16: numbugs = (grid[n*25 + 11] == '#') + (grid[n*25 + 15] == '#') + (grid[n*25 + 17] == '#') + (grid[n*25 + 21] == '#'); break;
          case 17: numbugs = (grid[n*25 + 16] == '#') + (grid[n*25 + 18] == '#') + (grid[n*25 + 22] == '#'); for (int k = 0; k < 5; k++) numbugs += (grid[(n+1)*25 + 4*5 + k] == '#'); break;
          case 18: numbugs = (grid[n*25 + 13] == '#') + (grid[n*25 + 17] == '#') + (grid[n*25 + 19] == '#') + (grid[n*25 + 23] == '#'); break;
          case 19: numbugs = (grid[n*25 + 14] == '#') + (grid[n*25 + 18] == '#') + (grid[n*25 + 24] == '#') + (grid[(n-1)*25 + 13] == '#'); break;
          case 20: numbugs = (grid[n*25 + 15] == '#') + (grid[n*25 + 21] == '#') + (grid[(n-1)*25 + 11] == '#') + (grid[(n-1)*25 + 17] == '#'); break;
          case 21: numbugs = (grid[n*25 + 16] == '#') + (grid[n*25 + 20] == '#') + (grid[n*25 + 22] == '#') + (grid[(n-1)*25 + 17] == '#'); break;
          case 22: numbugs = (grid[n*25 + 17] == '#') + (grid[n*25 + 21] == '#') + (grid[n*25 + 23] == '#') + (grid[(n-1)*25 + 17] == '#'); break;
          case 23: numbugs = (grid[n*25 + 18] == '#') + (grid[n*25 + 22] == '#') + (grid[n*25 + 24] == '#') + (grid[(n-1)*25 + 17] == '#'); break;
          case 24: numbugs = (grid[n*25 + 19] == '#') + (grid[n*25 + 23] == '#') + (grid[(n-1)*25 + 13] == '#') + (grid[(n-1)*25 + 17] == '#'); break;
          default: break;
        }
        nextgrid[n*25+i] = (numbugs == 1 || (grid[n*25+i] != '#' && numbugs == 2 ))? '#':'.';
      }
    }
    totalbugs = 0;
    for (int i = 0; i < 402*25; i++) {
      grid[i] = nextgrid[i];
      totalbugs += grid[i] == '#';
    }
  }
  printf("%d\n", totalbugs);
  return 0;
}
