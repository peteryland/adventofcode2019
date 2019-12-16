#include <unistd.h>
#include <termios.h>
#include "intcode.h"

#define SIZE 50
enum direction { Q, N, S, W, E };
enum status { WALL, OK, OXY };
enum gridstate { NOTVISITED, VISITED, HASOXY, HASWALL };
enum gridstate grid[SIZE][SIZE];
char gridstate2char[] = " .O#";

void drawgrid(enum gridstate grid[SIZE][SIZE], int x, int y, int *bl, int *bp) {
  printf("[2J[0;0H");
  for (int j = 0; j < SIZE; j++) {
    for (int i = 0; i < SIZE; i++) {
      char c;
      if (i == x && j == y)
        c = 'D';
      else
        c = gridstate2char[grid[i][j]];
      putchar(c);
    }
    putchar('\n');
  }
  for (int *p = bl; p < bp; p++) {
    putchar("QNSWE"[*p]);
  }
  putchar('\n');
  fflush(0);
  usleep(5000);
}

// This could have been done more easily with recursion, but it's fun to
// explicity see the bearings stack too

int main() {
  int bearinglist[SIZE*SIZE];
  int num_movements = 0;
  int x = SIZE/2, y = SIZE/2;
  state *s = readprog();
  int *bearing = bearinglist;
  grid[x][y] = VISITED;
  do {
    int i = N;
    do {
      for (; i <= E; i++) {
        int x1 = x, y1 = y;
        switch (i) {
          case N: y1--; break; case E: x1++; break;
          case S: y1++; break; case W: x1--; break;
        }
        if (grid[x1][y1] == NOTVISITED) goto out;
      }
      // dead end, backtrack
      if (bearing == bearinglist) { // no more backtracks possible
        printf("-1\n");
        return 0;
      }
      bearing--;
      i = ((*bearing - 1) ^ 1) + 1; // N <-> S, E <-> W
      runinput(s, i); // don't check output, this should always work unless the maze changes
      switch (i) {
        case N: y--; break; case E: x++; break;
        case S: y++; break; case W: x--; break;
      }
      num_movements--;
//      drawgrid(grid, x, y, bearinglist, bearing);
    } while (1);
out:

    runinput(s, i);
    int x1 = x, y1 = y;
    switch (i) {
      case N: y1--; break; case E: x1++; break;
      case S: y1++; break; case W: x1--; break;
    }
    switch (s->output) {
      case WALL: grid[x1][y1] = HASWALL; break;
      case OXY: grid[x1][y1] = HASOXY; // fall through case to move as well
      default: x = x1; y = y1; grid[x][y] = VISITED; num_movements++; *bearing++ = i; break;
    }
//    drawgrid(grid, x, y, bearinglist, bearing);
  } while (s->state != NORMAL && s->output != OXY);
  printf("%d\n", num_movements);
  return 0;
}
