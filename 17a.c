#include "intcode.h"

#define SIZE 50

void outp(state *s) {
  char *x = (char*)&(s->userstateword), *y = x + 1;
  char *rx = (char*)&(s->useroutputstate), *ry = rx + 1, *rd = ry + 1;
  char *gridpos = ((char *)s->userstate) + *y * SIZE + *x;
  switch (s->output) {
    case '\n': *gridpos = 0; (*y)++; *x = 0; break;
    case '.': *gridpos = 0; (*x)++; break;
    case '<': (*rd)++; // fall through
    case 'v': (*rd)++; // fall through
    case '>': (*rd)++; // fall through
    case '^': *rx = *x; *ry = *y; // fall through
    default: *gridpos = 1; (*x)++; break;
  }
}

inline int getsig(char grid[SIZE][SIZE], int i, int j) {
  int sig = 0;
  for (int y = j - 1; y <= j + 1; y++) {
    for (int x = i - 1; x <= i + 1; x++) {
      sig *= 2;
      sig += grid[x][y];
    }
  }
  return sig;
}

int main() {
  char grid[SIZE][SIZE] = {0};
  state *s = readprog();
  s->mode = ASYNC;
  s->userstate = grid;
  s->userlen = sizeof(grid);
  s->onoutput = outp;
  runcode(s);

  int total = 0;
  for (int j = 1; j < SIZE - 1; j++) {
    for (int i = 1; i < SIZE - 1; i++) {
      if (getsig(grid, i, j) == 0xba)
        total += i * j;
    }
  }
  printf("%d\n", total);
  return 0;
}
