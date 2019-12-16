#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include "intcode.h"

#define SIZE 50
enum direction { Q, N, S, W, E };
enum status { WALL, OK, OXY };
enum gridstate { NOTVISITED, VISITED, HASOXY, HASWALL };
enum gridstate grid[SIZE][SIZE];
char gridstate2char[] = " .O#";

void drawgrid(enum gridstate grid[SIZE][SIZE], int x, int y) {
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
  fflush(0);
}

int getchardirect() {
  struct termios tty_opts_backup, tty_opts_raw;
  int istty = isatty(STDIN_FILENO);
  if (istty) {
    tcgetattr(STDIN_FILENO, &tty_opts_backup); // Back up current TTY settings
    cfmakeraw(&tty_opts_raw);
    tcsetattr(STDIN_FILENO, TCSANOW, &tty_opts_raw); // Change TTY settings to raw mode
  }
    int c = getchar();
  if (istty) {
    tcsetattr(STDIN_FILENO, TCSANOW, &tty_opts_backup); // Restore previous TTY settings
  }
    return c;
}

int getinput() {
  do {
    int c = getchardirect();
    switch(c) {
      case 104: return W; // h
      case 106: return S; // j
      case 107: return N; // k
      case 108: return E; // l
      case 113: return -1; // q
      default: break; // ignore anything else
    }
  } while (1);
}

int main() {
  int x = SIZE/2, y = SIZE/2;
  state *s = readprogf(fopen("15.input", "r"));
  int bearing;
  grid[x][y] = VISITED;
  do {
    drawgrid(grid, x, y);
    bearing = getinput();
    if (bearing < 0)
      break;
    runinput(s, bearing);
    int x1 = x, y1 = y;
    switch (bearing) {
      case N: y1--; break;
      case E: x1++; break;
      case S: y1++; break;
      case W: x1--; break;
    }
    switch (s->output) {
      case WALL: grid[x1][y1] = HASWALL; break;
      case OXY: grid[x1][y1] = HASOXY; // fall through case to move as well
      default: x = x1; y = y1; grid[x][y] = VISITED; break;
    }
  } while (s->state != NORMAL && s->output != OXY);
  drawgrid(grid, 10000, 10000); // at the end, don't show the robot position
  return 0;
}
