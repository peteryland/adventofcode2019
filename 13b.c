#include <unistd.h>
#include <termios.h>
#include "intcode.h"

//#define _interactive

int grid[100][100] = {0};

#ifdef _interactive
int getchardirect() {
  struct termios tty_opts_backup, tty_opts_raw;
  int istty = isatty(STDIN_FILENO);
  if (istty) {
    tcgetattr(STDIN_FILENO, &tty_opts_backup); // Back up current TTY settings
    cfmakeraw(&tty_opts_raw);
    tcsetattr(STDIN_FILENO, TCSANOW, &tty_opts_raw); // Change TTY settings to raw mode
    int c = getchar();
    tcsetattr(STDIN_FILENO, TCSANOW, &tty_opts_backup); // Restore previous TTY settings
    return c;
  }
  return 'k'; // Don't block when running non-interactively
}

void getinput(state *s, word ballx, word paddlex) {
  int c;
  fflush(0);
  do {
    c = getchardirect();
  } while (c < 106 || c > 108);
  runinput(s, c-107);
}
#else
void getinput(state *s, word ballx, word paddlex) {
  runinput(s, ballx == paddlex? 0 : (ballx < paddlex? -1 : 1));
}
#endif

char chars[] = " #*-o";

int main() {
  word score = 0;
#ifdef _interactive
  state *s = readprogf(fopen("13.input", "r"));
#else
  state *s = readprog();
#endif
  s->mode = SYNC;
  s->prog[0] = 2;
#ifdef _interactive
  printf("[2J");
#endif
  word xyt[] = {0, 0, 0}, xyts = 0;
  runcode(s);
  word ballx = 0, paddlex = 0;
  do {
    switch(s->state) {
      case NORMAL:
#ifdef _interactive
        printf("[2J[0;0HYour score was: ");
#endif
        printf("%lld\n", score);
        return 0;
      case NEEDINPUT:
        getinput(s, ballx, paddlex);
        break;
      case HAVEOUTPUT:
        xyt[xyts++] = s->output;
        xyts %= 3;
        if (xyts == 0) {
          word x = xyt[0], y = xyt[1], t = xyt[2];
          if (x == -1 && y == 0) {
            score = t;
#ifdef _interactive
            printf("[1;40H[2K%5lld[0;0H", score);
#endif
          } else {
            grid[x][y] = t;
            if (t == 3) paddlex = x;
            if (t == 4) ballx = x;
#ifdef _interactive
            printf("[%lld;%lldH%c[0;0H", y+2, x+1, chars[t]);
#endif
          }
        }
        runcode(s);
        break;
      default:
        return 1;
    }
  } while (1);
}
