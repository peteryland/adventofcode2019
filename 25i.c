#include <unistd.h>
#include <string.h>
#include "intcode.h"

void inp(state *s) {
  static char buf[1024] = {0}, *p = buf;
  if (*p == 0) {
    do {
      printf("> ");
      fgets(buf, 1024, stdin);
      p = buf;
    } while (buf[0] == '#' || buf[0] == '\n');
  }
  s->input = *p++;
}

void printcb2(state *s) {
  putchar((char)s->output);
  s->state = NORMAL;
}

int main() {
  state *s = readprogf(fopen("25.input", "r"));
  s->mode = ASYNC;
  s->onoutput = printcb2;
  s->oninput = inp;
  runcode(s);
}
