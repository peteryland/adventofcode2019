#include <unistd.h>
#include <string.h>
#include "intcode.h"

void inp(state *s) {
  static char buf[1024] = {0}, *p = buf;
  if (*p == 0) {
    do {
//      printf("> ");
      fgets(buf, 1024, stdin);
      p = buf;
    } while (buf[0] == '#' || buf[0] == '\n');
  }
  s->input = *p++;
}

void printcb2(state *s) {
  if (s->output > 256) {
    printf("%lld\n", s->output);
  } /* else {
//    if (s->useroutputstate++ > 2) {
      printf("%c", (char)s->output);
//    }
//    if (s->output == 10)
//      s->useroutputstate = 0;
  }
  */
  s->state = NORMAL;
}

int main() {
  state *s = readprogf(fopen("21.input", "r"));
  s->mode = ASYNC;
  s->onoutput = printcb2;
  s->oninput = inp;
  runcode(s);
}
