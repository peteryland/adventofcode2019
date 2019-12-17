#include "intcode.h"

void printcb2(state *s) {
  if (s->output != 0) {
    printf("%s%lld", s->useroutputstate?",":"", s->output);
    s->useroutputstate = 1;
  }
  s->state = NORMAL;
}

int main() {
  state *s = readprog();
  s->mode = ASYNC;
  s->input = 1;
  s->onoutput = printcb2;
  runcode(s);
  printf("\n");
}
