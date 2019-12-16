#include "intcode.h"

int main() {
  state *s = readprog();
  s->mode = ASYNC;
  s->input = 2;
  s->onoutput = printcb;
  runcode(s);
  printf("\n");
}
