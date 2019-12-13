#include "intcode.h"

int main() {
  state *s = readprog();
  s->input = 2;
  s->onoutput = printcb;
  runcode(s);
  printf("\n");
}
