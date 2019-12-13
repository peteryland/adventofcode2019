#include "intcode.h"

int main() {
  state *s = readprog();
  s->input = 5;
  s->onoutput = printcb;
  runcode(s);
  printf("\n");
}
