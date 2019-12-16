#include "intcode.h"

#define NUMPERMS 120
word perms[120][5] = {
  {5,6,7,8,9} ,{6,5,7,8,9} ,{7,6,5,8,9} ,{6,7,5,8,9} ,{7,5,6,8,9} ,{5,7,6,8,9} ,{8,7,6,5,9} ,{7,8,6,5,9},
  {7,6,8,5,9} ,{8,6,7,5,9} ,{6,8,7,5,9} ,{6,7,8,5,9} ,{8,5,6,7,9} ,{5,8,6,7,9} ,{5,6,8,7,9} ,{8,6,5,7,9},
  {6,8,5,7,9} ,{6,5,8,7,9} ,{8,5,7,6,9} ,{5,8,7,6,9} ,{5,7,8,6,9} ,{8,7,5,6,9} ,{7,8,5,6,9} ,{7,5,8,6,9},
  {9,8,7,6,5} ,{8,9,7,6,5} ,{8,7,9,6,5} ,{8,7,6,9,5} ,{9,7,8,6,5} ,{7,9,8,6,5} ,{7,8,9,6,5} ,{7,8,6,9,5},
  {9,6,7,8,5} ,{6,9,7,8,5} ,{6,7,9,8,5} ,{6,7,8,9,5} ,{9,7,6,8,5} ,{7,9,6,8,5} ,{7,6,9,8,5} ,{7,6,8,9,5},
  {9,6,8,7,5} ,{6,9,8,7,5} ,{6,8,9,7,5} ,{6,8,7,9,5} ,{9,8,6,7,5} ,{8,9,6,7,5} ,{8,6,9,7,5} ,{8,6,7,9,5},
  {9,5,6,7,8} ,{5,9,6,7,8} ,{5,6,9,7,8} ,{5,6,7,9,8} ,{9,6,5,7,8} ,{6,9,5,7,8} ,{6,5,9,7,8} ,{6,5,7,9,8},
  {9,6,7,5,8} ,{6,9,7,5,8} ,{6,7,9,5,8} ,{6,7,5,9,8} ,{9,5,7,6,8} ,{5,9,7,6,8} ,{5,7,9,6,8} ,{5,7,6,9,8},
  {9,7,5,6,8} ,{7,9,5,6,8} ,{7,5,9,6,8} ,{7,5,6,9,8} ,{9,7,6,5,8} ,{7,9,6,5,8} ,{7,6,9,5,8} ,{7,6,5,9,8},
  {9,5,8,7,6} ,{5,9,8,7,6} ,{5,8,9,7,6} ,{5,8,7,9,6} ,{9,8,5,7,6} ,{8,9,5,7,6} ,{8,5,9,7,6} ,{8,5,7,9,6},
  {9,8,7,5,6} ,{8,9,7,5,6} ,{8,7,9,5,6} ,{8,7,5,9,6} ,{9,5,7,8,6} ,{5,9,7,8,6} ,{5,7,9,8,6} ,{5,7,8,9,6},
  {9,7,5,8,6} ,{7,9,5,8,6} ,{7,5,9,8,6} ,{7,5,8,9,6} ,{9,7,8,5,6} ,{7,9,8,5,6} ,{7,8,9,5,6} ,{7,8,5,9,6},
  {9,5,8,6,7} ,{5,9,8,6,7} ,{5,8,9,6,7} ,{5,8,6,9,7} ,{9,8,5,6,7} ,{8,9,5,6,7} ,{8,5,9,6,7} ,{8,5,6,9,7},
  {9,8,6,5,7} ,{8,9,6,5,7} ,{8,6,9,5,7} ,{8,6,5,9,7} ,{9,5,6,8,7} ,{5,9,6,8,7} ,{5,6,9,8,7} ,{5,6,8,9,7},
  {9,6,5,8,7} ,{6,9,5,8,7} ,{6,5,9,8,7} ,{6,5,8,9,7} ,{9,6,8,5,7} ,{6,9,8,5,7} ,{6,8,9,5,7} ,{6,8,5,9,7}
};

int tryperm(state *s0, word *ps) {
  state *states[] = {0, 0, 0, 0, 0};
  word x = 0;

  for (int i = 0;; i = (i + 1) % 5) {
    state *s = states[i];
    if (s == 0) {
      s = states[i] = copystate(s0);
      runinput(s, ps[i]);
    }
    runinput(s, x);
    if (s->state == NORMAL)
      return x;
    x = s->output;
  }
}

int main() {
  state *s0 = readprog();
  word maxval = 0;

  for (int i = 0; i < NUMPERMS; i++) {
    word *ps = perms[i];
    word val = tryperm(s0, ps);
    if (val > maxval)
      maxval = val;
  }
  printf("%lld\n", maxval);
}
