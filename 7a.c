#include "intcode.h"

#define NUMPERMS 120
int perms[120][5] = {
  {0,1,2,3,4} ,{1,0,2,3,4} ,{2,1,0,3,4} ,{1,2,0,3,4} ,{2,0,1,3,4} ,{0,2,1,3,4} ,{3,2,1,0,4} ,{2,3,1,0,4},
  {2,1,3,0,4} ,{3,1,2,0,4} ,{1,3,2,0,4} ,{1,2,3,0,4} ,{3,0,1,2,4} ,{0,3,1,2,4} ,{0,1,3,2,4} ,{3,1,0,2,4},
  {1,3,0,2,4} ,{1,0,3,2,4} ,{3,0,2,1,4} ,{0,3,2,1,4} ,{0,2,3,1,4} ,{3,2,0,1,4} ,{2,3,0,1,4} ,{2,0,3,1,4},
  {4,3,2,1,0} ,{3,4,2,1,0} ,{3,2,4,1,0} ,{3,2,1,4,0} ,{4,2,3,1,0} ,{2,4,3,1,0} ,{2,3,4,1,0} ,{2,3,1,4,0},
  {4,1,2,3,0} ,{1,4,2,3,0} ,{1,2,4,3,0} ,{1,2,3,4,0} ,{4,2,1,3,0} ,{2,4,1,3,0} ,{2,1,4,3,0} ,{2,1,3,4,0},
  {4,1,3,2,0} ,{1,4,3,2,0} ,{1,3,4,2,0} ,{1,3,2,4,0} ,{4,3,1,2,0} ,{3,4,1,2,0} ,{3,1,4,2,0} ,{3,1,2,4,0},
  {4,0,1,2,3} ,{0,4,1,2,3} ,{0,1,4,2,3} ,{0,1,2,4,3} ,{4,1,0,2,3} ,{1,4,0,2,3} ,{1,0,4,2,3} ,{1,0,2,4,3},
  {4,1,2,0,3} ,{1,4,2,0,3} ,{1,2,4,0,3} ,{1,2,0,4,3} ,{4,0,2,1,3} ,{0,4,2,1,3} ,{0,2,4,1,3} ,{0,2,1,4,3},
  {4,2,0,1,3} ,{2,4,0,1,3} ,{2,0,4,1,3} ,{2,0,1,4,3} ,{4,2,1,0,3} ,{2,4,1,0,3} ,{2,1,4,0,3} ,{2,1,0,4,3},
  {4,0,3,2,1} ,{0,4,3,2,1} ,{0,3,4,2,1} ,{0,3,2,4,1} ,{4,3,0,2,1} ,{3,4,0,2,1} ,{3,0,4,2,1} ,{3,0,2,4,1},
  {4,3,2,0,1} ,{3,4,2,0,1} ,{3,2,4,0,1} ,{3,2,0,4,1} ,{4,0,2,3,1} ,{0,4,2,3,1} ,{0,2,4,3,1} ,{0,2,3,4,1},
  {4,2,0,3,1} ,{2,4,0,3,1} ,{2,0,4,3,1} ,{2,0,3,4,1} ,{4,2,3,0,1} ,{2,4,3,0,1} ,{2,3,4,0,1} ,{2,3,0,4,1},
  {4,0,3,1,2} ,{0,4,3,1,2} ,{0,3,4,1,2} ,{0,3,1,4,2} ,{4,3,0,1,2} ,{3,4,0,1,2} ,{3,0,4,1,2} ,{3,0,1,4,2},
  {4,3,1,0,2} ,{3,4,1,0,2} ,{3,1,4,0,2} ,{3,1,0,4,2} ,{4,0,1,3,2} ,{0,4,1,3,2} ,{0,1,4,3,2} ,{0,1,3,4,2},
  {4,1,0,3,2} ,{1,4,0,3,2} ,{1,0,4,3,2} ,{1,0,3,4,2} ,{4,1,3,0,2} ,{1,4,3,0,2} ,{1,3,4,0,2} ,{1,3,0,4,2}
};

int tryperm(word *a, size_t len, int *ps) {
  state *states[] = {0, 0, 0, 0, 0};
  int i = 0;
  word x[] = {0, 0};

  for (int i = 0;; i = (i + 1) % 5) {
    x[0] = ps[i];
    if (states[i] == 0)
      states[i] = runcode_new(a, len, x);
    else
      states[i] = resume(states[i], x);
    if (states[i] == 0)
      return x[1];
    x[1] = states[i]->output;
  }
}

int main() {
  word *a;
  size_t len = readprogs(&a);
  int maxval = 0;

  for (int i = 0; i < NUMPERMS; i++) {
    int *ps = perms[i];
    int val = tryperm(a, len, ps);
    if (val > maxval)
      maxval = val;
  }
  printf("%d\n", maxval);
}