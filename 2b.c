#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "intcode.h"

int main() {
  word *a;
  size_t len = readprogs(&a);
  int i, j;

  for (i = 0; i <= 100; i++) {
    for (j = 0; j <= 100; j++) {
      int result = runcode(a, len, i, j);
      if (runcode(a, len, i, j) == 19690720)
        goto here;
    }
  }
here:
  printf("%d%d\n", i, j);
}

