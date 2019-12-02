#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "intcode.h"

int main() {
  word *a;
  size_t len = readprogs(&a);
  int result = runcode(a, len, 12, 2);
  printf("%d\n", result);
}
