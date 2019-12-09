#include <stdlib.h>
#include <string.h>
#include "intcode.h"

int main() {
  word *a, input = 2;
  size_t len = readprogs(&a);
  word *a2 = calloc(len * 2, sizeof(word));
  memcpy(a2, a, len * sizeof(word));
  printf("%lld\n", runcode(a2, len, &input));
}
