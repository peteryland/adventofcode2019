#include "intcode.h"

int main() {
  word *a, input = 5;
  size_t len = readprogs(&a);
  printf("%d\n", runcode(a, len, &input));
}
