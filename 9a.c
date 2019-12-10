#include "intcode.h"

int main() {
  word *a, input = 1;
  size_t len = readprogs(&a);
  runcode(a, len, &input);
}
