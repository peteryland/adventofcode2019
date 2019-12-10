#include "intcode.h"

int main() {
  word *a, input = 2;
  size_t len = readprogs(&a);
  runcode(a, len, &input);
}
