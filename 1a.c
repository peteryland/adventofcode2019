#include "p.h"

int calcFuel(int x) {
  return x / 3 - 2;
}

int main() {
  listi *l = readints(stdin);
  int total = 0;
  while (1) {
    int x;
    if (listi_poph(l, &x) == 0) break;
    total += calcFuel(x);
  }
  printf("%d\n", total);
  return 0;
}
