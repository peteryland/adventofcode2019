#include "p.h"

int calcFuel(int x) {
  return x / 3 - 2;
}

int recurCalcFuel(int x) {
  int f = calcFuel(x);
  if (f <= 0)
    return x;
  else
    return x + recurCalcFuel(f);
}

int main() {
  listi *l = readints(stdin);
  int total = 0;
  while (1) {
    int x;
    if (listi_poph(l, &x) == 0) break;
    total += recurCalcFuel(calcFuel(x));
  }
  printf("%d\n", total);
  return 0;
}
