#include <stdlib.h>
#include <stdio.h>

inline int calcElem(int x, int *nums, int n) {
  int r = 0;
  int j = 1;
  int op = x == 0;
  int *p = nums;

  for (int i = 0; i < n; i++) {
    if (op & 1) {
      if (op & 2)
        r -= *p;
      else
        r += *p;
    }
    p++;
    j++;
    if (j > x) {
      j = 0;
      op++;
    }
  }
  return abs(r) % 10;
}

int main() {
  int nums[1000];
  int n = 0;
  int c = getchar();
  while (c >= 0) {
    if (c >= 0x30 && c <= 0x39)
      nums[n++] = c - 0x30;
    c = getchar();
  }

  for (int j = 0; j < 100; j++) {
    for (int i = 0; i < n; i++) {
      nums[i] = calcElem(i, nums, n);
    }
  }

  for (int i = 0; i < 8; i++)
    printf("%d", nums[i]);
  printf("\n");
}
