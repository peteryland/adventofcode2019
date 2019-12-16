#include <stdlib.h>
#include <stdio.h>

int nums[6500000];

int main() {
  int n = 0;
  int c = getchar();
  while (c >= 0) {
    if (c >= 0x30 && c <= 0x39)
      nums[n++] = c - 0x30;
    c = getchar();
  }

  for (int j = 1; j < 10000; j++)
    for (int i = 0; i < n; i++)
      nums[j*650+i] = nums[i];
  n *= 10000;

  int offset = 0;
  for (int i = 0; i < 7; i++)
    offset = offset * 10 + nums[i];

  for (int j = 0; j < 100; j++) {
    int acc = 0;
    for (int i = n - 1; i >= offset; i--) {
      acc = nums[i] = (nums[i] + acc) % 10;
    }
  }

  for (int i = offset; i < offset + 8; i++)
    printf("%d", nums[i]);
  printf("\n");
}
