#include "p.h"

inline int getsig(grid *grid, bool (*tobool)(void *val), int x, int y) {
  int ret = 0;
  if (x + 4 > grid->width || y + 6 > grid->height)
    return 0;
  for (int j = y; j < y + 6; j++) {
    for (int i = x; i < x + 4; i++) {
      ret *= 2;
      ret += tobool(grid->get(grid, i, j));
    }
  }
  return ret;
}

char *findletters(char *buf, grid *grid, bool (*tobool)(void *val)) {
  char *p = buf;
  for (int j = 0; j < grid->height; j++) {
    for (int i = 0; i < grid->width; i++) {
      int v = getsig(grid, tobool, i, j);
      char c = 0;
      switch (v) {
        case  6922137: c = 'A'; break;
        case  6916246: c = 'C'; break;
        case 16312456: c = 'F'; break;
        case 10144425: c = 'K'; break;
        case 15310472: c = 'P'; break;
        case 15310505: c = 'R'; break;
        case 10066326: c = 'U'; break;
        case  8933922: c = 'Y'; break;
        case 15803535: c = 'Z'; break;
      }
      if (c) { *p++ = c; i += 3; }
    }
  }
  if (p == buf) return 0;
  return buf;
}
