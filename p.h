#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#ifndef _p_h
#define _p_h

typedef struct grid_t {
  int width, height;
  void *(*get)(struct grid_t *grid, int x, int y);
  void (*put)(struct grid_t *grid, int x, int y, void *val);
  void *data;
  size_t len;
} grid;

int getsig(grid *grid, bool (*tobool)(void *val), int x, int y);
char *findletters(char *buf, grid *grid, bool (*tobool)(void *val));

#endif
