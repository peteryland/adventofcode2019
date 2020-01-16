#include <ctype.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>

#ifndef _p_h
#define _p_h

typedef int (*tryfunc)(void *, int);
int binarysearch(tryfunc func, void *arg, int min, int max);
int binarysearch0(tryfunc func, void *arg);

typedef struct grid_t {
  int width, height;
  void *(*get)(struct grid_t *grid, int x, int y);
  void (*put)(struct grid_t *grid, int x, int y, void *val);
  void *data;
  size_t len;
} grid;

typedef struct elemi_t {
  int x;
  struct elemi_t *n;
  struct elemi_t *p;
} elemi;

typedef struct listi_t {
  elemi *h;
  elemi *t;
  size_t len;
} listi;

listi *listi_new();
void listi_pushh(listi *l, int x); // add to head
int *listi_poph(listi *l, int *x); // pop from head
void listi_pusht(listi *l, int x); // add to tail
int *listi_popt(listi *l, int *x); // pop from tail

listi *readints(FILE *in); // read all ints, separated by non-numbers
int getsig(grid *grid, bool (*tobool)(void *val), int x, int y);
char *findletters(char *buf, grid *grid, bool (*tobool)(void *val));

#endif
