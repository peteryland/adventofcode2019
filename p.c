#include "p.h"

int binarysearch(tryfunc func, void *arg, int min, int max) {
  int r = 0, rr = 0, y = min;
  for (int f = (max - min) / 2; f; f /= 2) {
    if (r) {
      y -= f;
      rr = r;
    } else {
      y += f;
    }
    r = func(arg, y);
  }
  return rr;
}

int binarysearch0(tryfunc func, void *arg) {
  int r = 0, y = 1;
  while (r == 0) {
    y *= 2;
    r = func(arg, y);
  }
  return binarysearch(func, arg, y / 2, y);
}

listi *listi_new() {
  return calloc(1, sizeof(listi));
}

void listi_pushh(listi *l, int x) {
  l->len++;
  elemi *e = calloc(1, sizeof(elemi));
  e->x = x;
  if (l->h == 0) {
    l->h = l->t = e;
    return;
  }
  elemi *h = l->h;
  e->n = h;
  l->h = e;
  h->p = e;
}

int *listi_poph(listi *l, int *x) {
  elemi *h = l->h;
  if (h == 0)
    return 0;
  l->len--;
  *x = h->x;
  elemi *n = h->n;
  l->h = n;
  if (n)
    n->p = 0;
  else
    l->t = 0;
  free(h);
  return x;
}

void listi_pusht(listi *l, int x) {
  l->len++;
  elemi *e = calloc(1, sizeof(elemi));
  e->x = x;
  if (l->t == 0) {
    l->t = l->h = e;
    return;
  }
  elemi *t = l->t;
  e->p = t;
  l->t = e;
  t->n = e;
}

int *listi_popt(listi *l, int *x) {
  elemi *t = l->t;
  if (t == 0)
    return 0;
  l->len--;
  *x = t->x;
  elemi *p = t->p;
  l->t = p;
  if (p)
    p->n = 0;
  else
    l->h = 0;
  free(t);
  return x;
}

listi *readints(FILE *in) {
  char buf[1024] = {0};
  listi *l = listi_new();
  do {
    char *p = fgets(buf, 1024, in);
    if (p == 0)
      break;
    int wraps = strlen(buf) == 1023;
    while (*p) {
      if (wraps && p > buf + 512) {
        memcpy(buf, buf + 512, 512);
        p -= 512;
        int len = strlen(buf);
        fgets(buf + 511, 513, in);
      }

      errno = 0;
      int x = strtol(p, &p, 10);
      if (errno == 0)
        listi_pusht(l, x);
      else
        if (*p) p++;
      while (!(*p == '-' || *p == '+' || isdigit(*p) || *p == 0))
        p++;
    }
  } while (1);
  return l;
}

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
