#include <stdbool.h>
#include <string.h>
#include "intcode.h"

#define SIZE 50

enum { L = -2, R = -1 };

void outp(state *s) {
  char *x = (char*)&(s->userstateword), *y = x + 1;
  char *rx = (char*)&(s->useroutputstate), *ry = rx + 1, *rd = ry + 1;
  char *gridpos = ((char *)s->userstate) + *y * SIZE + *x;
  switch (s->output) {
    case '\n': (*y)++; *x = 0; break;
    case '.': (*x)++; break;
    case '<': (*rd)++; // fall through
    case 'v': (*rd)++; // fall through
    case '>': (*rd)++; // fall through
    case '^': *rx = *x; *ry = *y; // fall through
    default: *gridpos = 1; (*x)++; break;
  }
}

void inp2(state *s) {
  char *in = (char *)s->userstate;
  s->input = *in++;
  s->userstate = in;
  s->userlen--;
}

void outp2(state *s) {
  if (s->output >= 256)
    printf("%lld\n", s->output);
}

void sprintfullpath(char *buf, char *p, char len) {
  bool isfirst = true;
  for (; len; len--, p++) {
    switch (*p) {
      case L: sprintf(buf, "%sL", isfirst? "" : ","); break;
      case R: sprintf(buf, "%sR", isfirst? "" : ","); break;
      default: sprintf(buf, "%s%d", isfirst? "" : ",", *p); break;
    }
    isfirst = false;
    buf += strlen(buf);
  }
  *buf++ = '\n'; *buf = 0;
}

void printfullpath(char *p, char len) {
  char buf[SIZE*SIZE];
  sprintfullpath(buf, p, len);
  puts(buf);
}

char *tryabc(char *dirs, int dirlen, char *a, int alen, char *b, int blen, char *c, int clen, char *result) {
  char *dp = dirs, *p = result;
  while (dp - dirs < dirlen) {
    if (strncmp(dp, a, alen) == 0) { *p++ = 'A'; dp += alen; }
    else if (strncmp(dp, b, blen) == 0) { *p++ = 'B'; dp += blen; }
    else if (strncmp(dp, c, clen) == 0) { *p++ = 'C'; dp += clen; }
    else { return 0; }
  }
  *p = 0;
  return result;
}

int main() {
  char grid[SIZE][SIZE] = {0};
  state *s0 = readprog();
  s0->mode = ASYNC;
  state *s = copystate(s0);
  s->userstate = grid;
  s->userlen = sizeof(grid);
  s->onoutput = outp;
  runcode(s);
  char *rx = (char*)&(s->useroutputstate), *ry = rx + 1, *rd = ry + 1;

  // first get the full path
  char directions[SIZE*SIZE], *dirp = directions;
  while (true) {
    char dir;
    switch (*rd) {
      case 0: dir = grid[*ry][*rx + 1]? R : L; break; // N
      case 1: dir = grid[*ry + 1][*rx]? R : L; break; // E
      case 2: dir = grid[*ry][*rx + 1]? L : R; break; // S
      case 3: dir = grid[*ry + 1][*rx]? L : R; break; // W
    }
    *dirp++ = dir;
    *rd = *rd + (dir == L? -1 : 1);
    *rd &= 3;
    char x = *rx, y = *ry, i = 0;
    while (true) {
      switch (*rd) {
        case 0: y--; break; // N
        case 1: x++; break; // E
        case 2: y++; break; // S
        case 3: x--; break; // W
      }
      if (x < 0 || y < 0 || grid[y][x] == 0) break;
      *rx = x; *ry = y;
      i++;
    }
    if (i == 0) { dirp--; break; }
    *dirp++ = i;
  }
//  printfullpath(directions, dirp-directions);

  // now calculate the best option for A, B, C

  char *a = directions, alen, *b, blen, *c, clen, main[20];
  for (alen = 2; a + alen < dirp && alen <= 12; alen += 2) {
    char *dp = directions;
    while (true) {
      if (strncmp(dp, a, alen) == 0) { dp += alen; }
      else { break; }
    }
    b = dp;
    for (blen = 2; b + blen < dirp && blen <= 12; blen += 2) {
      dp = directions;
      while (true) {
        if (strncmp(dp, a, alen) == 0) { dp += alen; }
        else if (strncmp(dp, b, blen) == 0) { dp += blen; }
        else { break; }
      }
      c = dp;
      for (clen = 2; c + clen < dirp && clen <= 12; clen += 2) {
        if (tryabc(directions, dirp - directions, a, alen, b, blen, c, clen, main))
          goto out;
      }
    }
  }
  printf(":-(\n");
  return 0;
out: ;

  // now run the result through another intcode instance to get the final output

  char buf[21*4+2], *bufp = buf;
  for (char *p = main; *p; p++) {
    *bufp++ = *p;
    *bufp++ = ',';
  }
  *(bufp-1) = '\n';
  sprintfullpath(bufp, a, alen);
  bufp += strlen(bufp);
  sprintfullpath(bufp, b, blen);
  bufp += strlen(bufp);
  sprintfullpath(bufp, c, clen);
  bufp += strlen(bufp);
  *bufp++ = 'y';
  *bufp++ = '\n';
  *bufp = 0;
//  puts(buf);

  freestate(s);
  s = s0;
  s->prog[0] = 2;
  s->onoutput = outp2;
  s->oninput = inp2;
  s->userstate = buf;
  s->userlen = sizeof(buf);
  runcode(s);
  return 0;
}
