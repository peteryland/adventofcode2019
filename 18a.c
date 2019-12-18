#include "p.h"

typedef struct {
  int x, y;
} V2i;

typedef struct LV2i_t {
  V2i v;
  struct LV2i_t *next;
} LV2i;

#define SIZE 85
#define NUMKEYS 26
char mygrid[SIZE][SIZE];

int char2num(char c) {
  return (c & 0x1f) - 1;
}

void drawgrid(char mygrid[SIZE][SIZE], int mykeys, int existkeys, int x, int y) {
  printf("[2J[0;0H");
  for (int j = 0; j < SIZE; j++) {
    if (mygrid[j][0] == 0)
      break;
    for (int i = 0; i < SIZE; i++) {
      char c;
      if (i == x && j == y)
        c = '@';
      else
        c = mygrid[j][i];
      if (c == 0) break;
      putchar(c);
    }
    putchar('\n');
  }
  putchar('\n');
  for (int i = 0; i < 26; i++) {
    if (existkeys & (1 << i))
      putchar(0x61 + i - 0x20 * ((mykeys & (1 << i)) != 0));
  }
  putchar('\n');
  fflush(0);
  usleep(20000);
}

void find(char mygrid[SIZE][SIZE], char needle, V2i *v, V2i keys[NUMKEYS], V2i doors[NUMKEYS], int *existkeys) {
  for (int j = 0; j < SIZE; j++) {
    for (int i = 0; i < SIZE; i++) {
      char c = mygrid[j][i];
      if (c == needle) {
        v->x = i; v->y = j;
        mygrid[j][i] = '.';
      }
      if (isupper(c)) {
        doors[char2num(c)].x = i;
        doors[char2num(c)].y = j;
      } else if (islower(c)) {
        keys[char2num(c)].x = i;
        keys[char2num(c)].y = j;
        (*existkeys) |= 1 << char2num(c);
      }
    }
  }
}

int solve(char mygrid[SIZE][SIZE], int mykeys, int existkeys, V2i me, LV2i *visited) {
  int mykeys1 = mykeys;
  int minsteps = -1;

  LV2i visit = { { me.x, me.y }, 0 };

  if (visited == 0) {
    visited = &visit;
  } else {
    LV2i *curr = visited;
    while (curr->next)
      curr = curr->next;
    curr->next = &visit;
  }

//  drawgrid(mygrid, mykeys, existkeys, me.x, me.y);

  for (int dir = 0; dir < 4; dir++) {
    LV2i *visited1 = visited;
    V2i me1;
    me1.x = me.x; me1.y = me.y;
    switch (dir) {
      case 0: me1.y--; break;
      case 1: me1.x++; break;
      case 2: me1.y++; break;
      case 3: me1.x--; break;
    }
//    printf("%d %d %d %d\n", me.x, me.y, me1.x, me1.y);
    if (me1.x < 0 || me1.x >= SIZE || me1.y < 0 || me1.y >= SIZE)
      continue;
    char c = mygrid[me1.y][me1.x];
//    printf("c = %c\n", c);
    if (c == '#' || (isupper(c) && (mykeys1 & (1 << char2num(c))) == 0))
      continue;

    LV2i *curr = visited;
    bool havevisited = false;
    while (curr) {
      if (curr->v.x == me1.x && curr->v.y == me1.y) { havevisited = true; break; }
      curr = curr->next;
    }
    if (havevisited) continue; // check if we've been here before

    if (islower(c) && (mykeys1 & 1 << char2num(c)) == 0) {
      mykeys1 |= 1 << char2num(c);
      visited1 = 0; // after collecting a key, it doesn't matter where we've already been
    }

    if (!(existkeys & ~mykeys1)) {
      minsteps = 0;
      break;
    }
    int steps = solve(mygrid, mykeys1, existkeys, me1, visited1);
    if (steps > 0 && (minsteps == -1 || steps < minsteps))
      minsteps = steps;
  }
  if (visited) {
    LV2i **curr = &visited;
    while ((*curr)->next) {
      curr = &((*curr)->next);
    }
    *curr = 0;
  }
  return 1 + minsteps;
}

int main() {
  int num_movements = 0;
  V2i me, keys[NUMKEYS] = {0}, doors[NUMKEYS] = {0};
  for (int j = 0; j < SIZE; j++) {
    char *s = fgets(mygrid[j], SIZE, stdin);
    if (s == 0) break;
    if (s[strlen(s)-1] == '\n')
      s[strlen(s)-1] = 0;
    if (*s == 0) j--;
  }
  int mykeys = 0, existkeys = 0;
  find(mygrid, '@', &me, keys, doors, &existkeys);
//  printf("%d %d\n", me.x, me.y);
  int steps = solve(mygrid, mykeys, existkeys, me, 0);
  printf("%d\n", steps);
  return 0;
}
