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

int have(int keys, char key) {
  return (keys & (1 << char2num(key))) != 0;
}

void drawgrid(char mygrid[SIZE][SIZE], int mykeys, int existkeys, int x, int y, int curmin) {
  printf("[2J[0;0H");
  for (int j = 0; j < SIZE; j++) {
    if (mygrid[j][0] == 0)
      break;
    for (int i = 0; i < SIZE; i++) {
      char c;
      if (i == x && j == y) c = '@';
      else c = mygrid[j][i];
      if (c == 0) break;
      if (isalpha(c) && have(mykeys, c)) c = '.';
      putchar(c);
    }
    putchar('\n');
  }
  putchar('\n');
  for (char i = 'a'; i <= 'z'; i++) {
    if (have(existkeys, i))
      putchar(i - 0x20 * have(mykeys, i));
  }
  putchar('\n');
  printf("curmin = %d\n", curmin);
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

/*
int solveforkey(char mygrid[SIZE][SIZE], int mykeys, int existkeys, V2i me, LV2i *visited, char goal, int currminsteps) {
  int mykeys1 = mykeys;
  int minsteps = INT_MAX/4;
  LV2i visit = { { me.x, me.y }, 0 };

  if (currminsteps == 0)
    return minsteps;

  if (visited == 0) {
    visited = &visit;
  } else {
    LV2i *curr = visited;
    while (curr->next)
      curr = curr->next;
    curr->next = &visit;
  }

//  drawgrid(mygrid, mykeys, existkeys, me.x, me.y, currminsteps);

  for (int dir = 0; dir < 4; dir++) {
    V2i me1;
    me1.x = me.x; me1.y = me.y;
    switch (dir) {
      case 0: me1.y--; break;
      case 1: me1.x++; break;
      case 2: me1.y++; break;
      case 3: me1.x--; break;
    }
    if (me1.x < 0 || me1.x >= SIZE || me1.y < 0 || me1.y >= SIZE)
      continue;
    char c = mygrid[me1.y][me1.x];
    if (c == '#' || (isupper(c) && !have(mykeys1, c)))
      continue;

    LV2i *curr = visited;
    bool havevisited = false;
    while (curr) {
      if (curr->v.x == me1.x && curr->v.y == me1.y) { havevisited = true; break; }
      curr = curr->next;
    }
    if (havevisited) continue; // check if we've been here before during the current search

    if (islower(c) && !have(mykeys1, c)) {
      if (c != goal) continue; // don't gather other keys first on this run
      minsteps = 1; // it's our goal, let's return 1
      break;
    }

    int steps = solveforkey(mygrid, mykeys1, existkeys, me1, visited, goal, currminsteps - 1);
    if ((steps + 1) < minsteps) minsteps = steps + 1;
  }
  if (visited) {
    LV2i **curr = &visited;
    while ((*curr)->next) {
      curr = &((*curr)->next);
    }
    *curr = 0;
  }
  return minsteps;
}

int solve(char mygrid[SIZE][SIZE], int mykeys, int existkeys, V2i me, LV2i *visited) {
  int mykeys1 = mykeys;
  int minsteps = INT_MAX/4;

  LV2i visit = { { me.x, me.y }, 0 };

  if (visited == 0) {
    visited = &visit;
  } else {
    LV2i *curr = visited;
    while (curr->next)
      curr = curr->next;
    curr->next = &visit;
  }

//  drawgrid(mygrid, mykeys, existkeys, me.x, me.y, 0);

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
    if (c == '#' || (isupper(c) && !have(mykeys1, c)))
      continue;

    LV2i *curr = visited;
    bool havevisited = false;
    while (curr) {
      if (curr->v.x == me1.x && curr->v.y == me1.y) { havevisited = true; break; }
      curr = curr->next;
    }
    if (havevisited) continue; // check if we've been here before

    if (islower(c) && !have(mykeys1, c)) {
      mykeys1 |= 1 << char2num(c);
      visited1 = 0; // after collecting a key, it doesn't matter where we've already been
    }

    if (!(existkeys & ~mykeys1)) {
      minsteps = 1;
      break;
    }
    int steps = solve(mygrid, mykeys1, existkeys, me1, visited1);
    if ((steps + 1) < minsteps) minsteps = steps + 1;
  }
  if (visited) {
    LV2i **curr = &visited;
    while ((*curr)->next) {
      curr = &((*curr)->next);
    }
    *curr = 0;
  }
  return minsteps;
}

int solveallkeys(char mygrid[SIZE][SIZE], V2i keys[NUMKEYS], int mykeys, int existkeys, V2i me, int level) {
  int minsteps = INT_MAX/4;
  for (char i = 'a'; i <= 'z'; i++) {
    if (have(existkeys, i) && !have(mykeys, i)) {
      int steps = solveforkey(mygrid, mykeys, existkeys, me, 0, i, minsteps);
      if (steps >= minsteps) continue;
      int mykeys1 = mykeys | (1 << char2num(i));
      if (level == 0) printf("%c\n", i);
      if (level == 1) printf("  %c: ", i);
      if (mykeys1 != existkeys)
        steps += solveallkeys(mygrid, keys, mykeys1, existkeys, keys[char2num(i)], level+1);
      if (steps < minsteps) minsteps = steps;
      if (level == 0) printf("%d/ %c: %d\n", steps, i, minsteps);
      if (level == 1) printf("%d %d\n", steps, minsteps);
    }
  }
  return minsteps;
}
*/

inline int mygetsigplus(char *mygrid, int i, int j) {
  return                              (mygrid[(j-1)*SIZE+i] == '#') * 16 +
    (mygrid[j*SIZE+i-1] == '#') * 8 + (mygrid[j*SIZE+i] == '#') * 4 + (mygrid[j*SIZE+i+1] == '#') * 2 +
                                      (mygrid[(j+1)*SIZE+i] == '#');
}

void remove_deadends(char *mygrid) {
  for (int j = 1; j < SIZE - 1; j++) {
    for (int i = 1; i < SIZE - 1; i++) {
      if (!islower(mygrid[j*SIZE+i])) {
        int i1 = i, j1 = j;
        switch (mygetsigplus(mygrid, i, j)) {
          case 0x0b: j1-=2; i1++; // up-facing
          case 0x13: i1-=2; // left-facing
          case 0x1a: case 0x19: case 0x1b: // down-facing, right-facing, completely enclosed
            mygrid[j*SIZE+i] = '#';
            i = i1 < 0? 0 : i1;
            j = j1 < 0? 0 : j1;
        }
      }
    }
  }
}

/*
typedef struct intlist_t {
  int distance;
  int z;
  struct intlist_t *next;
} intlist;

typedef struct cell_t {
  char c;
  intlist *neighbours;
} cell;

void intlistadd(intlist **node, int distance, int z) {
  intlist *x = calloc(1, sizeof(intlist));
  x->distance = distance;
  x->z = z;
  while (*node) {
    node = &(*node)->next;
  }
  *node = x;
}

cell *mkgraph(char *mygrid) {
  cell *mygraph = calloc(sizeof(cell), SIZE*SIZE);

  for (int j = 1; j < SIZE - 1; j++) {
    for (int i = 1; i < SIZE - 1; i++) {
      int z = j * SIZE + i;
//      switch (mygetsigplus(mygrid, i, j)) {
//        case 00011: case 01001: case 01010: case 10001: case 10010: case 11000: // only two ways
//          //???
//      }
      mygraph[z].c = mygrid[z];
      if (mygrid[z-SIZE] != '#')
        intlistadd(&mygraph[z].neighbours, 1, z-SIZE);
      if (mygrid[z-1] != '#')
        intlistadd(&mygraph[z].neighbours, 1, z-1);
      if (mygrid[z+1] != '#')
        intlistadd(&mygraph[z].neighbours, 1, z+1);
      if (mygrid[z+SIZE] != '#')
        intlistadd(&mygraph[z].neighbours, 1, z+SIZE);
    }
  }

  return mygraph;
}

void list_v2i_add(LV2i **node, V2i v) {
  LV2i *x = calloc(1, sizeof(intlist));
  x->v = v;
  while (*node) node = &(*node)->next;
  *node = x;
}
*/

typedef struct {
  int x, y, dist, doors;
} V4i;

int findshortestpath(char *mygrid, V2i v, V2i w, int *dist, int *doors) {
  V4i trynext[SIZE*SIZE] = {0}, *trynextptr = trynext;
  V2i track[SIZE*SIZE] = {0}, *trackptr = track;

  *trynextptr++ = (V4i){ v.x, v.y, 0, 0 };

  while (1) {
    if (trynextptr == trynext) return 0; // nothing left on stack
    V4i v1 = *(--trynextptr); // pop

    int visited = 0;
    for (V2i *p = track; p < trackptr; p++) {
      if (p->x == v1.x && p->y == v1.y)
        visited = 1;
    }
    if (visited) continue;
    *trackptr++ = (V2i){ v1.x, v1.y };

    if (v1.x == w.x && v1.y == w.y) { *dist = v1.dist; *doors = v1.doors; return 1; }
    int dist1 = v1.dist + 1, doors1 = v1.doors;
    char c = mygrid[v1.y * SIZE + v1.x];
    if (isupper(c)) doors1 |= 1 << char2num(c);

    V2i v2 = {v1.x, v1.y - 1};
    if (mygrid[v2.y * SIZE + v2.x] != '#') { *trynextptr++ = (V4i){ v2.x, v2.y, dist1, doors1 }; }
    v2 = (V2i){v1.x, v1.y + 1};
    if (mygrid[v2.y * SIZE + v2.x] != '#') { *trynextptr++ = (V4i){ v2.x, v2.y, dist1, doors1 }; }
    v2 = (V2i){v1.x - 1, v1.y};
    if (mygrid[v2.y * SIZE + v2.x] != '#') { *trynextptr++ = (V4i){ v2.x, v2.y, dist1, doors1 }; }
    v2 = (V2i){v1.x + 1, v1.y};
    if (mygrid[v2.y * SIZE + v2.x] != '#') { *trynextptr++ = (V4i){ v2.x, v2.y, dist1, doors1 }; }
  }
  return 0;
}

typedef struct {
  int dist;  // distance betwwen keys
  int doors; // doors in between
} edge;

edge *mkmap(char *mygrid, int existkeys, V2i *keys, V2i me) {
  edge *map = calloc(sizeof(edge), NUMKEYS*(NUMKEYS+1));

  for (char j = 'a'; j <= 'z'; j++) {
    if (!have(existkeys, j)) continue;
    int dist, doors;
    if (findshortestpath(mygrid, me, keys[char2num(j)], &dist, &doors)) {
      map[char2num(j)].doors = doors;
      map[char2num(j)].dist  = dist;
    }
  }

  for (char i = 'a'; i <= 'z'; i++) {
    if (!have(existkeys, i)) continue;
    for (char j = i + 1; j <= 'z'; j++) {
      if (!have(existkeys, j)) continue;
      int dist, doors;
      if (findshortestpath(mygrid, keys[char2num(i)], keys[char2num(j)], &dist, &doors)) {
        map[NUMKEYS + char2num(i)*NUMKEYS + char2num(j)].doors = map[char2num(j)*NUMKEYS + char2num(i)].doors = doors;
        map[NUMKEYS + char2num(i)*NUMKEYS + char2num(j)].dist  = map[char2num(j)*NUMKEYS + char2num(i)].dist  = dist;
      }
    }
  }
  return map;
}

int solveforkey(edge *mymap, int mykeys, int existkeys, char location, char goal, int *steps) {
  edge *myedge;
  myedge = location == '@'? &mymap[char2num(goal)] : &mymap[NUMKEYS + char2num(location)*NUMKEYS + char2num(goal)];
  if (myedge->doors & ~mykeys) return 0;
  *steps = myedge->dist;
  return 1;
}

int solveallkeys(edge *mymap, V2i *keys, int mykeys, int existkeys, char location, int *minsteps) {
  int foundall = 0;
  *minsteps = INT_MAX/4;
  for (char i = 'a'; i <= 'z'; i++) {
    if (have(existkeys, i) && !have(mykeys, i)) {
      int steps;
      if (solveforkey(mymap, mykeys, existkeys, location, i, &steps)) {
        if (steps >= *minsteps) continue;
        int mykeys1 = mykeys | (1 << char2num(i));
        int steps1 = 0;
        if (mykeys1 == existkeys || solveallkeys(mymap, keys, mykeys1, existkeys, i, &steps1)) {
          steps += steps1;
          if (steps < *minsteps) *minsteps = steps;
          foundall = 1;
        }
      }
    }
  }
  return foundall;
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
  remove_deadends((char *)mygrid);
  find(mygrid, '@', &me, keys, doors, &existkeys);
//  cell *mygraph = mkgraph((char *)mygrid);
  edge *mymap = mkmap((char *)mygrid, existkeys, keys, me);
  //drawgraph(mygraph);
  drawgrid(mygrid, mykeys, existkeys, me.x, me.y, 0);
//  printf("%d %d\n", me.x, me.y);
  int steps = 0;
  if (solveallkeys(mymap, keys, mykeys, existkeys, '@', &steps)) {
    printf("%d\n", steps);
  }
    printf("%d\n", steps);
  return 0;
}
