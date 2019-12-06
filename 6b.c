#include <stdio.h>
#include <string.h>

typedef struct Orbit_t {
  struct Orbit_t *satellites; // children
  struct Orbit_t *next; // linked list of siblings
} Orbit;

int encode(char *p) {
  char buf[4];
  strncpy(buf, p, 4);
  return *(int*)buf;
}

char *decode(char *buf, int i) {
  *(int*)buf = i;
  return buf;
}

void show_orbits(Orbit *data, int i, int depth) {
  char buf[8];
  Orbit *current = data + i, *p;

  printf("%*s %x %s\n", depth, "", i, decode(buf, i));
  for (p = current->satellites; p; p = p->next)
    show_orbits(data, p - data, depth + 1);
}

long find_lowest_common_ancestor(Orbit *data, Orbit *p, Orbit *a, Orbit *b) {
  int founda = 0, foundb = 0;
  if (p == a) return 1;
  if (p == b) return 1l << 32;

  for (Orbit *q = p->satellites; q; q = q->next) {
    long i = find_lowest_common_ancestor(data, q, a, b);
    int deptha = i, depthb = i >> 32;
    if (deptha && depthb) return i; // found lca already, trickle up
    if (deptha) founda = deptha;
    if (depthb) foundb = depthb;
    if (founda && foundb) return ((long)foundb << 32) + founda; // we are lca
  }
  if (founda) return founda + 1l;
  if (foundb) return (foundb + 1l) << 32;
  return 0;
}

Orbit data[0x1000000] = {0};

int main() {
  char line[1048576];
  char *object, *satellite;
  int r = 0;

  while (fgets(line, sizeof line, stdin) != 0) {
    satellite = line;
    satellite[strlen(satellite)-1] = '\0';
    object = strsep(&satellite, ")");

    int objecti = encode(object), satellitei = encode(satellite);
    Orbit **p = &(data[objecti].satellites);
    while (*p) p = &((*p)->next);
    *p = data + satellitei;
  }

  long i = find_lowest_common_ancestor(data, data + encode("COM"), data + encode("YOU"), data + encode("SAN"));
  int dist = i + (i >> 32) - 2;
  printf("%d\n", dist);

  return 0;
}
