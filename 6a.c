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

int total_depth(Orbit *data, Orbit *p, int depth) {
  int r = depth;
  for (p = p->satellites; p; p = p->next) r += total_depth(data, p, depth + 1);
  return r;
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

  printf("%d\n", total_depth(data, data + encode("COM"), 0));

  return 0;
}
