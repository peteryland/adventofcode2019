#include <unistd.h>
#include <string.h>
#include "intcode.h"

typedef struct wordlist_t {
  word x;
  struct wordlist_t *next;
} wordlist;

void push(wordlist **list, word val) {
  wordlist *item = calloc(1, sizeof(wordlist));
  item->x = val;
  item->next = *list;
  *list = item;
}

word pop(wordlist **list) {
  if (*list == 0) return -1;
  wordlist *this = *list, *prev = 0;
  while (this->next) {
    prev = this;
    this = this->next;
  }
  word ret = this->x;
  free(this);
  if (prev)
    prev->next = 0;
  else
    *list = 0;
  return ret;
}

int main() {
  state *s0 = readprog();
  state *ss[50] = {0};
  s0->userstate = 0;

  for (int i = 0; i < 50; i++) {
    state *s = ss[i] = copystate(s0);
    runinput(s, i);
  }

  while (1) {
    for (int i = 0; i < 50; i++) {
      state *s = ss[i];
      if (!s) continue;

      word dest, x, y;
      switch (s->state) {
        case HAVEOUTPUT:
          dest = s->output; runcode(s);
          x = s->output; runcode(s);
          y = s->output;
          if (dest == 255) {
            printf("%lld\n", y);
            return 0;
          }
          push((wordlist **)&(ss[dest]->userstate), x);
          push((wordlist **)&(ss[dest]->userstate), y);
          runcode(s);
          break;
        case NEEDINPUT:
          if (s->userstate) {
            x = pop((wordlist **)&s->userstate);
            runinput(s, x);
            x = pop((wordlist **)&s->userstate);
            runinput(s, x);
          } else {
            runinput(s, -1);
          }
          break;
        default: ss[i] = 0; break;
      }
    }
  }
}
