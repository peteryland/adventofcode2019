#include <unistd.h>
#include <string.h>
#include "intcode.h"

void inp(state *s) {
  static char buf[1024] = {0}, *p = buf;
  if (*p == 0) {
    printf("> ");
    scanf("%s", buf);
    p = buf;
    int l = strlen(buf);
    buf[l] = '\n';
    buf[l+1] = 0;
  }
  s->input = *p++;
}

void outp(state *s) {
  if (s->output == '.' && s->useroutputstate == 2) {
    usleep(50000);
    printf("[2J[1;1H");
    s->useroutputstate = 0;
  }

  if (s->output == '\n') {
    s->useroutputstate++;
  } else {
    s->useroutputstate = 0;
  }

  if (s->output < 256)
    putchar(s->output);
  else
    printf("%lld\n", s->output);
}

int main() {
  state *s = readprogf(fopen("17.input", "r"));
  s->mode = ASYNC;
  s->prog[0] = 2;
  s->onoutput = outp;
  s->oninput = inp;
  runcode(s);
}
