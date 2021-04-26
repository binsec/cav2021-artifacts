#include <stdlib.h>
#include <string.h>
#include <time.h>

char input[20] = "24/08/+00:29";

int main() {
  struct tm tm;
  memset(&tm, 0, sizeof(tm));
  const char *cp;
  input[16] = '\0';
  cp = strptime(input, "%d/%m/%Y %H:%M", &tm);
  if (cp==NULL
      || cp[0]!=0
      || tm.tm_year!=112 // 1900+112
      || tm.tm_mon!=11
      || tm.tm_min!=41
      || tm.tm_hour!=10
      || tm.tm_mday!=21) {
    exit(1);
  }
  return 0;
}
