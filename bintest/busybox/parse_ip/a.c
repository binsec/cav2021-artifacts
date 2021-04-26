#include <stdlib.h>
#include <stdint.h>
#include <sys/socket.h>
//#include <libbb.h>
typedef struct {
	uint8_t family;
	uint8_t bytelen;
	int16_t bitlen;
	uint32_t data[4];
} inet_prefix;
int get_addr_1(inet_prefix *dst, char *arg, int family);
char *applet_name = "";


char input[20] = "120.86.52.18";

int main() {
  inet_prefix res;
  if (get_addr_1(&res, input, AF_INET)>=0 && res.data[0]==0x12345678) {
    exit(1);
  }
  return 0;
}
