void opaque(unsigned int n, char* ptr);

volatile int input = 10;

void victim() {
  int n = input;

  char buffer[8];
  opaque(n, buffer);
}

void main() {
  victim();
}


