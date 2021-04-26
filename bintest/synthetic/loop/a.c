volatile int a = 4;

int main() {
  int i;
  int buffer[20];
  buffer[0] = a;
  for (i=1; i<5; i++) {
    asm volatile ("" ::: "memory");
    buffer[i] = buffer[i-1] + 1;
  }
  return buffer[4];
}
