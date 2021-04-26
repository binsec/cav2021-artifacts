#define SIZE 42

int printf(const char *format, ...);

char buf[SIZE]; /* = symbolic */
int len; /* = symbolic */

int main() {
  volatile int unprivileged = 1;

  // parse the length of the field
  int *as_int = (int*)buf;
  int field_length = *(as_int++);

  char* field = (char*)as_int;
  
  if (field_length >= len) {
    return 1;
  }
  int field_offset = field_length + sizeof(field_length);
  if (field_offset >= SIZE) {
    return 1;
  }

  field[field_length + 1] = '\0';

  // to prevent reordering by the compiler
  asm volatile ("" ::: "memory");

  if (!unprivileged) {
    printf("do something with field %s", field);
    return 0;
  }
  return 1;
}


