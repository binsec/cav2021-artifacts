char table[6];

void noop(int);
void noop2(int);

int main(int argc, char** argv) {
  (void)(argv);

  unsigned int x = ((unsigned)argc) & 3;

  if (table[x] == 'A') {
    noop(x);
    return 0;
  } else {
    noop2(x);
    return 1;
  };
}


