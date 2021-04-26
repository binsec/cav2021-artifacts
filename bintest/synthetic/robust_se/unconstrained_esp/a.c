void bug();

int constant;

int victim(int n) {
  if (constant==42) bug();
  return n;
}

int main(int argc, char **argv) {
  return victim(argc);
}


