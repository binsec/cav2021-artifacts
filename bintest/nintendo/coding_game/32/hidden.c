#define size 32
unsigned int a[size/16] = {0x12345678, 0xdeadbeef};
unsigned int expected[size/16] = {0xde112da8, 0xc42fde8};
