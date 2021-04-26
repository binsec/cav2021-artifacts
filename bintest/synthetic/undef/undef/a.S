    .text
    .globl  main
    .type   main, @function
main:
  xor %eax, %eax
  shl $3, %eax
  jo ko
ok:
  ret
ko:
  hlt


