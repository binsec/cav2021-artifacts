    .text
    .globl  main
    .type   main, @function
main:
  cmp $0, %ebx
  je exit
  xor %eax, %eax
loop:
  nop
  jmp loop
exit:
  ret



