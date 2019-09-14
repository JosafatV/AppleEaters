[bits 16]    ; use 16 bits
[org 0x7c00] ; sets the start address

section .text
  global _start

_start:
  mov ah, 0x0e ; sets AH to 0xe (function teletype)
  mov si, msg  ; loads the address of "msg" into SI register

_printLoop:
  lodsb     ; loads the current byte from SI into AL and increments the address in SI
  cmp al, 0 ; compares AL to zero
  je _done   ; if AL == 0, jump to "_done"
  int 0x10  ; print to screen using function 0xe of interrupt 0x10
  jmp _printLoop ; repeat with next byte

_done:
  hlt ; stop execution

msg db "Hello world!", 0 ; the zero acts as an EOF

times 510-($-$$) db 0           ; fill the output file with zeroes until 510 bytes are full
dw 0xaa55                       ; magic number that tells the BIOS this is bootable