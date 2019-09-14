
section .data
   userMsg db 'Rolling a D8: '
   lenUserMsg equ $-userMsg

section .bss              ;Uninitialized Data
   d8 resb 5

   section .text           ; Code Segment
global _start              ; Must be declared for using gcc

_start:         ; generate a rand num using the system time
   mov eax, 4 ; load sys_write
   mov ebx, 1
   mov ecx, userMsg
   mov edx, lenUserMsg
   int 80h
   jmp _rolld8

_rolld8:
   mov eax, 96  ; interrupts to get system time        
   int 80h      ; CX:DX now hold number of clock ticks since midnight      

   mov ax, dx
   xor dx, dx
   mov cx, 10    
   div cx       ; here dx contains the remainder of the division - from 0 to 9
   
   mov eax, 4  ; load sys_write
   mov ebx, 1
   mov ecx, resb
   mov edx, 5
   int 80h

   ; Exit code
_stop:
   mov eax, 1
   mov ebx, 0
   int 80h