section .data
   userMsg db 'Rolling a D8: '
   lenUserMsg equ $-userMsg
   resd8 db 'Number'

   section .text           ; Code Segment
global _start              ; Must be declared for using gcc

_start:
   mov eax, 4             ; load sys_write
   mov ebx, 1             ; print to console
   mov ecx, userMsg       ; load message
   mov edx, lenUserMsg    ; message size
   int 80h                ; execute interrupt

_rand:
   rdtsc              ; Read Time-stamp Counter
   and eax, 7         ; make a 7 bit number
   add ax, 48         ; make it printable
   mov [resd8], eax   ; store it away

_printRand:
   mov eax, 4     ; load sys_write
   mov ebx, 1     ; print to console
   mov ecx, resd8 ; load message
   mov edx, 2     ; message size
   int 80h        ; execute interrupt

   ; Exit code
_stop:
   mov eax, 1
   mov ebx, 0
   int 80h
