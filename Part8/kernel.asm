org 0x8000
bits 16 

;precompiler constant
%define entityArraySize 100
kernel:
    jmp k_main

welcome_msg db "Welcome to whyamInotdeadalreadyOS", 0x0D, 0x0A, 0

print_string:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0E
    int 0x10
    jmp print_string
.done:
    ret

k_main:
    ; push cs          ; save the cs reg
    ; pop  ds          ; use as ds also

    mov si, welcome_msg
    call print_string

    .k_main_loop:

    jmp .k_main_loop

cli
hlt

%assign usedMemory ($-$$)
%assign usableMemory (512*16)
%warning [usedMemory/usableMemory] Bytes used
times (512*16)-($-$$) db 0 ;kernel must have size multiple of 512 so let's pad it to the correct size
;times (512*1000)-($-$$) db 0 ;toggle this to use in bochs