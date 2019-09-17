bits 16
org 0x7E00
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
    push cs          ; save the cs reg
    pop  ds          ; use as ds also

    mov si, welcome_msg
    call print_string

    .k_main_loop:

    jmp .k_main_loop

cli
hlt

times 512-($-$$) db 0