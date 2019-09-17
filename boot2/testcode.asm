ORG 0x7e00

start:
    mov si, testCodeStr
    call print_string

    cli
.end_loop:
    hlt
    jmp .end_loop

testCodeStr: db "Test harness loaded and is executing code in stage2!", 0

; Function: print_string
;           Display a string to the console on display page 0
;
; Inputs:   SI = Offset of address to print
; Clobbers: AX, BX, SI

print_string:
    mov ah, 0x0e                ; BIOS tty Print
    xor bx, bx                  ; Set display page to 0 (BL)
    jmp .getch
.repeat:
    int 0x10                    ; print character
.getch:
    lodsb                       ; Get character from string
    test al,al                  ; Have we reached end of string?
    jnz .repeat                 ;     if not process next character
.end:
    ret