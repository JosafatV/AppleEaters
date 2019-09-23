create_rand:
    PUSHA           ; Push everything to stack
    RDTSC           ; Read Time-Stamp Counter
    AND EAX, 0xF    ; Turn EAX into a 4 bit number
    POPA            ; Pop everything from Stack
    RET

score:
    PUSHA                   ; Push everything to stack
    XOR ebx, ebx            ; Clean EBX
    MOVB bl, score_inc      ; Move points earned to EBX
    movl eax, score         ; Get current score
    addl eax, ebx           ; Add to update the score
    movl score, eax         ; Store updated score
    POPA                    ; Pop everything from Stack
    RET


.bss
score           .long   0
score_inc       .byte   0