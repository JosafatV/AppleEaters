;Snake!
[BITS 16]

TOTAL_SEGMENTS equ 0x09

section .bss
  x_coord   RESW TOTAL_SEGMENTS ; [x_coord] is the head, [x_coord+2] is the next cell, etc.
  y_coord   RESW TOTAL_SEGMENTS ; Same here
  t1        RESB 2
  t2        RESB 2
  enabled   RESB 2
  x_apple   RESB 2
  y_apple   RESB 2
  
section  .text
  global_start: 

_start:
  call SetVideoMode
  call SetInitialCoords
  call ClearScreen
  call Debug
  call ListenForInput

SetVideoMode:
  mov AH, 0x00
  mov AL, 0x13
  INT 0x10
  ret

ClearScreen:
  mov CX, 0x00
  mov DX, 0x00
  mov AL, 0x00
  mov BH, 0x00
  mov AH, 0x0C
  .x_loop_begin:
   mov CX, 0x00
   .y_loop_begin:
    INT 0x10
    INC CX
    CMP CX, 0x140
    JNAE .y_loop_begin
   .y_loop_end:
   INC DX
   CMP DX, 0xFF
   JNAE .x_loop_begin
  .x_loop_end:
  ret 

SetInitialCoords:
  mov AX, 0x0F ; Initial x/y coord
  mov BX, 0x00
  mov DX, TOTAL_SEGMENTS
  ADD DX, DX

  .initialize_loop_begin:
   mov [x_coord+BX], AX
   mov [y_coord+BX], AX
   ADD BX, 0x02
   CMP BX, DX
   JNE .initialize_loop_begin
  
  mov AX, 0x00
  mov [t1]       , AX
  mov [t2]       , AX
  mov AX, 2
  mov [enabled]  , AX

  call RandomNumber
  mov [x_apple], AX
  call RandomNumber
  mov [y_apple], AX
  ret

ListenForInput:  ;Repeatedly check for keyboard input
  mov AH, 0x00 ; Set AH to 0 to lock when listening for key
  mov AL, 0x00 ; Set last key to 0
  INT 0x16   ; Listen for a keypress, save to register AL
  
  call InterpretKeypress

  call ListenForInput
  ret

InterpretKeypress:
  CMP AL, 0x48 ;UP
  JE .w_pressed

  CMP AL, 0x4B ;LEFT
  JE .a_pressed

  CMP AL, 0x4D ;DOWN
  JE .s_pressed

  CMP AL, 0x50 ;RIGHT
  JE .d_pressed
  call Debug

  ret ; Invalid keypress, start listening again

  .w_pressed:
  mov AX, [x_coord]
  mov BX, [y_coord]
  DEC BX
  JMP .after_control_handle

  .a_pressed:
  mov AX, [x_coord]
  mov BX, [y_coord]
  DEC AX
  JMP .after_control_handle

  .s_pressed:
  mov AX, [x_coord]
  mov BX, [y_coord]
  INC BX
  JMP .after_control_handle
  
  .d_pressed:
  mov AX, [x_coord]
  mov BX, [y_coord]
  INC AX
 
  .after_control_handle:
  mov [t1], AX
  mov [t2], BX
  call CheckAppleCollision
  call ShiftArray
  call DrawSnake
  call DrawApple
  ret

CheckAppleCollision:
  CMP AX, [x_apple]
  JNE .no_collision

  CMP BX, [y_apple]
  JNE .no_collision
  
  mov AX, [enabled]
  INC AX
  mov [enabled], AX
  
  call RandomNumber
  mov [x_apple], AX
  call RandomNumber
  mov [y_apple], AX

  .no_collision:
  ret

DrawApple:
  mov CX, [x_apple]
  mov DX, [y_apple]
  mov AL, 0x0C
  call DrawPixel
  ret

DrawSnake:
  call ClearScreen
  mov BX, 0x00
  mov AL, 0x0A
  mov [t1], BX
  .draw_snake_loop_begin:
   CMP [enabled], BX
   JBE .skip
   mov [t1], BX
   ADD BX, BX
   mov CX, [x_coord+BX]
   mov DX, [y_coord+BX]
   call DrawPixel
   mov BX, [t1]
   INC BX
   JMP .draw_snake_loop_begin
  
  .skip:
  ret

ShiftArray:
  mov BX, TOTAL_SEGMENTS
  DEC BX
  ADD BX, BX
  .loop_begin:
   ADD BX, -2
   mov DX, [x_coord+BX]
   mov CX, [y_coord+BX]
   ADD BX, 2
   mov [x_coord+BX], DX
   mov [y_coord+BX], CX
   ADD BX, -2
   CMP BX, 0x00
   JNE .loop_begin
  mov DX, [t1]
  mov [x_coord], DX
  mov DX, [t2]
  mov [y_coord], DX
  ret

DrawPixel:
  mov AH, 0x0C     ; Draw mode
  mov BH, 0x00     ; Pg 0
  INT 0x10         ; Draw
  ret

RandomNumber:
  RDTSC
  AND EAX, 0xF
  ret

Debug:
  mov AL, 0x0A
  mov CX, 0x00
  mov DX, 0x00
  call DrawPixel
  ret

TIMES 510 - ($ - $$) db 0  ;Fill the rest of sector with 0
DW 0xAA55      ;Add boot signature at the end of bootloader