org 0x8000
bits 16 

;precompiler constant
%define entityArraySize 100
%define snake_max_size 20
;Let's begin by going into graphic mode
call initGraphics

;Now let's register some custom interrupt handlers
call registerInterruptHandlers

init_game:

mov word [appleFound], 0

;init map
call initMap


moved db 0;

;Main game loop
gameLoop:
  
  cmp word [gamewon_flag],1
  je gameWon

  cmp word [gameover_flag],1
  je gameOver

  cmp word [paused],1
  je gamePause

  cmp byte [pressEsc], 1
  je escapeGame

  cmp byte [pressP], 1
  je restartGame

  call resetBuffer ;reset screen to draw on empty canvas
  
  ;MODULAR DRAWING CODE
  mov di, entityArray
  add di, 2 ;skip drawing player
  .nextEntity:
  cmp [di], word 0
  je .skip
    pusha
    mov cx, [snake_head_posx] ;player x to draw relative
    mov dx, [snake_head_posy] ;player z to draw relative
    mov di, [di]
    call drawEntity
    popa
  .skip:
  add di, 2
  cmp di, entityArray+((entityArraySize-1)*2) ;confirm that di is still pointing into the entityArray
  jl .nextEntity
  
  call drawMap

  cmp word [moved], 0
  je done_drawing
  
  ; PLAYER DRAWING CODE
  mov si, [snake_head_sprite]   ;get animation

  mov ax, 75;160/2 - 8/2 - 1      ;center player image CHANGED 
  mov bx, 45;100/2 - 8/2 - 1     ;center player image CHANGED

  call drawImage
  ; END OF PLAYER DRAWING CODE

   ; PLAYER DRAWING CODE

  tail:
    mov si, [snake_body_sprite] ;image of tail
    mov cx, 0 ;counter for tail length

    .loop_tail:
      cmp cx, word [snake_length] ;if it ended drawing snake
      je draw_score
      ;save this node position x
      mov bx, cx
      add bx, bx ; 2*cx index

      mov ax, word [snake_positions_x+bx]
      mov word [tmp_old_x_2], ax

      ;save this node position y
      mov ax, word [snake_positions_y+bx]
      mov word [tmp_old_y_2], ax

      ;get position from front node x
      mov ax, word [tmp_old_x]
      mov word [snake_positions_x+bx], ax ;index according to position

      ;get position from front node y
      mov ax, word [tmp_old_y]
      mov word [snake_positions_y+bx], ax ;index according to position

      ;sets tmp_old to old position of this
      mov ax, word [tmp_old_x_2]
      mov word [tmp_old_x], ax

      ;sets tmp_old to old position of this
      mov ax, word [tmp_old_y_2]
      mov word [tmp_old_y], ax

      mov ax, word [snake_positions_x+bx]
      sub ax, word [snake_head_posx]   ;subtract the position of the snake_head_posx from the x position
      add ax, 75    ;relative to screen image drawing code for x position CHANGED

      mov bx, word [snake_positions_y+bx]
      sub bx, word [snake_head_posy]   ;subtract the position of the snake_head_posx from the z position
      add bx, 45  ;relative to screen image drawing code for z position CHANGED

      .draw:

      call drawImage
      inc cx ;++increase tail drawn

    jmp .loop_tail

  draw_score:
    mov si, score ;score label
    mov ax, 125
    mov bx, 2
    call drawImage

    cmp word [appleFound], 10
    jge .set_first_digit_1

    .set_first_digit_0:
    mov si, num_0
    mov ax, 150
    mov bx, 2
    call drawImage
    ;second digit
    mov bx, word [appleFound]
    add bx, bx 
    mov si, [nums+bx]
    mov ax, 154
    mov bx, 2
    call drawImage
    jmp done_drawing

    .set_first_digit_1:
    mov si, num_1
    mov ax, 150
    mov bx, 2
    call drawImage
    ;second digit
    mov bx, word [appleFound]
    sub bx, 10 ;we just want the first digit
    add bx, bx 
    mov si, [nums+bx]
    mov ax, 154
    mov bx, 2
    call drawImage
    jmp done_drawing

  gamePause:
    call resetBufferBlack
    mov si, game_paused ;paused image
    mov ax, 55
    mov bx, 45
    call drawImage
    mov si, game_return ;return image
    mov ax, 48
    mov bx, 52
    call drawImage
    jmp done_drawing

  gameOver:
    call resetBufferBlack
    mov si, game_over ;over image
    mov ax, 57
    mov bx, 45
    call drawImage
    mov si, game_restart ;restart image
    mov ax, 40
    mov bx, 52
    call drawImage
    mov si, game_exit ;exit image
    mov ax, 44
    mov bx, 59
    call drawImage
    jmp done_drawing

  gameWon:
    call resetBufferBlack

    cmp word [flag_animation], 0
    je .not_animation

    .animation:
      mov si, game_congrats ;congrats image
      mov ax, 40
      mov bx, 45
      call drawImage
      mov word [flag_animation], 0
      jmp .won_labels

    .not_animation:
      mov word [flag_animation], 1

    .won_labels:
    mov si, game_restart ;restart image
    mov ax, 40
    mov bx, 52
    call drawImage
    mov si, game_exit ;exit image
    mov ax, 44
    mov bx, 59
    call drawImage
    jmp done_drawing



  done_drawing:

  call copyBufferOver ;draw frame to screen

  mov word [snake_direction_x], 0
  mov word [snake_direction_y], 0
  
call gameControls ;handle control logic
  
call synchronize ;synchronize emulator and real application through delaying
  
jmp gameLoop

jmp $

escapeGame:
    ;call resetBufferBlack
    ;call copyBufferOver
    jmp end

restartGame:
    mov byte [pressP], 0
    mov word [snake_length], 0
    mov word [gamewon_flag],0
    mov word [gameover_flag],0
    mov word [paused],0
    call resetEntities
    jmp init_game
    
    resetEntities:
    
    pusha
        mov cx, (entityArraySize-1)  ; Moves amount to loop
        .loopEntities:
        mov di, cx
        imul di, 2
        add di, entityArray
        mov word [di], 0
        loop .loopEntities
    popa
    ret


;di = entity cx,dx = xpos,zpos
drawEntity:
  push dx
  inc word [di+6]
  mov ax, [di+6] ;get index within animation
  mov si, [di]
  xor dx,dx
  div word [si+2]    ; animation time % time of full animation
  mov ax, dx
  xor dx, dx
  div word [si]      ; (animation time % time of full animation) /  time of one frame
  add ax, ax         ; index*2 because image address is a word
  
  add si, 4          ;skip first two words of structure
  add si, ax       ;add the offset to the frame
  mov si, [si]       ;set the image parameter to the image referenced in the frame
  pop dx
  
  ;mov si, word [di]   ;get animation
  ;mov si, word [si+4] ;get first frame of animation
  
  mov ax, word [di+2] ;get entity x
  sub ax, cx          ;subtract the position of the player from the x position
  ;add ax, 80/2 - 9/2 - 1  ;relative to screen image drawing code for x position
  add ax, 160/2 - 8/2 - 1  ;relative to screen image drawing code for x position CHANGED
  mov bx, word [di+4] ;get entity y
  sub bx, dx          ;subtract the position of the player from the z position
  ;add bx, 50/2 - 12/2 - 1 ;relative to screen image drawing code for z position
  add bx, 100/2 - 12/2 - 1 ;relative to screen image drawing code for z position CHANGED
  call drawImage      ;draw image to buffer
  ret

;di = entity, cx = new_xpos, dx = new_zpos, bp = new animation
;fixed for modular entity system
checkForCollision:
    pusha                       ;save current state
        mov si, entityArray         ;set si to entityArray
        .whileLoop:
            mov bx, word [si]   ;read entityArray entry

            test bx, bx         ;if entry is zero => end of array
            jz .whileSkip

            cmp bx, di          ;if entity is equal to di => next entity to not collide with it self
            jz .whileSkip

            mov ax, word [bx+2] ;ax = entity x
            sub ax, 7           ;subtract 8 because of hitbox
            cmp ax, cx ; (entityX-8 <= playerX)
            jg .whileSkip

            mov ax, word [bx+2] ;ax = entity x
            add ax, 7           ;add 8 because of hitbox
            cmp ax, cx ; (entityX+8 > playerX)
            jle .whileSkip

            mov ax, word [bx+4] ;ax = entity z
            sub ax, 7          ;subtract 10 because of hitbox
            cmp ax, dx ; (entityZ-10 <= playerZ)
            jg .whileSkip

            mov ax, word [bx+4] ;ax = entity z
            add ax, 6           ;subtract 9 because of hitbox
            cmp ax, dx ; (entityZ+9 > playerZ)
            jle .whileSkip

            ;if we reach this point => actual collision
            ;mov cx, [di+2]         ;set new x pos to current x pos => no movement
            ;mov dx, [di+4]         ;set new z pos to current z pos => no movement

            lea bx, [bx+8]
            mov ax, [bx] 
            ;mov ax, [bx+8]
            cmp ah, 'A' ; Compare to apple 
            je .found_apple
            cmp ah, 'O' ; Compare to orange
            je .found_orange
            cmp ah, 'L' ; Compare to lemon
            je .found_lemon
            jmp .found_none ; found nothing, do nothing

            .found_apple:
            inc word [appleFound]
            inc word [snake_length]
            cmp word [appleFound], 15
            jl .found_none
            mov word [gamewon_flag], 1
            jmp .found_none

            .found_orange:
            dec word [snake_length]
            cmp word [snake_length], 0
            jge .found_none
            mov word [gameover_flag], 1
            jmp .found_none

            .found_lemon:
            add word [snake_length], 3
            jmp .found_none
            .found_none:

            mov word [si], 0

            jmp .noMapCollision
            .whileSkip:
            add si, 2           ;set si to the next entry in the entityArray
            cmp si, entityArray+((entityArraySize-1)*2) ; I think it is if it reached the end
            jl .whileLoop
        .whileEnd:

        pusha
            mov si, cx
            mov bx, dx
            call collideMap
        popa
        jnc .noMapCollision
        ;if we reach this point => actual collision
        mov cx, [di+2]         ;set new x pos to current x pos => no movement
        mov dx, [di+4]         ;set new z pos to current z pos => no movement
        .noMapCollision:
        mov byte [canWalk], 1
        mov word [di]   ,bp  ;update the animation in use

        ;UPDATE NEW POSITION,BUT STORE OLD
        ;X
        mov bp, word [di+2]
        mov word [tmp_old_x], bp
        ;Y
        mov bp, word [di+4]
        mov word [tmp_old_y], bp

        mov word [di+2] ,cx  ;update x pos
        mov word [di+4] ,dx  ;update y pos
    popa                 ;reload old register state
    ret

canWalk db 0
gameControls:
  mov byte [canWalk], 0
  mov di, snake ;select the player as the main entity for "checkForCollision"

  .check_pause:
    ;check if game was paused
    cmp byte [pressL], 1
    ;if it was not pressed continue
    jne .check_inverted
    ;else toggle
    call toggle_pause


  .check_inverted:
    ;check if movement is inverted
    cmp byte [pressSpace], 1
    ;if it was not pressed continue
    jne .check_movement
    ;else toggle
    call toggle_inverted


  .check_movement:

  mov al, byte [pressLeft]
  add al, byte [pressRight]
  cmp al, 0

  mov al, byte [pressLeft]
  add al, byte [pressRight]
  cmp al, 0

  mov al, byte [pressLeft]
  add al, byte [pressRight]
  cmp al, 0
  ;Not left nor right

  jz .nokeyad
    mov cx, word [snake_head_posx] ;set cx to player x
    mov dx, word [snake_head_posy] ;set dx to player z
    mov bp, [snake]           ;set bp to current animation

    cmp byte [paused], 1
    ;game paused, do nothing
    je .gamePaused

    cmp byte [pressRight], 1 ;try to move x+1 if 'd' is pressed and set animation accordingly, test other cases otherwise
    jne .nd
    cmp byte [inverted], 1 ;verify if movement is inverted
    je .mov_left
    .mov_right:
    add cx, 8
    mov bp, snake_head_img_right
    mov word [snake_direction_x], 1
    jmp .na ;already moved so go check collision

    .nd:
    cmp byte [pressLeft], 1 ;try to move x-1 if 'a' is pressed and set animation accordingly, test other cases otherwise
    jne .na
    cmp byte [inverted], 1
    je .mov_right
    .mov_left:
    sub cx, 8
    mov bp, snake_head_img_left
    mov word [snake_direction_x], -1

    .na:
    call checkForCollision ;check if player would collide on new position, if not change position to new position
  .nokeyad:
  mov al, byte [pressUp]
  add al, byte [pressDown]
  cmp al, 0

  ;Not up nor down

  jz .nokeyws
    mov cx, word [snake_head_posx] ;set cx to player x
    mov dx, word [snake_head_posy] ;set dx to player z
    mov bp, [snake]           ;set bp to current animation

    cmp byte [paused], 1
    ;game paused, do nothing
    je .gamePaused

    cmp byte [pressUp], 1 ;try to move z-1 if 'w' is pressed and set animation accordingly, test other cases otherwise
    jne .nw
    cmp byte [inverted], 1; verify if movement is inverted
    je .mov_down
    .mov_up:
    sub dx, 8
    mov bp, snake_head_img_up
    mov word [snake_direction_y], 1
    jmp .ns ;already moved so go check collision

    .nw:
    cmp byte [pressDown], 1 ;try to move z+1 if 's' is pressed and set animation accordingly, test other cases otherwise
    jne .ns
    cmp byte [inverted], 1
    je .mov_up
    .mov_down:
    add dx, 8
    mov bp, snake_head_img_down
    mov word [snake_direction_y], -1

    .ns:
    call checkForCollision ;check if player would collide on new position, if not change position to new position
  .nokeyws:
  cmp byte [canWalk], 0
  jnz .noCollision
    ret
  .noCollision:
    ret
  .gamePaused:
    mov byte [canWalk], 0
    ret
    ;cannot move

  
;======================================== NEW STUFF ==========================================
registerInterruptHandlers:
  mov [0x0024], dword keyboardINTListener ;implements keyboardListener
  ret
  
;; NEW KEYBOARD EVENT BASED CODE
pressLeft db 0
pressRight db 0
pressUp db 0
pressDown db 0
pressSpace db 0
pressL db 0
pressEsc db 0
pressP db 0


keyboardINTListener: ;interrupt handler for keyboard events
  pusha 
    xor bx,bx ; bx = 0: signify key down event
    inc bx
    in al,0x60 ;get input to AX, 0x60 = ps/2 first port for keyboard
    btr ax, 7 ;al now contains the key code without key pressed flag, also carry flag set if key up event
    jnc .keyDown
      dec bx ; bx = 1: key up event
    .keyDown:
    cmp al,0x4b ;left
    jne .check1         
      mov byte [cs:pressLeft], bl ;use cs overwrite because we don't know where the data segment might point to
    .check1:
    cmp al,0x4d ;right
    jne .check2
      mov byte [cs:pressRight], bl
    .check2:
    cmp al,0x48 ;up
    jne .check3
      mov byte [cs:pressUp], bl
    .check3:
    cmp al,0x50 ;down
    jne .check4
      mov byte [cs:pressDown], bl
    .check4:
    cmp al,0x39 ;space
    jne .check5
      mov byte [cs:pressSpace], bl
    .check5:
    cmp al,0x26 ; L
    jne .check6
      mov byte [cs:pressL], bl
    .check6:
    cmp al,0x01 ; Esc
    jne .check7
      mov byte [cs:pressEsc], bl
    .check7:
    cmp al,0x19 ; P
    jne .check8
      mov byte [cs:pressP], bl
    .check8:
    mov al, 20h ;20h
    out 20h, al ;acknowledge the interrupt so further interrupts can be handled again 
  popa ;resume state to not modify something by accident
  iret ;return from an interrupt routine
  
;using interrupts instread of the BIOS is SUUPER fast which is why we need to delay execution for at least a few ms per gametick to not be too fast
synchronize:
  pusha
    mov si, 80 ; si = time in ms
    mov dx, si
    mov cx, si
    shr cx, 6
    shl dx, 10
    mov ah, 86h
    int 15h ;cx,dx sleep time in microseconds - cx = high word, dx = low word
  popa
  ret

;cx, dx = xpos, zpos, si = animation
;eax == 0 => success, else failed
addEntity:
    pusha
    mov bx, cx
    mov di, entityArray
    push ax
      xor ax, ax
      mov cx, (entityArraySize-1)
      repne scasw                 ; iterate through entity array until empty stop is found
      sub di, 2
      test ecx, ecx               ; abort here if at the end of the the entity array
      je .failed
      sub cx, (entityArraySize-1) ; calculate index within the array by using the amount of iterated entires
      neg cx
      ;shl cx, 3
      imul cx, 10 ;CHANGED!!! 10 bytes
      add cx, entityArrayMem
      mov [di], cx   ; saves address
      mov di, cx
      mov [di], si
      mov [di+2], bx ; set x position of the entity
      mov [di+4], dx ; set y position of the entity
      xor bx, dx     ; "randomise" initial animation position
      mov [di+6], bx ; set animation state
    pop ax
    ; Code to add a fruit value as ax was saved, ah has id byte from map iterate
    lea bx, [di+8]
    mov al, 0
    mov word [bx], ax;1
    mov ax, [bx]
;    call resetBuffer2
;    call copyBufferOver
;    call waits

    popa
    xor eax, eax   ; return 0 if successfully added
    ret
    .failed:
    popa
    xor eax, eax
    inc eax       ; return 1 if failed to find a place for the entity
    ret

;di = entity cx,dx = xpos,zpos
drawBlock:
  pusha
    mov ax, word [snake_head_posx]
    sub ax, cx
    ;imul ax, ax

    call abs_value

    cmp ax, 90 ; CHANGED
    jge .skip    ; CHANGED

    mov bx, word [snake_head_posy]
    sub bx, dx
    mov ax, bx
    call abs_value
    ; imul bx, bx

    cmp ax, 90 ; CHANGED
    jge .skip    ; CHANGED

    ;add ax, bx
    ;cmp ax, 0 ;calculate distance CHANGED
    ;jge .skip

    mov ax, cx
    mov bx, dx
    sub ax, word [snake_head_posx]   ;subtract the position of the snake_head_posx from the x position
    ;add ax, 80/2 - 9/2 - 1    ;relative to screen image drawing code for x position
    add ax, 160/2 - 9/2 - 1    ;relative to screen image drawing code for x position CHANGED
    sub bx, word [snake_head_posy]   ;subtract the position of the snake_head_posx from the z position
    ;add bx, 50/2 - 12/2 - 1   ;relative to screen image drawing code for z position
    add bx, 100/2 - 12/2 - 1   ;relative to screen image drawing code for z position CHANGED
    call drawImage            ;draw image to buffer
    .skip:
    clc
  popa
  ret

; value in ax, output in ax
abs_value:
  push dx
    mov bx, ax ; copies ax
    shr bx, 15 ; gets sign of ax
    and bx, 1 ; gets only sign of ax
    cmp bx, 1
    jne .abs_value_end
    ; if sign is negative, multiply by -1
    imul ax, -1
  .abs_value_end:
  pop dx
  ret
  
;set the position of the player to x=cx, z=dx
setSpawn:
  mov word [snake_head_posx], cx ; set player x
  mov word [snake_head_posy], dx ; set player z
  add word [snake_head_posy], 3  ; offset player z
  clc
  ret
  
;spawn the elements add set the spawn position of the player
initMap:

  mov si, appleImg
  mov bp, addEntity
  mov ah, 'A'
  call iterateMap  ; iterate the map and add an apple at every 'A' on the map

  mov si, lemonImg
  mov bp, addEntity
  mov ah, 'L'
  call iterateMap  ; iterate the map and add a lemon at every 'L' on the map

  mov si, orangeImg
  mov bp, addEntity
  mov ah, 'O'
  call iterateMap  ; iterate the map and add an orange at every 'O' on the map

  call spawnPlayer ; set spawn for player
  ret
  
;draw the map
drawMap:
  pusha
    mov si, boxImg_0
    mov bp, drawBlock
    mov ah, '0'
    call iterateMap ; iterate the map and add a box at every '0' on the map
    ;this second iteration is pretty unefficient but only optional for some ground texture
    mov si, tileImg_0
    mov bp, drawBlock
    mov ah, ' '
    call iterateMap ; iterate the map and add a tile at every ' ' on the map
  popa
  ret
  
; si = player X, bx = player Y
collideMap:
  mov bp, blockCollison
  mov ah, '0'
  call iterateMap ; iterate the map and check for a collision with a '0'
  ret
  
;set the spawn of the player to the field 'P'
spawnPlayer:
  mov bp, setSpawn
  mov ah, 'P'
  call iterateMap ; iterate the map and set the player position to the last 'P' found on the map
  ret
    
%define tileWidth      8
%define ASCIImapWidth  64
%define ASCIImapHeight 64
;bp = function to call, ah = search for, si = parameter for bp function

iterateMap:
  pusha
    mov di, ASCIImap
    mov cx, 0x0 ; map start x
    mov dx, 0x0 ; map start y
    .next:
    mov al, [di]
    test al, al
    je .stop    ; stop when null terminator found
    cmp al, ah
    jne .skip   ; skip if the character is not the one this iteration is searching for
    push ax     ; save the content of ax
    call bp     ; call the specified function of this iteration
    pop ax
    jc .term    ; the carry flag determines if the specified function has found what it was searching for (and thus exits)
    .skip:
      inc di                           ; point to the next character
      add cx, tileWidth                ; increase x pixel position
      cmp cx, ASCIImapWidth*tileWidth  ; check if x position is at the end of the line
      jl .next
    sub dx, tileWidth                    ; decrease y pixel position
    xor cx, cx                           ; reset x position
    jmp .next
    .stop:
      clc
    .term:
  popa
  ret
  
;si = player x, bx = player z, cx = block x, dx = block z
blockCollison:
  push cx
  push dx
  sub cx, 7    ;subtract 8 because of hitbox
  cmp cx, si ; (blockX-8 <= playerX)
    jg .skip
  add cx, 8+7          ;add 8 because of hitbox
  cmp cx, si ; (blockX+8 > playerX)
    jle .skip
  sub dx, 9          ;subtract 10 because of hitbox
  cmp dx, bx ; (blockZ-10 <= playerZ)
    jg .skip
  add dx, 5+10         ;subtract 9 because of hitbox
  cmp dx, bx ; (blockZ+9 > playerZ)
    jle .skip
    stc
    jmp .end
  .skip:
    clc
  .end:
  pop dx
  pop cx
  ret

toggle_inverted:
  cmp byte [inverted], 0
  ;direction is inverted so set to normal
  jne .set_not_inverted
  ;direction is not inverted so invert
  .set_inverted:
    mov byte [inverted], 1
    ret
  .set_not_inverted:
    mov byte [inverted], 0
  ret

toggle_pause:
pusha
  cmp byte [paused], 0
  ;game is paused so set to running
  jne .set_not_paused
  ;game is not paused so pause
  .set_paused:
    mov byte [paused], 1

    ; mov si, game_paused
    ; mov ax, [snake_positions_x]
    ; sub ax, [snake_head_posx]
    ; mov bx, [snake_positions_y]
    ; sub bx, [snake_head_posy]
    ; call drawImage


    popa
    ret
  .set_not_paused:
    mov byte [paused], 0

popa
ret

%include "buffer.asm"


;game value
paused db 0
gameover_flag db 0
gamewon_flag db 0
inverted db 0
appleFound dw 0
flag_animation dw 0


;entity array

entityArray:
      dw snake
      resw entityArraySize

;snake structure
snake:
  snake_head_sprite dw snake_head_img_left
  snake_head_posx dw 0x32
  snake_head_posy dw 0x32
  snake_positions_x times snake_max_size dw 0
  snake_positions_y times snake_max_size dw 0
  snake_direction_x dw 0 ;-1 left , 1 right
  snake_direction_y dw 0 ;-1 down , 1 up
  snake_length dw 0
  snake_body_sprite dw snake_body_img



tmp_old_x dw 0
tmp_old_x_2 dw 0
tmp_old_y dw 0
tmp_old_y_2 dw 0

;entity structure
box:
box_Anim  dw boxImg          ;pointer to animation
box_PosX  dw 0x10            ;position of box (x)
box_PosZ  dw 0x10            ;position of box (z)
box_AnimC dw 0               ;animation counter

;other entity structures:
entityArrayMem:
  resw 5*entityArraySize
  
boxImg:
  dw 1            ;time per frames
  dw 1            ;time of animation
  dw boxImg_0     ;frames
  dw 0            ;zero end frame
  
appleImg:
  dw 5            ;time per frames
  dw 20           ;time of animation
  dw apple_0       ;frames
  dw apple_1       ;frames
  dw apple_2       ;frames
  dw apple_1       ;frames
  dw 0            ;zero end frame

orangeImg:
  dw 5            ;time per frames
  dw 20           ;time of animation
  dw orange_0       ;frames
  dw orange_1       ;frames
  dw orange_2       ;frames
  dw orange_1       ;frames
  dw 0            ;zero end frame

lemonImg:
  dw 5            ;time per frames
  dw 20           ;time of animation
  dw lemon_0       ;frames
  dw lemon_1       ;frames
  dw lemon_2       ;frames
  dw lemon_1       ;frames
  dw 0            ;zero end frame

nums:
  dw num_0
  dw num_1
  dw num_2
  dw num_3
  dw num_4
  dw num_5
  dw num_6
  dw num_7
  dw num_8
  dw num_9

snake_head_img_left  incbin "img/snake_head_left.bin"
snake_head_img_up  incbin "img/snake_head_up.bin"
snake_head_img_down  incbin "img/snake_head_down.bin"
snake_head_img_right  incbin "img/snake_head_right.bin"
snake_body_img       incbin "img/snake_body.bin"

apple_0  incbin "img/apple_0.bin"
apple_1  incbin "img/apple_1.bin"
apple_2  incbin "img/apple_2.bin"

orange_0 incbin "img/orange_0.bin"
orange_1 incbin "img/orange_1.bin"
orange_2 incbin "img/orange_2.bin"

lemon_0 incbin "img/lemon_0.bin"
lemon_1 incbin "img/lemon_1.bin"
lemon_2 incbin "img/lemon_2.bin"

game_over incbin "img/game_over.bin"
game_exit incbin "img/exit.bin"
game_restart incbin "img/restart.bin"
game_return incbin "img/return.bin"
game_paused incbin "img/paused.bin"
game_congrats incbin "img/congratulations_0.bin"

num_0 incbin "img/0.bin"
num_1 incbin "img/1.bin"
num_2 incbin "img/2.bin"
num_3 incbin "img/3.bin"
num_4 incbin "img/4.bin"
num_5 incbin "img/5.bin"
num_6 incbin "img/6.bin"
num_7 incbin "img/7.bin"
num_8 incbin "img/8.bin"
num_9 incbin "img/9.bin"
score incbin "img/score.bin"
;move incbin "img/move.bin"
;exit_key incbin "img/exit_key.bin"
;pause_key incbin "img/pause_key.bin"
;invert_key incbin "img/invert_key.bin"

boxImg_0         incbin "img/block.bin"
tileImg_0        incbin "img/grass.bin"

ASCIImap          incbin "img/map.bin"

end:
  ;CODE TO END PROGRAM     
    mov ax, 0x1000
    mov ax, ss
    mov sp, 0xf000
    mov ax, 0x5307
    mov bx, 0x0001
    mov cx, 0x0003
    int 15h

%assign usedMemory ($-$$)
%assign usableMemory (512*32)
%warning [usedMemory/usableMemory] Bytes used
times (512*32)-($-$$) db 0 ;kernel must have size multiple of 512 so let's pad it to the correct size
;times (512*1000)-($-$$) db 0 ;toggle this to use in bochs