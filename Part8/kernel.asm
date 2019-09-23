org 0x8000
bits 16 

;precompiler constant
%define entityArraySize 100
;Let's begin by going into graphic mode
call initGraphics

;Now let's register some custom interrupt handlers
call registerInterruptHandlers

init_game:

;init map
call initMap

;Main game loop
gameLoop:

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
  
  ; PLAYER DRAWING CODE
  mov si, [snake_head_sprite]   ;get animation

  mov ax, 160/2 - 8/2 - 1      ;center player image CHANGED
  mov bx, 100/2 - 8/2 - 1     ;center player image CHANGED

  call drawImage
  ; END OF PLAYER DRAWING CODE

  call copyBufferOver ;draw frame to screen
  
  call gameControls ;handle control logic
  
  call synchronize ;synchronize emulator and real application through delaying
  
jmp gameLoop


jmp $

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
	add si, ax		   ;add the offset to the frame
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
  add ax, 7           ;subtract 9 because of hitbox
  cmp ax, dx ; (entityZ+9 > playerZ)
    jle .whileSkip
    
  ;if we reach this point => actual collision
  ;mov cx, [di+2]         ;set new x pos to current x pos => no movement
  ;mov dx, [di+4]         ;set new z pos to current z pos => no movement
  
  mov word [si], 0
  inc word [appleFound]

  ;compare end of game 
  cmp word [appleFound], 3
  ;je init_game
  
  ;ding ding count found
  
  jmp .noMapCollision
  .whileSkip:
  add si, 2           ;set si to the next entry in the entityArray
  cmp si, entityArray+((entityArraySize-1)*2)
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
    cmp byte [pressSpace], 1
    ;if it was not pressed continue
    jne .check_inverted
    ;else toggle
    call toggle_pause


  .check_inverted:
    ;check if movement is inverted
    cmp byte [pressL], 1
    ;if it was not pressed continue
    jne .check_movement
    ;else toggle
    call toggle_inverted


  .check_movement:
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
    inc cx
    mov bp, snake_head_img_right
    jmp .na ;already moved so go check collision

    .nd:
    cmp byte [pressLeft], 1 ;try to move x-1 if 'a' is pressed and set animation accordingly, test other cases otherwise
    jne .na
    cmp byte [inverted], 1
    je .mov_right
    .mov_left:
    dec cx
    mov bp, snake_head_img_left

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
    dec dx
    mov bp, snake_head_img_up
    jmp .ns ;already moved so go check collision

    .nw:
    cmp byte [pressDown], 1 ;try to move z+1 if 's' is pressed and set animation accordingly, test other cases otherwise
    jne .ns
    cmp byte [inverted], 1
    je .mov_up
    .mov_down:
    inc dx
    mov bp, snake_head_img_down

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
    mov al, 20h ;20h
    out 20h, al ;acknowledge the interrupt so further interrupts can be handled again 
  popa ;resume state to not modify something by accident
  iret ;return from an interrupt routine
  
;using interrupts instread of the BIOS is SUUPER fast which is why we need to delay execution for at least a few ms per gametick to not be too fast
synchronize:
  pusha
    mov si, 15 ; si = time in ms
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
  xor ax, ax
  mov cx, (entityArraySize-1)
  repne scasw                 ; iterate through entity array until empty stop is found
  sub di, 2
  test ecx, ecx               ; abort here if at the end of the the entity array
  je .failed
  sub cx, (entityArraySize-1) ; calculate index within the array by using the amount of iterated entires
  neg cx
    shl cx, 3
  add cx, entityArrayMem
  mov [di], cx
  mov di, cx
  mov [di], si
  mov [di+2], bx ; set x position of the entity
  mov [di+4], dx ; set y position of the entity
  xor bx, dx     ; "randomise" initial animation position
  mov [di+6], bx ; set animation state
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
		jge .skip 	 ; CHANGED

		mov bx, word [snake_head_posy]
		sub bx, dx
		mov ax, bx
		call abs_value
		; imul bx, bx

		cmp ax, 90 ; CHANGED
		jge .skip 	 ; CHANGED

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
  cmp byte [paused], 0
  ;game is paused so set to running
  jne .set_not_paused
  ;game is not paused so pause
  .set_paused:
    mov byte [paused], 1
    ret
  .set_not_paused:
    mov byte [paused], 0
  ret

%include "buffer.asm"


;game value
paused db 0
inverted db 0
appleFound dw 0

;entity array

entityArray:
      dw snake
      resw entityArraySize

;snake structure
snake:
  snake_head_sprite dw snake_head_img_left
  snake_head_posx dw 0x32
  snake_head_posy dw 0x32
  snake_body_sprite dw snake_body_img


;entity structure
box:
box_Anim  dw boxImg          ;pointer to animation
box_PosX  dw 0x10            ;position of box (x)
box_PosZ  dw 0x10            ;position of box (z)
box_AnimC dw 0               ;animation counter

;other entity structures:
entityArrayMem:
  resw entityArraySize*4
  
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

boxImg_0         incbin "img/block.bin"
tileImg_0        incbin "img/grass.bin"

ASCIImap          incbin "img/map.bin"


%assign usedMemory ($-$$)
%assign usableMemory (512*16)
%warning [usedMemory/usableMemory] Bytes used
times (512*16)-($-$$) db 0 ;kernel must have size multiple of 512 so let's pad it to the correct size
;times (512*1000)-($-$$) db 0 ;toggle this to use in bochs