%ifndef CODE_BUFFER
    %define CODE_BUFFER
	
;set's graphic mode	
initGraphics:
	mov ah, 0   ;set display mode
	mov al, 13h ;13h = 320x200
	int  0x10
	ret

;using interrupts instread of the BIOS is SUUPER fast which is why we need to delay execution for at least a few ms per gametick to not be too fast
waits:
	pusha
		mov si, 2000 ; si = time in ms
		mov dx, si
		mov cx, si
		shr cx, 6
		shl dx, 10
		mov ah, 86h
		int 15h ;cx,dx sleep time in microseconds - cx = high word, dx = low word
	popa
	ret

;resets screen to full black
resetBuffer:
	pusha
	mov cx, 160*120/2 ; divide by 2 because it cant fit in 16 bits
	;xor ax, ax ;this will make the background black
	mov ax, 0x0303 ;this paints the background green
	mov di, [screenPos]
	rep stosw
	rep stosw
	popa
	ret

;resets screen to full black
resetBuffer2:
	pusha
	mov cx, 160*120/2 ; divide by 2 because it cant fit in 16 bits
	;xor ax, ax ;this will make the background black
	;mov ax, 0x0303 ;this paints the background green
	mov di, [screenPos]
	rep stosb
	popa
	ret

;resets screen to full black
resetBufferBlack:
	pusha
	mov cx, 160*120/2 ; divide by 2 because it cant fit in 16 bits
	xor ax, ax ;this will make the background black
	mov di, [screenPos]
	rep stosw
	popa
	ret

; Original
; screen has size 320x200 but buffer only 80x60
; copyBufferOver:
; 	pusha
; 	push es
;     mov es, word [graphicMemory]
;     xor di, di
; 	mov cx, 200
; 	.loop: ; loop for all y
; 		mov dx, cx
; 		mov cx, 320/4
; 		.innerloop: ; loop for all x
; 			mov si, 320
; 			sub si, cx ;invert x-axis
; 			mov bx, 200
; 			sub bx, dx ;invert y-axis
; 			shr bx, 2
; 			imul bx, 80
; 			add si, bx
; 			add si, [screenPos]
; 			lodsb ;read from buffer (ds:si)
; 			mov ah, al
; 			stosb ;write 4 pixel row to graphic memory (es:di) di increments automatically
; 			;stosw
; 		loop .innerloop
; 		mov cx, dx
; 	loop .loop
; 	pop es
; 	popa
; 	ret

; gamePause:
; 	; pusha
; 	; mov cx, 160*120/2 ; divide by 2 because it cant fit in 16 bits
; 	; ;xor ax, ax ;this will make the background black
; 	; mov ax, 0x0000 ;this paints the background green
; 	; mov di, [screenPos]
; 	; rep stosw

; 	; mov si, game_over
; 	; mov ax, 50
; 	; mov bx, 50
; 	; call drawImage
; 	; popa
; 	call resetBuffer
; 	pusha
; 	mov si, game_over
; 	mov ax, 50
; 	mov bx, 50
; 	popa
; 	call drawImage

; 	jmp waitForUnPause

; 	pause_received:

; 	ret


; waitForUnPause: ;interrupt handler for keyboard events
;   pusha 
;     xor bx,bx ; bx = 0: signify key down event
;     inc bx
;     in al,0x60 ;get input to AX, 0x60 = ps/2 first port for keyboard
;     btr ax, 7 ;al now contains the key code without key pressed flag, also carry flag set if key up event
;     jnc .keyDown
;       dec bx ; bx = 1: key up event
;     .keyDown:
;     cmp al,0x39 ;space
;     jne .check
;     jmp pause_received
;     .check:
;     mov al, 20h ;20h
;     out 20h, al ;acknowledge the interrupt so further interrupts can be handled again 
;   popa ;resume state to not modify something by accident
;   jmp waitForUnPause

 ;screen has size 320x200 but buffer only 80x60
 copyBufferOver:
 	pusha
 	push es
	mov es, word [graphicMemory]
	xor di, di
 	mov cx, 200
 	.loop: ; loop for all y
 		mov dx, cx
 		mov cx, 320/2 ; loop 160 pixels
 		.innerloop: ; loop for all x
 			mov si, 320
 			sub si, cx ;invert x-axis
 			mov bx, 200
 			sub bx, dx ;invert y-axis
 			shr bx, 1 	; divides by 2
 			imul bx, 160
 			add si, bx
 			add si, [screenPos]
 			lodsb ;read from buffer (ds:si)
 			mov ah, al
 			stosw ;write 4 pixel row to graphic memory (es:di) di increments automatically
 			;stosw
 		loop .innerloop
 		mov cx, dx
 	loop .loop
 	pop es
 	popa
 	ret

; Original
; ; si = position of image, ax = xpos, bx = ypos
; ;a bit messy because of all the error checks to not draw out of screen
; drawImage:
; 	pusha
; 	xor di, di
; 	imul di, bx, 80     ;add offset y-position
; 	add di, [screenPos] ;make it a pixel in buffer
; 	;add di, ax          ;add offset x-position
; 	mov bp, ax          ;backup x-position offset
; 	xor ax, ax
; 	lodsb
; 	mov cx, ax ;x-size
; 	lodsb
; 	mov dx, ax ;y-size
; 	.for_y: 
; 			mov bx, di
; 			add bx, cx ;bx = offsetOnScreen + xsize
; 			sub bx, word [screenPos]  ;skip if line is out of top border screen
; 			jl .skip_x
; 			sub bx, cx
; 			sub bx, 80*60
; 			jge .skip_x   ;skip if line is out of bottom border screen
; 			xor bx, bx
; 		.for_x:
; 			mov al, byte [si+bx]
; 			add bx, bp
; 			test al, al  ;skip 0bytes as transparent
; 			jz .skip
; 			cmp bx, 80   ;if pixel is right out of screen, skip it
; 			jge .skip
; 			cmp bx, 0    ;if pixel is left out of screen, skip it
; 			jl .skip			
; 			mov byte [di+bx], al ;write byte to buffer
; 			.skip:
; 			sub bx, bp
; 			inc bx
; 			cmp bx, cx
; 		jl .for_x
; 		.skip_x:
; 		add di, 80 ;next row within buffer
; 		add si, cx ;next row within image
; 		dec dx
; 	jnz .for_y ;repeat for y-length
; 	popa
; 	ret

;si = position of image, ax = xpos, bx = ypos
;a bit messy because of all the error checks to not draw out of screen
drawImage:
	pusha
	xor di, di
	imul di, bx, 160     ;add offset y-position
	add di, [screenPos] ;make it a pixel in buffer
	;add di, ax          ;add offset x-position
	mov bp, ax          ;backup x-position offset
	xor ax, ax
	lodsb
	mov cx, ax ;x-size
	lodsb
	mov dx, ax ;y-size
	.for_y: 
			mov bx, di
			add bx, cx ;bx = offsetOnScreen + xsize
			sub bx, word [screenPos]  ;skip if line is out of top border screen
			jl .skip_x
			sub bx, cx
			sub bx, 160*120
			jge .skip_x   ;skip if line is out of bottom border screen
			xor bx, bx
		.for_x:
			mov al, byte [si+bx]
			add bx, bp
			test al, al  ;skip 0bytes as transparent
			jz .skip
			cmp bx, 160   ;if pixel is right out of screen, skip it
			jge .skip
			cmp bx, 0    ;if pixel is left out of screen, skip it
			jl .skip			
			mov byte [di+bx], al ;write byte to buffer
			.skip:
			sub bx, bp
			inc bx
			cmp bx, cx
		jl .for_x
		.skip_x:
		add di, 160 ;next row within buffer
		add si, cx ;next row within image
		dec dx
	jnz .for_y ;repeat for y-length
	popa
	ret

graphicMemory dw 0xA000
screenPos dw 0x0500 ;double buffer will be at this address




;game_congrats_1 incbin "img/congratulations_1.bin"



%endif