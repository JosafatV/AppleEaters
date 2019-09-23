org 0x7C00
%define SECTOR_AMOUNT 0x10  ;Precompiler defined value for easy changing
jmp short start
nop

                                ; BPB
OEMLabel		db "Example "	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 1		; Reserved sectors for boot record
NumberOfFats		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		; Number of entries in root dir
LogicalSectors		dw 2880		; Number of logical sectors
MediumByte		db 0F0h		    ; Medium descriptor byte
SectorsPerFat		dw 9		; Sectors per FAT
SectorsPerTrack		dw 18		; Sectors per track (36/cylinder)
Sides			dw 2		    ; Number of sides/heads
HiddenSectors		dd 0		; Number of hidden sectors
LargeSectors		dd 0		; Number of LBA sectors
DriveNo			dw 0		    ; Drive No: 0
Signature		db 41		    ; Drive signature: 41 for floppy
VolumeID		dd 00000000h	; Volume ID: any number
VolumeLabel		db "Example    "; Volume Label: any 11 chars
FileSystem		db "FAT12   "	; File system type: don't change!
start: 
; ------------------------------------------------------------------

; call movecursor
call clearscreen
mov si, welcomemsg ; loads the address of "msg" into SI register
call print_string
call display_message

;Initialize Registers
cli
xor ax, ax
mov ds, ax
mov ss, ax
mov es, ax
mov fs, ax
mov gs, ax
mov sp, 0x6ef0 ; setup the stack like qemu does
sti

                      ;Reset disk system
mov ah, 0
int 0x13              ; 0x13 ah=0 dl = drive number
jc errorpart
                      ;Read from harddrive and write to RAM
mov bx, 0x8000        ; bx = address to write the kernel to
mov al, SECTOR_AMOUNT ; al = amount of sectors to read
mov ch, 0             ; cylinder/track = 0
mov dh, 0             ; head           = 0
mov cl, 2             ; sector         = 2
mov ah, 2             ; ah = 2: read from drive
int 0x13   		      ; => ah = status, al = amount read
jc errorpart
jmp 0x8000

; ------------------------------------------------------------------

display_message:
	pusha
		.display_message_start:
			dec BYTE [secondcount]
			cmp BYTE [secondcount], 0
			je .display_message_end

			; Convert first to char
			mov al, [secondcount]
			call dig_to_char
			mov BYTE [startingmsg2], al

			; print that message
			mov si, startingmsg1
			call print_string
			mov si, startingmsg2
			call print_string

			call waits
			jmp .display_message_start
		.display_message_end:
 	popa
	ret

; input in ax
; output in ax
dig_to_char:
	add ax, 48
	ret

;using interrupts instread of the BIOS is SUUPER fast which is why we need to delay execution for at least a few ms per gametick to not be too fast
waits:
	pusha
		mov si, 1000 ; si = time in ms
		mov dx, si
		mov cx, si
		shr cx, 6
		shl dx, 10
		mov ah, 86h
		int 15h ;cx,dx sleep time in microseconds - cx = high word, dx = low word
	popa
	ret

print_string:
	push bp
	mov bp, sp
	pusha
		.print_char:
			lodsb 								; loads the current byte from SI into AL and increments the address in SI
			or al, al 
			jz .print_string_end		; compares AL to zero, if AL == 0, jump to "done"
			mov ah, 0x0E
			int 0x10
			jmp .print_char
		.print_string_end:
	popa
	mov sp, bp
	pop bp
	ret

clearscreen:
	push bp
	mov bp, sp
	pusha
		mov ah, 0x07        ; tells BIOS to scroll down window
		mov al, 0x00        ; clear entire window
		mov bh, 0x07        ; white on black
		mov cx, 0x00        ; specifies top left of screen as (0,0)
		mov dh, 0x18        ; 18h = 24 rows of chars
		mov dl, 0x4f        ; 4fh = 79 cols of chars
		int 0x10            ; calls video interrupt
	popa
	mov sp, bp
	pop bp
	ret

movecursor:
	push bp
	mov bp, sp
	pusha
		mov dx, [bp+4]  			; get the argument from the stack. |bp| = 2, |arg| = 2
		mov ah, 0x02        		; set cursor position
		mov bh, 0x00        		; page 0 - doesn't matter, we're not using double-buffering
		int 0x10
	popa
	mov sp, bp
	pop bp
	ret

errorpart:            ;if stuff went wrong you end here so let's display a message
	mov si, errormsg
	mov bh, 0x00          ;page 0
	mov bl, 0x07          ;text attribute
	mov ah, 0x0E          ;tells BIOS to print char
	.part:
		lodsb
		sub al, 0
		jz end
		int 0x10              ;interrupt
		jmp .part
	end:
		jmp $

errormsg db "Failed to load...",0x0D, 0x0A, 0 
welcomemsg db "Welcome to AppleEaters! ",0x0D, 0x0A, 0
startingmsg1 db "Starting in ", 0 
startingmsg2 db "n seconds...",0x0D, 0x0A, 0 
secondcount db 2
times 510-($-$$) db 0
        ;Begin MBR Signature
db 0x55 ;byte 511 = 0x55
db 0xAA ;byte 512 = 0xAA