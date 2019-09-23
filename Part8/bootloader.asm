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

mov si, welcomemsg ; loads the address of "msg" into SI register

push si
call print
pop si

call keyboardINTListener

load_game:
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

keyboardINTListener: ;interrupt handler for keyboard events
  pusha 
    xor bx,bx ; bx = 0: signify key down event
    inc bx
    in al,0x60 ;get input to AX, 0x60 = ps/2 first port for keyboard
    btr ax, 7 ;al now contains the key code without key pressed flag, also carry flag set if key up event
    jnc .keyDown
      dec bx ; bx = 1: key up event
    .keyDown:
    cmp al,0x39 ;space
    jne .check
    	call load_game
    .check:
    mov al, 20h ;20h
    out 20h, al ;acknowledge the interrupt so further interrupts can be handled again 
  popa ;resume state to not modify something by accident
  jmp keyboardINTListener

print:
    push bp
    mov bp, sp
    pusha
    mov si, [bp+4]      ; grab the pointer to the data
    mov bh, 0x00        ; page number, 0 again
    mov bl, 0x00        ; foreground color, irrelevant - in text mode
    mov ah, 0x0E        ; print character to TTY
 .char:
     mov al, [si]       ; get the current char from our pointer position
     add si, 1          ; keep incrementing si until we see a null char
     or al, 0
     je .return         ; end if the string is done
     int 0x10           ; print the character if we're not done
     jmp .char          ; keep looping
 .return:
     popa
     mov sp, bp
     pop bp
     ret

errormsg db "Failed to load...",0x0D, 0x0A, 0 
welcomemsg db "Welcome to AppleEaters! Please press space",0x0D, 0x0A, 0

secondcount db 2
times 510-($-$$) db 0
        ;Begin MBR Signature
db 0x55 ;byte 511 = 0x55
db 0xAA ;byte 512 = 0xAA