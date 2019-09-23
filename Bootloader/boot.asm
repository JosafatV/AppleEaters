%define SECTOR_AMOUNT 0x10  ;Precompiler defined value for easy changing
STAGE2_ABS_ADDR  equ 0x07e00
STAGE2_RUN_SEG   equ 0x0000
STAGE2_RUN_OFS   equ STAGE2_ABS_ADDR
STAGE2_LOAD_SEG  equ STAGE2_ABS_ADDR>>4
STAGE2_LBA_START equ 1          ; Logical Block Address(LBA) Stage2 starts on
                                ;     LBA 1 = sector after boot sector

bits 16

org 0x7C00

%include "bpb.inc"

init: 

	xor ax, ax                  ; DS=SS=ES=0 for stage2 loading
	mov ds, ax
	mov ss, ax                  ; Stack at 0x0000:0x7c00
	mov sp, 0x7c00
	cld                         ; Set string instructions to use forward movement
	;call movecursor

	call clearscreen

	mov si, welcome_message ; loads the address of "msg" into SI register
	call print_string

	call reset_floppy

	call read_kernel

	hlt

movecursor:
  push bp
  mov bp, sp
  pusha

  mov dx, [bp+4]  				; get the argument from the stack. |bp| = 2, |arg| = 2
  mov ah, 0x02        		; set cursor position
  mov bh, 0x00        		; page 0 - doesn't matter, we're not using double-buffering
  int 0x10

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

print_string:
	push bp
	mov bp, sp
	pusha
print_char:
  lodsb 								; loads the current byte from SI into AL and increments the address in SI
  or al, al 
  jz print_string_end		; compares AL to zero, if AL == 0, jump to "done"
  mov ah, 0x0E
  int 0x10
  jmp print_char
print_string_end:
  popa
  mov sp, bp
  pop bp
  ret

kernel:
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
  int 0x13            ; => ah = status, al = amount read
  jc errorpart
  jmp 0x8000

reset_floppy:
	push bp
  mov bp, sp
  pusha

  mov ah, 0
  mov dl, 0
  int 0x13
  jc reset_floppy
  mov si, floppy_reset_done
  call print_string

  popa
  mov sp, bp
  pop bp
  ret

read_kernel:
  mov si, loading_msg
  call print_string

  mov si, 1
  ;call lba_to_chs             ; Convert current LBA to CHS

  mov ax , STAGE2_LOAD_SEG   ; setting up the address to read into
  mov es, ax
  xor bx, bx       ; clearing bx
  mov ah, 0x02     ; floppy function
  mov al, 1        ; read 1 sector
  mov ch, 0        ; cylinder
  mov cl, 2        ; sector to read
  mov dh, 0        ; head number
  mov dl, 0x80     ; drive number, normally defaults

  int 0x13         ; BIOS Interrupt Call

  jc read_kernel 	 ; Error loading the segment
  
  mov si, loading_success
  call print_string
	
	mov ax, STAGE2_RUN_SEG      ; Set up the segments appropriate for Stage2 to run
  mov ds, ax
  mov es, ax

  ; FAR JMP to the Stage2 entry point at physical address 0x07e00
  xor ax, ax                  ; ES=FS=GS=0 (DS zeroed earlier)
  mov es, ax
  mov fs, ax
  mov gs, ax
  ; SS:SP is already at 0x0000:0x7c00, keep it that way
  ; DL still contains the boot drive number
  ; Far jump to second stage at 0x0000:0x7e00; just a jump to 0x7e00
  jmp STAGE2_RUN_SEG:STAGE2_RUN_OFS
  ;jmp STAGE2_LOAD_SEG:STAGE2_RUN_SEG

welcome_message: db "Bootloader started!", 0x0D, 0x0A, 0 			; we need to explicitely put the zero byte here
floppy_reset_done: db "Floppy has been reset.", 0x0D, 0x0A, 0
loading_msg: db "Reading Kernel Sector", 0x0D, 0x0A, 0
loading_success: db "Kernel Sector Loaded", 0x0D, 0x0A, 0
done: db "Bootloader Done.", 0x0D, 0x0A, 0

bootDevice:      db 0x00

times 510-($-$$) db 0     ; fill the output file with zeroes until 510 bytes are full
dw 0xaa55                 ; magic number that tells the BIOS this is bootable