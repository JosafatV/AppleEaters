[BITS 16]			; Tell nasm that we are running in real mode
[ORG 0x7C00]    		; Bootloader starts at physical address 0x07c00

init:
	XOR AX, AX		; Reset value of register
	MOV DS, AX  		; DS = 0

    	CLI        		; Turn off interrupts

	MOV SS, AX 		; SS = 0x0000
    	MOV SP, 0x7C00		; SP = 0x7c00

    	STI          		; Turn on interrupts

    	XOR AX, AX   		; Reset value of register
    	INT 0x13
    	JC init        		; If failure, run init again

    	MOV AX, 0x1000		; When we read the sector, we are going to read address 0x1000
    	MOV ES, AX     		; Set ES with 0x1000

to_game:
	XOR BX, BX   		; Reset value of register to ensure that the buffer offset is 0
	MOV AH, 0x2  		; 2 = Read USB drive
	MOV AL, 0x8  		; Read eight sectors
	MOV CH, 0x0  		; Track 1
	MOV CL, 0x2  		; Sector 2, track 1
	MOV DH, 0x0  		; Head 1
	INT 0x13
	JC to_game   		; If failure, run to_game again.

	JMP 0x1000:0000 	; Jump to 0x1000, this is the start of the Pacman game

TIMES 510 - ($ - $$) DB 0	; Fill the rest of the sector with zeros
DW 0xAA55   			; Boot signature at the end
