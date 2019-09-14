CC = nasm
all: make_output 

make_output: boot.asm
	@echo "Compiling..."
	${CC} -o boot.bin boot.asm

clean: 
	@echo "Cleaning..."
	rm *.bin

# sudo apt-get install qemu
execute: boot.bin
	@echo "Executing..."
	qemu-system-x86_64 boot.bin 
