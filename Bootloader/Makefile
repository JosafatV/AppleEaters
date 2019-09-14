CC = as
all: make_output make_linker

make_output: boot.s
	@echo "Compiling..."
	${CC} -o boot.o boot.s

make_linker: boot.o
	@echo "Linking..."
	ld -o boot.bin --oformat binary -e _start -Ttext 0x7c00 -o boot.bin boot.o

clean: 
	@echo "Cleaning..."
	rm *.o
	rm *.bin

# sudo apt-get install qemu
execute: boot.bin
	@echo "Executing..."
	qemu-system-x86_64 boot.bin 
