all: build run

clean:
	rm -rf ./*.bin
build:
	nasm -fbin kernel.asm -o kernel.bin
	nasm -fbin bootloader.asm -o bootloader.bin
	cat bootloader.bin kernel.bin > apple_eaters.bin
	#copy /b bootloader.bin + kernel.bin kernelCopy.bin
run:
	qemu-system-i386 apple_eaters.bin
run_debug:
	start qemu-system-i386 -s -S apple_eaters.bin
	gdb -ex "target remote localhost:1234" -ex "set architecture i8086" -ex "set disassembly-flavor  intel" -ex "b *0x7C00" -ex "b *0x8000" -ex "c"