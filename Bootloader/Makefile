CC = nasm
BOOT = boot
SRC_BOOT = $(BOOT).asm
OBJ_BOOT = $(BOOT).o
BIN_BOOT = $(BOOT).bin
IMG_BOOT = $(BOOT).img

KERNEL = kernel
SRC_KERNEL = $(KERNEL).asm
OBJ_KERNEL = $(KERNEL).o
BIN_KERNEL = $(KERNEL).bin
IMG_KERNEL = $(KERNEL).img

IMG = livesession.img

NFLAGS = -f bin
QFLAGS = -drive format=raw,file=$(IMG) ##qemu flags

$(IMG): $(IMG_BOOT) $(IMG_KERNEL)
	dd if=$(IMG_BOOT) of=$(IMG) bs=512 count=1
	dd if=$(IMG_KERNEL) of=$(IMG) bs=512 count=1 seek=1


$(IMG_BOOT):
	@echo "Compiling and linking..."
	nasm $(NFLAGS) -o $(IMG_BOOT) $(SRC_BOOT)

$(IMG_KERNEL):
	@echo "Compiling and linking..."
	nasm $(NFLAGS) -o $(IMG_KERNEL) $(SRC_KERNEL)

clean: 
	@echo "Cleaning..."-
	rm *.img
	rm *.bin

# sudo apt-get install qemu
execute: $(IMG)
	@echo "Executing..."
	qemu-system-x86_64 $(QFLAGS)