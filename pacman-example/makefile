ASM=nasm
FLAGS= -f bin
BL=bootloader
SO=pacman
COD=.asm
BIN=.bin
IMG=.img
DD=dd
ZERO=/dev/zero
USB=/dev/sdc

all: compile create_img save_usb

compile:
		$(ASM) $(FLAGS) $(BL)$(COD) -o $(BL)$(BIN)
		$(ASM) $(FLAGS) $(SO)$(COD) -o $(SO)$(BIN)

create_img:
		$(DD) if=$(ZERO) of=$(SO)$(IMG) bs=1024 count=512
		$(DD) if=$(BL)$(BIN) of=$(SO)$(IMG) conv=notrunc
		$(DD) if=$(SO)$(BIN) of=$(SO)$(IMG) bs=512 seek=1 conv=notrunc

save_usb:
		$(DD) if=$(SO)$(IMG) of=$(USB)

clean:
		rm $(BL)$(BIN) $(SO)$(BIN) $(SO)$(IMG)

help:
	@echo "Execute using \"sudo make 'USB=/dev/sdX'\""
	@echo "sudo used for load the pacman game in the USB drive"
	@echo "the 'X' on the USB path may be changed with a correct letter on the system"
	@echo "\nTry typing \"sudo fdisk -l\" on terminal to get the USB drive path"
