CC = nasm
NAME = snake
SRC = $(NAME).asm
OBJ = $(NAME).o
BIN = $(NAME).bin
IMG = $(NAME).img
NFLAGS = -f bin
QFLAGS = -drive format=raw,file=$(IMG)

all: $(BIN)
$(BIN):
	@echo "Compiling and linking..."
	nasm $(NFLAGS) -o $(IMG) $(SRC)


clean: 
	@echo "Cleaning..."-
	rm *.img

# sudo apt-get install qemu
execute: $(IMG)
	@echo "Executing..."
	qemu-system-x86_64 $(QFLAGS)