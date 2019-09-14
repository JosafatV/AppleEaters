CC = nasm
echo: make_output_echo make_executable_echo
d8: make_output_d8 make_executable_d8

# sudo apt-get install nasm
make_output_echo: echo.asm
	@echo "Compiling..."
	${CC} -f elf64 -o echo.o echo.asm

make_executable_echo: echo.o
	@echo "Creating exe..."
	ld echo.o -o echo

make_output_d8: rollD8.asm
	@echo "Compiling..."
	${CC} -f elf64 -o rollD8.o rollD8.asm

make_executable_d8: rollD8.o
	@echo "Creating exe..."
	ld rollD8.o -o rollD8

clean: 
	@echo "Cleaning..."
	rm *.o
	rm *.bin
