# AppleEaters
Segunda Tarea de Sistemas Operativos - II 2019

### Bootloader

Intel Syntax
NASM compiler

Testable with QEmu
	
~~~~
    sudo apt-get install qemu
	
~~~~
To run:
	
~~~~
    make
	
~~~~
To burn into a USB drive:

* Find your drive ID with fdisk -l or check in the disk manager
* Get your path to the image
	
~~~~
    dd if=/pathtoimage of=/dev/sdx
~~~~
