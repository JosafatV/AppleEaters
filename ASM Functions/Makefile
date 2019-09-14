CC = gcc

eco: eco.s
	@echo "Compiling and running eco..."
	${CC} eco.s -no-pie -o eco
	./eco

rand: rand.s
	@echo "Compiling and running rand..."
	${CC} rand.s -no-pie -o rand
	./rand

clean: 
	@echo "Cleaning..."
	rm *.o
	rm *.bin
