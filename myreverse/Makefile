all:myreverse

myreverse: main.o myreverse.o
		gcc -w -o myreverse myreverse.o main.o

main.o: main.c
		gcc -w -c -std=c99 main.c

myreverse.o: myreverse.c
		gcc -w -c -std=c99 myreverse.c

