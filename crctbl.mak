OBJ=crctbl.o

CC=gcc

CFLAGS=-c -Wall -O2

LDFLAGS=

crctbl:				$(OBJ)
		$(CC) -Wall -O2 $(OBJ) -o crctbl $(LDFLAGS)

crctbl.o:			crctbl.c
		$(CC) $(CFLAGS) crctbl.c

clean:
		rm -f $(OBJ) crctbl
