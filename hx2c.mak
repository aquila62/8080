OBJ=hx2c.o

CC=gcc

CFLAGS=-c -Wall -O2

LDFLAGS=

hx2c:				$(OBJ)
		$(CC) -Wall -O2 $(OBJ) -o hx2c $(LDFLAGS)

hx2c.o:				hx2c.c
		$(CC) $(CFLAGS) hx2c.c

clean:
		rm -f $(OBJ) hx2c
