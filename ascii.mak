CC=m80

ascii.hex:			ascii.asm
		$(CC) ascii.asm
		kom.sh ascii

clean:
		rm -f ascii.hex ascii.prn ascii.com
