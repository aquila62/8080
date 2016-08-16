CC=m80

hx.hex:			hx.asm
		$(CC) hx.asm
		kom.sh hx

clean:
		rm -f hx.hex hx.prn hx.com
