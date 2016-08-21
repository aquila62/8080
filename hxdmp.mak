CC=m80

hxdmp.hex:		hxdmp.asm
		$(CC) hxdmp.asm
		kom.sh hxdmp

clean:
		rm -f hxdmp.hex hxdmp.prn hxdmp.com
