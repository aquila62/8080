tower.hex:			tower.asm
		m80 tower.asm
		kom.sh tower

clean:
		rm -f tower.hex tower.prn tower.com
