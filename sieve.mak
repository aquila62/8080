sieve.hex:			sieve.asm
		m80 sieve.asm
		kom.sh sieve

clean:
		rm -f sieve.hex sieve.prn sieve.com
