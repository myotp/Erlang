all:
	gcc -o priv/sniff c_src/sniff.c -lpcap -lpthread
	erlc -Iinclude -o ebin src/*.erl
clean:
	@rm -f priv/sniff
	@rm -f ebin/*.beam
	@rm -f *~
