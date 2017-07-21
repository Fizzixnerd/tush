CC=/usr/bin/gcc
LD=/usr/bin/gcc
CFLAGS=-fno-stack-protector
SOFLAGS=-fPIC -c

io.so: io.c
	$(CC) $(CFLAGS) $(SOFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.so
