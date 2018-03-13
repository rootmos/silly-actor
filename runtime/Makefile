test: stack_test

stack_test: stack.o common.o tests.o
	gcc -nostartfiles -o $@ -Wl,-e$@_suite $^
	./$@

%.o: %.c
	gcc -DTEST -Iinclude -c $<

clean:
	rm -f *.o *_test

.PHONY: test clean
