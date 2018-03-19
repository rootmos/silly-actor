
libruntime.a: runtime.o stack.o common.o
	ar cr $@ $^

test: stack_test

stack_test: stack.test.o common.o tests.o
	gcc -nostartfiles -o $@ -Wl,-e$@_suite $^
	./$@

%.o: %.c
	gcc -Iinclude -c $<

%.test.o: %.c
	gcc -DTEST -Iinclude -o $@ -c $<

clean:
	rm -f *.o *_test

.PHONY: test clean
