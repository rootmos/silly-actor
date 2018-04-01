CC_OPTS=-Wpedantic -Werror -g

libruntime.a: runtime.o stack.o common.o value.o queue.o
	ar cr $@ $^

tests: stack_test

stack_test: stack.test.o common.o tests.o
	gcc $(CC_OPTS) -nostartfiles -o $@ -Wl,-e$@_suite $^
	./$@

%.o: %.c
	gcc $(CC_OPTS) -I../bdwgc-dist/include -Iinclude -c $<

%.test.o: %.c
	gcc -DTEST -Iinclude -o $@ -c $<

clean:
	rm -f *.o *_test *.a

.PHONY: tests clean
