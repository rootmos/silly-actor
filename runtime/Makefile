CC_OPTS=-Wpedantic -Werror

libruntime.a: runtime.o stack.o common.o value.o queue.o
	ar cr $@ $^

test: stack_test

stack_test: stack.test.o common.o tests.o
	gcc $(CC_OPTS) -nostartfiles -o $@ -Wl,-e$@_suite $^
	./$@

%.o: %.c
	gcc $(CC_OPTS) -Iinclude -c $<

%.test.o: %.c
	gcc $(CC_OPTS) -DTEST -Iinclude -o $@ -c $<

clean:
	rm -f *.o *_test

.PHONY: test clean
