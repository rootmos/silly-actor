CC_OPTS=-Wpedantic -Werror -g

libruntime.a: runtime.o stack.o common.o value.o queue.o
	ar cr $@ $^

tests: stack_test

stack_test: stack.test.o common.o tests.c
	gcc $(CC_OPTS) -Iinclude -o $@ -DTEST_SUITE=$@_suite $^
	./$@

%.o: %.c
	gcc $(CC_OPTS) -Iinclude -c $<

%.test.o: %.c
	gcc -DTEST -Iinclude -o $@ -c $<

clean:
	rm -f *.o *_test *.a

.PHONY: tests clean
