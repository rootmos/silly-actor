SCHEME_BIN ?= scheme
SCHEME=$(SCHEME_BIN) --compile-imported-libraries --libdirs .:nanopass-framework-scheme:thunderchez

tests: runtime runtime-tests
	timeout 20s $(SCHEME) --script tests.scm

scheme:
	$(SCHEME) silly-actor.scm

runtime:
	make -C runtime

runtime-tests:
	make -C runtime tests

clean:
	rm -f *.so examples/*.exe*
	make -C runtime clean

.PHONY: tests scheme clean runtime runtime-tests
