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
	rm -f *.so
	make -C runtime clean

docker:
	docker build -t silly-actor .
	docker run --rm --memory=100m silly-actor

.PHONY: tests scheme clean runtime runtime-tests docker
