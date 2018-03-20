SCHEME=scheme --compile-imported-libraries --libdirs .:nanopass-framework-scheme:thunderchez

snippet: runtime
	timeout 2s $(SCHEME) --script snippet.scm

scheme:
	$(SCHEME) silly-actor.scm

runtime:
	make -C runtime

clean:
	rm -f *.so
	make -C runtime clean

.PHONY: snippet scheme clean runtime
