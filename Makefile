SCHEME=scheme --compile-imported-libraries --libdirs .:nanopass-framework-scheme:thunderchez

RUNTIME=runtime/libruntime.a

snippet: $(RUNTIME)
	timeout 2s $(SCHEME) --script snippet.scm

scheme:
	$(SCHEME) silly-actor.scm

$(RUNTIME):
	make -C runtime ${notdir $@}

clean:
	rm -f *.so
	make -C runtime clean

.PHONY: snippet scheme clean
