SCHEME=scheme --compile-imported-libraries --libdirs .:nanopass-framework-scheme:thunderchez

snippet:
	timeout 2s $(SCHEME) --script snippet.scm

scheme:
	$(SCHEME) silly-actor.scm

.PHONY: snippet scheme
