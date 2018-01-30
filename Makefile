SCHEME=scheme --libdirs nanopass-framework-scheme

snippet:
	$(SCHEME) --script snippet.scm

scheme:
	$(SCHEME)

.PHONY: snippet scheme
