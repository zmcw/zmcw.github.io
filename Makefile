
SRC := $(shell find . -name '*.lips')
HTML := $(patsubst %.lips,%.html,$(SRC)) 

.PHONY: all clean
all: $(HTML)

clean:
	rm -f $(HTML)

%.html: %.lips site.lisp
	mkdir -p $(shell dirname $@)
	lips -i site.lisp $< >$@.tmp
	mv $@.tmp $@
