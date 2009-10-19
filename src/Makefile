.PHONY: clean

sources := $(shell find . -name .deps -prune -o -name \*.scala -print)

default: $(patsubst %,.deps/%,${sources})

.deps/%.scala: %.scala
	@mkdir -p $(dir $@) && \
	echo "compiling $^"; \
	fsc $^ && touch $@

clean:
	rm -rf .deps org
