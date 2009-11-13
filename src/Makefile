.PHONY: clean

outOfDateSources := $(shell find . -name .deps -prune -o \( -name \*.scala -a -exec test {} -nt .deps/{} \; \) -print)

default:
	@if [ -n "${outOfDateSources}" ] ; then \
		echo "compiling ${outOfDateSources}"; \
		fsc ${outOfDateSources} || echo "nothing to compile"; \
		for f in ${outOfDateSources}; do \
			mkdir -p `dirname .deps/$$f`; \
			touch .deps/$$f; \
		done; \
	else \
		echo "nothing to compile"; \
	fi

clean:
	rm -rf .deps org
