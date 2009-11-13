.PHONY: clean

outOfDateSources := $(shell find . -name .deps -prune -o \( -name \*.scala -a -exec test {} -nt .deps/{} \; \) -print)

classPath := $(addprefix -cp ,${CLASSPATH})

default:
	@if [ -n "${outOfDateSources}" ] ; then \
		echo "compiling ${outOfDateSources}"; \
		if fsc ${classPath} ${outOfDateSources}; then \
			echo "compilation succeeded"; \
			for f in ${outOfDateSources}; do \
				mkdir -p `dirname .deps/$$f`; \
				touch .deps/$$f; \
			done; \
		else \
			echo "failed"; \
		fi \
	else \
		echo "nothing to compile"; \
	fi

clean:
	rm -rf .deps org
