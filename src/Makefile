.PHONY: depend clean

-include .deps/all
-include .deps/sources

sources := $(shell find . -name .deps -prune -o -name \*.scala -print | sort)

compile_get_deps = \
	`fsc -verbose $1 | grep '^\[wrote ' | sed -e "s,\[wrote $$PWD/./,," -e "s/\]$$//" -e 's/\\$$/$$$$/g'`

store_deps = \
	mkdir -p .deps/`dirname $1`; \
	echo $2: $1 > .deps/$1;

compile_store_deps = \
	echo -n "compiling $1 "; \
	$(call store_deps,$1,$(call compile_get_deps,$1)) \
	echo "done"

depend:
	@echo "compiling project and building dependency list"
	@mkdir -p .deps
	@rm -f .deps/sources
	@echo -n "default: " > .deps/all
	@for file in ${sources} ; do \
		outDir=.deps/`dirname $$file`; \
		mkdir -p $$outDir; \
		echo -n "compiling $$file "; \
		output=$(call compile_get_deps,$$file); \
		$(call store_deps,$$file,$$output) \
		echo "done"; \
		echo -n "`echo $$output` " >> .deps/all; \
		echo "-include .deps/$$file" >> .deps/sources; \
	done; \
	echo ${sources} | sed "s/ /\n/g" > .deps/files

%.class:
	@$(call compile_store_deps,$^)

clean:
	rm -rf .deps org
