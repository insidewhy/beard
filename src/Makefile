.PHONY: depend clean

-include .deps/all

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
	@rm -f .deps/sources
	@rm -f .deps/all
	for file in */*.scala ; do \
		outDir=.deps/`dirname $$file`; \
		mkdir -p $$outDir; \
		echo -n "compiling $$file "; \
		output=$(call compile_get_deps,$$file); \
		$(call store_deps,$$file,$$output) \
		echo "done"; \
		echo "-include .deps/$$file" >> .deps/sources; \
	done; \
	echo "include .deps/sources" >> .deps/all

%.class:
	@$(call compile_store_deps,$^)

clean:
	rm -rf .deps org
