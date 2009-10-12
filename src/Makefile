.PHONY: depend clean

-include .deps/all

compile_with_deps = \
	echo -n "compiling $1 "; \
	echo `fsc -verbose $1 | grep '^\[wrote ' | sed -e "s,\[wrote $$PWD/./,," -e "s/\]$$//"`: $1 > .deps/$1; \
	echo "done"

depend:
	@echo "compiling project and building dependency list"
	@rm -f .deps/sources
	@rm -f .deps/all
	@for file in */*.scala ; do \
		outDir=.deps/`dirname $$file`; \
		mkdir -p $$outDir; \
		$(call compile_with_deps,$$file); \
		echo "-include .deps/$$file" >> .deps/sources; \
	done; \
	echo "include .deps/sources" >> .deps/all

%.class:
	@$(call compile_with_deps,$^)

clean:
	rm -rf .deps org
