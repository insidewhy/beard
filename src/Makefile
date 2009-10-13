.PHONY: depend clean

-include .deps/sources

sources := $(shell find . -name .deps -prune -o -name \*.scala -print | sort)

compile_store_deps = \
	mkdir -p .deps/`dirname $1`; \
	rm -f .deps/$1; \
	echo -n "compiling $1 "; \
	fsc -verbose $1 | awk \
		'BEGIN { objs = "" } \
		/error:/ { error = 1 } \
		/./ { if(error == 1) print } \
		/^\[wrote / { \
			gsub("\\]","",$$2); \
			gsub("\\$$","$$$$",$$2); \
			objs = (objs substr($$2,length("'$$PWD'") + 4) " ") } \
		END { if (error) exit(1) ; print objs ": '$1'\ndefault: " objs > ".deps/'$1'.d" }' || exit 1; \
	echo "done";

depend:
	@echo "compiling project and building dependency list"
	@mkdir -p .deps
	@echo -e "default:\n" > .deps/sources
	@for file in ${sources} ; do \
		$(call compile_store_deps,$$file) \
		echo "-include .deps/$$file.d" >> .deps/sources; \
	done; \
	echo ${sources} | sed "s/ /\n/g" > .deps/files

%.class:
	@$(call compile_store_deps,$^)

clean:
	rm -rf .deps org
