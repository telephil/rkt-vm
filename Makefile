# Build VM command line tools
EXE := vmas vm vmdb
DEP := $(EXE:=.deps)

all: $(EXE)
deps: $(DEP)
clean:
	$(RM) $(EXE) $(DEP)

-include $(DEP)

%: %.rkt
	raco exe -o $@ $<

%.deps: %.rkt
	raco make $<
	(echo -n $(@:.deps=)": "; \
	 tr ' ' '\n' < compiled/$(@:.deps=)_rkt.dep | grep rkt-vm | sed -e "s|#\"$$PWD/||" -e 's|"||' | tr '\n' ' '; \
	 echo "") >> $@
	rm -Rf compiled

.PHONY: all clean deps
