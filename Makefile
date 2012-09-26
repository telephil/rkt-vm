# Build VM command line tools

vmas_SOURCES = vmas.rkt
assembler_SOURCES = private/asm-parser.rkt  \
	private/assembler.rkt  \
	private/memory.rkt  \
	private/opcodes.rkt  \
	private/registers.rkt  \
	private/syntax.rkt

all:	vmas

vmas:	$(vmas_SOURCES) $(assembler_SOURCES)
	raco exe $(vmas_SOURCES)

clean:
	@rm -f vmas
