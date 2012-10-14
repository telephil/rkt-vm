# Build VM command line tools

vmas_SOURCES = vmas.rkt
assembler_SOURCES = private/asm-parser.rkt  \
	private/assembler.rkt  \
	private/memory.rkt  \
	private/opcodes.rkt  \
	private/registers.rkt  \
	private/syntax.rkt

vmr_SOURCES = vmr.rkt
runner_SOURCES = private/vm.rkt \
	private/memory.rkt \
	private/opcodes.rkt \
	private/registers.rkt

vmdb_SOURCES = vmdb.rkt

all:	vmas vmr vmdb

vmas:	$(vmas_SOURCES) $(assembler_SOURCES)
	raco exe $(vmas_SOURCES)

vmr:	$(vmr_SOURCES) $(runner_SOURCES)
	raco exe $(vmr_SOURCES)

vmdb:	$(vmdb_SOURCES) $(runner_SOURCES) private/disassembler.rkt
	raco exe $(vmdb_SOURCES)

clean:
	rm -f vmas
