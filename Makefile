sl: stdlib.c $(file)
	dune exec llvmtut $(file)
	cc -c stdlib.c
	cc test.obj stdlib.o -o main