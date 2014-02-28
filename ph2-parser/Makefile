all: pyparse
	@echo "compiled"

lex:
	@echo "insert code to run lexer here"

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

parse: pyparse derivative-parsers.rkt
	@pylex | ./pyparse

clean:
	rm -rf compiled pyparse sdiff
	@echo "insert code to clean here"

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: pyparse sdiff

#for i in tests/*.lex; do make parse < $$i > $$i.out; ./sdiff $$i.out $$i.expected; done
#for i in tests_old/*.py; do ./pyparse < $$i.lex > $$i.out; ./sdiff $$i.out $$i.expected; done

# test_new has been modified as test_3
#	for i in test_new/*.txt; do racket ../pyparse_exec/pyparse_rkt.zo < $$i.res > $$i.expected; ./pyparse < $$i.res > $$i.out; ./sdiff $$i.out $$i.expected; done
	for i in test_3/input/*.txt; do ./cheatlex.py < $$i > $$i.lex ;racket ../pyparse_exec/pyparse_rkt.zo < $$i.lex > $$i.parsed; ./pyparse < $$i.lex > $$i.out; ./sdiff $$i.out $$i.parsed; done
