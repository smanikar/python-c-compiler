default: pytrans
	@echo "compiled"

lex:
	@echo "insert code to run lexer here"

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

parse: pyparse derivative-parsers.rkt
	@pylex | ./pyparse

pytrans: pytrans.rkt
	raco exe pytrans.rkt

trans: pyptrans derivative-parsers.rkt
	@pylex | ./pyparse | ./pytrans

clean:
	rm -rf compiled pyparse sdiff
	@echo "insert code to clean here"

sdiff: sdiff.rkt
	raco exe ./sdiff.rkt

test: pytrans sdiff 

	for i in tests1/*.txt; do racket ./pytrans_rkt.zo < $$i.parsed > $$i.trans ; ./pytrans < $$i.parsed > $$i.out ; ./sdiff $$i.out $$i.trans; done

testall: pytrans sdiff

	for i in tests1/*.txt; do ./cheatlex.py < $$i > $$i.lex ;racket ./pyparse_exec/pyparse_rkt.zo < $$i.lex > $$i.parsed; racket ./pytrans_rkt.zo < $$i.parsed > $$i.trans ; ./pytrans < $$i.parsed > $$i.out ; ./sdiff $$i.out $$i.trans; done
