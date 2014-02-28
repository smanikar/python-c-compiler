# Srikanth Manikarnike
# U070564

PYLEX=pylex
PYPARSE=pyparse
PYTRANS=pytrans
PYDESUGAR=./pydesugar
PYCPS=./pycps

PYTHON=python
RACKET=racket

all: pycps pydesugar 


desugar: pydesugar
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR)

cps: pydesugar pycps 
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS)


pycps: pycps.rkt
	raco exe pycps.rkt

pydesugar: pydesugar.rkt
	raco exe pydesugar.rkt

sdiff: sdiff.rkt
	raco exe ./sdiff.rkt

test-ds: pydesugar sdiff
	for i in tests_2/*.py;do ./pydesugar < $$i.hir > $$i.hir.out ; ./sdiff $$i.lir $$i.hir.out; done

test: pycps sdiff
	for i  in tests_1/*.py; do ./pycps < $$i.lir > $$i.lir.out ; echo "My output for $$i.rkt "; cat ./cps-header.rkt $$i.lir.out > $$i.rkt ; racket $$i.rkt > $$i.rkt.out ; echo "Python output for $$i "; cat ./cps-header.rkt $$i.cps > $$i.cps.out ; racket $$i.cps.out > $$i.out ; echo "*********" ; ./sdiff $$i.rkt.out $$i.out ; done

testall:
for i in tests_3/*.py; do ./pylex < $$i > $$i.lex ;racket ./pyparse_rkt.zo   < $$i.lex > $$i.ast; racket ./pytrans_rkt.zo   < $$i.ast > $$i.hir ; racket ./pydesugar_rkt.zo < $$i.hir > $$i.lir ; racket ./pycps_rkt.zo  < $$i.lir > $$i.cps ; done

clean:
	rm -rf stub
	rm -rf compiled
	rm -f pydesugar pycps 
#pycps_rkt.zo pydesugar_rkt.zo
#rm -f tests/*.{lxd,ast,lir,hir,crkt,lrkt,cdiff,ldiff,expected,cresult,lresult} stub.tar.gz
