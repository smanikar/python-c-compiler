PYLEX=pylex
PYPARSE=pyparse
PYTRANS=pytrans
PYDESUGAR=pydesugar
PYCPS=pycps
PYMVE=./pymve
PYCC=./pycc
PYLL=./pyll
PYTOC=./pytoc

PYTHON=python
RACKET=racket

all: test

desugar: pydesugar
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR)

cps: pydesugar pycps 
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS)

mve: pymve
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS) | $(PYMVE)
	
cc: pymve pycc
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS) | $(PYMVE) | $(PYCC)

ll: pymve pycc pyll
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS) | $(PYMVE) | $(PYCC) | $(PYLL)

toc: pymve pycc pyll pytoc
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS) | $(PYMVE) | $(PYCC) | $(PYLL) $(PYTOC)

pycps: pycps.rkt
	raco exe pycps.rkt

pydesugar: pydesugar.rkt
	raco exe pydesugar.rkt

pymve:
	raco exe pymve.rkt

pycc:
	raco exe pycc.rkt

pyll:
	raco exe pyll.rkt

pytoc:
	raco exe pytoc.rkt

sdiff: sdiff.rkt
	raco exe ./sdiff.rkt

test: pymve sdiff pycc pyll pytoc

clean:
	rm -rf pymve pyll pycc pytoc
