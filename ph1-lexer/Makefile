default: pylex

pylex.yy.c: pylex.l
	flex pylex.l

pylex: pylex.yy.c
	gcc -g -o pylex lex.yy.c

run: pylex
	./pylex

clean:
	rm -v -f pylex pylex.yy.c tests/*.out

test: pylex
	 j=0;for i in tests/*.py; do ./pylex < $$i > $$i.out;diff $$i.out $$i.expected || j=`expr $$j + 1`; echo "************ test $$i done"; sleep 0.25; echo "Test Cases Failed =$$j" ; done ;
