all: ppurs

ppurs:
	dune build ppurs.exe
	mv ppurs.exe ppurs

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

clean:
	rm ppurs
	rm _build -d -r

test: ppurs
	./ppurs --parse-only test.purs

.PHONY: all clean explain ppurs