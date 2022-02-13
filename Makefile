examples/hello: examples/hello.mlb examples/hello.sml qbe.lex.sml qbe.grm.sig qbe.grm.sml
	mlton examples/hello.mlb

qbe.lex.sml: qbe.lex
	mllex qbe.lex

qbe.grm.sig qbe.grm.sml: qbe.grm
	mlyacc qbe.grm

clean:
	rm -rf qbe.lex.sml qbe.grm.sig qbe.grm.sml qbe.grm.desc examples/hello
