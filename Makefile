PARSER_SML = qbe.lex.sml qbe.grm.sig qbe.grm.sml

examples/hello: examples/hello.mlb examples/hello.sml $(PARSER_SML)
	mlton examples/hello.mlb

qbe.lex.sml: qbe.lex
	mllex qbe.lex

qbe.grm.sig qbe.grm.sml: qbe.grm
	mlyacc qbe.grm

clean:
	rm -rf $(PARSER_SML) qbe.grm.desc examples/hello
