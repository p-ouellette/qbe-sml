SML_SRC = datatypes.sig \
	  datatypes.sml \
	  gen.sig \
	  gen.sml \
	  print.sig \
	  print.sml \
	  interface.sml \
	  qbe.grm.sig \
	  qbe.grm.sml \
	  qbe.lex.sml \
	  parse.sig \
	  parse.sml
GEN_SRC = qbe.lex.sml qbe.grm.sig qbe.grm.sml

examples/hello: examples/hello.mlb examples/hello.sml $(SML_SRC)
	mlton examples/hello.mlb

%.lex.sml: %.lex
	mllex $<

%.grm.sig %.grm.sml: %.grm
	mlyacc $<

clean:
	rm -rf $(GEN_SRC) qbe.grm.desc examples/hello

.PHONY: clean
