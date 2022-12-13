SML_SRC = datatypes.sig \
	  datatypes.sml \
	  module.sig \
	  module.sml \
	  print.sig \
	  print.sml \
	  interface.sml \
	  qbe.grm.sig \
	  qbe.grm.sml \
	  qbe.lex.sml \
	  parse.sig \
	  parse.sml

examples/hello: examples/hello.mlb examples/hello.sml $(SML_SRC)
	mlton examples/hello.mlb

%.lex.sml: %.lex
	mllex $<

%.grm.sig %.grm.sml: %.grm
	mlyacc $<

clean:
	rm -rf qbe.lex.sml qbe.grm.sig qbe.grm.sml qbe.grm.desc examples/hello

.PHONY: clean
