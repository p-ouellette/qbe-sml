# qbe-sml

QBE IL library for Standard ML.
It can generate and parse QBE IL, the intermediate language of the
[QBE compiler backend][qbe].

See the `examples` directory for a usage example.

To build and run the example with MLton:

    make
    cd examples && ./hello

To run it with SML/NJ:

    cd examples && sml -m hello.cm

[qbe]: https://c9x.me/compile/
