# qbe-sml

This is a QBE IL library for Standard ML.
It can parse and print QBE IL, the intermediate language of the
[QBE compiler backend][qbe].

See the [examples](examples/) directory for a usage example.

To build and run the example with MLton:

    make
    cd examples && ./hello

To run it with SML/NJ:

    cd examples && sml -m hello.cm

[qbe]: https://c9x.me/compile/
