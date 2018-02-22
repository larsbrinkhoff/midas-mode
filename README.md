### Emacs mode for MIDAS assembly language

MIDAS is an assembler made at MIT.  It was originally made for the
TX-0, ported over to the PDP-1, and then the PDP-6.  It was used as
the main assembler, indeed main programming language, in the PDP-10
Incompatible Timesharing System.

Syntactic elements:

| Syntax | Purpose
| --- | ---
| ; | Comment to end of line
| ? | Starts a new statement on the same line
| ^X | ASCII control character X
| 'X | SIXBIT character X
| "X | ASCII character X
| ASCII/.../ | ASCII string
| SIXBIT/.../ | SIXBIT string
| (...) | Syntactical grouping, and halfword swapping
| [...] | Syntactical grouping, and literals
| <...> | Syntactical grouping
| X" | Global symbol
| X"Y | Symbol Y in block X
| X' | Variable X
| # | XOR operator
| \ | OR operator
| _ | Left shift operator

Midas mode is licensed under the GPL v3, see COPYING.md.
