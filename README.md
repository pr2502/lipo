# LiPo

LiPo is a compiler and a VM for a language based on Lox from the
[Crafting Interpreters] book.

[Crafting Interpreters]: https://craftinginterpreters.com/

The compiler and VM are implemented in Rust instead of C. The language itself
is modified both in syntax and in semantics.

Unlike Clox LiPo is not a single-pass compiler, it has an AST representation
and the parser is inspired more by
<https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
than the book.
