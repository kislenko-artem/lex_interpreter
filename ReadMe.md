### Simplified implementation interpreter of language: [Lex](https://craftinginterpreters.com)

More full implementation the language on Rust: 

* https://github.com/Lapz/tox

Main purpose:

* studying and understanding how languages work inside.
* I don't recommend use this anywhere except for understanding inside work (because I didn't aim to create good product)

What do features work here:

* types: string, boolean, number
* operations: 
  * addition (`+`)
  * subtraction (`-`)
  * multiplication (`*`)
  * division (`/`)
  * comparison (`==` `>=` `<=` `>`  `<` ) 
* variables assigment
* branches (`if`, `else`)
* cycles (`while`) (not support `break` and `continue`)
* functions (for instance `fun pr(s, t){return s + t;} print pr(1, 2)`) (`return` doesn't abort execution)