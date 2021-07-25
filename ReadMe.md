### Simplified implementation interpreter of language: [Lex](https://craftinginterpreters.coml)

More full implementation the language on Rust: 

* https://github.com/Lapz/tox

Main purpose:

* studying and understanding how languages work inside.

What do features work here:

* types: string, bool, number
* operation: 
  * addition (`+`)
  * subtraction (`-`)
  * multiplication (`*`)
  * division (`/`)
  * comparison (`==` `>=` `<=` `>`  `<` ) 
* variables assigment
* branches (`if`, `else`)
* cycles (`while`) (not support `break` and `continue`)
* functions (for instance `fun pr(s, t){return s + t;} print pr(1, 2)`)