Sfl stands for a small functional language. It is somehow based on lambda calculus and allows function definitions/applications and currying. The basic operations available are addition, subtraction, multiplication, division, modulo and comparison operators. A conditional statement is also available. 

The interpreter is written in Haskell and nicely showcases the power of functional programming and Haskell in particular. A small semantic analysis is performed by the parser - it is able to detect problems such as: calling an undefined function, redefinition of a function, multiple occurrences of a formal parameter, too many arguments in a function call in a definition of a function or too few arguments in the calculation part of the program. 

A typical program consists of two parts - definition part and calculation part. The first one is a collection of statements such as let x:=23 (assigning 23 to identifier x) or let f:=^x->2*x (definition of function f which takes one argument and doubles it). In the second part the statements which are called are evaluated - x would yield the output of 23 and f{x} would output 46. 

Two example files are included in the examplePrograms folder - one of them is evaluated, the other does not pass semantic analysis and outputs a list of errors. 

Below the grammar of the language is presented:
```
   Program -> Defines Calculation  
   Defines -> let <identifier> := Lambda Exp Defines | <empty string>    
   Lambda  -> ^ <identifier> IDlist ->  | <empty string>    
   IDlist  -> . ^ <identifier> IDlist  | <empty string>  
   Calculation -> Expr Calculation | <empty string>  
   Expr -> T E’  
   E’ -> + T E’| - T E’| <empty string>  
   T -> F T’  
   T’ -> * F T’ | / F T’ | % F T' | <empty string>  
   F  -> [Expr Rel Expo ? Expr : Expr]  
       | ( Expr )  
       | <integer>  
       | <identifier> Args  
   Args -> { Explist }  
   Args -> .  
   Explist -> Expr Explist’.  
   Explist’ -> Expr Explist’.  
   Explist’ -> .  
   Rel   -> < | <= | = | >= | > | <>  
```

To run a program x.txt written in sml compile sml.hs with ghc like this:  
$ ghc sml.hs  
And run it with the file as input like this:  
$ ./sml < x.txt  
