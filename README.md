# Snake-Language
Interpreted programming language written in Haskell, featuring a lexer, LL(1) predictive parser, modules, semantic analysis, ASTs

Snake programming language
Snake is implemented in Haskell using an LL(1) predictive parser,
recursive AST construction and direct AST interpretation. It also
includes an editor to be able to run the code without using the console.
Architecture diagram:
Source code (written by the user)
|
Lexer/Tokeniser (transforms the source code into tokens)
|
Creation of parsing table and LL(1) parsing using a stack with AST creation
during parsing
|
Semantic analysis with type checking and a linker that imports/finds imported
functions from the three libraries of Snake
|
Execution of the code
Features:
- Written entirely in Haskell
- Handwritten LL(1) recursive descent parser
- Abstract Syntax Trees (AST)
- Semantic analysis and basic type checking
- Basic error handling
- Interpreter
- Module import system and libraries written in Snake (StdLib,Math,Arrays)
- Functions and effective recursion
- Arrays, strings, integers and booleans
Editor:
Simple editor implemented in Java, where you can run Snake programs,
manage files and see the outputs/errors that your code produces. You can also
use it to open the libraries and edit them (the same way you would open/edit/
save any Snake code file) and add your own functions.
- Standard library written in Snake
Example programs:
1.Factorial calculation:
Factorial [n] -> if n = 0 then return 1
else t<- n - 1
do Factorial[t]
t<- Factorial
t<- t * n
return t
endif
do Factorial[10]
print Factorial
end
>>3628800
2. Even Parity with a randomly generated error for the signal 01101:
a<- [0] + [1] + [1] + [0] + [1]
Count[signal i sum] -> if i = length signal then return sum else sum<- signal/i +
sum i<- i + 1 do Count[signal i sum] return Count endif
do Count [a 0 0]
count <- Count
import Math
do Math.IsEven[count]
iseven<- Math.IsEven
if iseven = 1 then a<- a + 0 else a<- a + 1 endif
print a
inpt <- random length a
import Arrays
t<- a/inpt
do Math.Not[t]
t<- Math.Not
do Arrays.Change[a inpt t]
print Arrays.Change
a<-Arrays.Change
do Count[a 0 0]
count<- Count
do Math.IsEven[count]
temp<- Math.IsEven
if temp = 1 then
print 'no error detected'
else
print 'error detected in signal:'
print a
endif
end
Planned features:
- Structs
- Optimisations
- Improved error handling
- More libraries
- While loops
How do I run it?
If you’re on macOS using an arm architecture chip, you can run the executable
or run the .app to be able to use the editor.
Otherwise, if you have Haskell installed on your device you can perform:
runhaskell snakev0_1.hs
After opening a terminal in the project folde
