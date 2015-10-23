##About:
This is a _semantic analyser for C program_, with _limited capabilities_.
It handles the following cases:
  ####Function Semantics:
    * Declaration:
      - Overloading: unique function names.
    * Definition:
      - Overloading: unique function names.
    * Call:
      - Function must be declared before call.
      - Type Check: actual params against signature.
			
  ####Variable Semantics:
    * Declaration:
      - Redeclaration: redeclaration not allowed within the same scope.
    * Assignment:
      - Declaration: variable must be declared before assignment.
      - Type Check: variable type against assigned value type.
			
  ####Parenthesis Matching: Error Message raised on detection of mismatch.
	
##Dependencies:
  * ply

##Assumptions:
  * Function Prototypes:
    - Must contain return type int or float.
    - Formal parameter must have data type and name.
  * Variable Declarations:
    - In global space only one declaration per line is allowed.
    - Must be of type int or float.