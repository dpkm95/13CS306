About:
This is a semantic analyser for C program, with limited capabilities.
It handles the following cases:
	Function Semantics:
		Declaration:
			Overloading: unique function names.
		Definition:
			Overloading: unique function names.
			Type Check: return variable and formal param type against signature.
		Call:
			Definition: function must be defined before call.
			Type Check: actual params against signature.
			
	Variable Semantics:
		Declaration:
			Redeclaration: redeclaration not allowed within the same scope.
		Assignment:
			Declaration: variable must be declared before assignment.
			Type Check: variable type against assigned value type.
			
	Parenthesis Matching: Error Message raised on detection of mismatch.
	
Dependencies:
	ply

Assumptions:
	Function Prototypes:
		Must contain return type int or float.
	Variable Declarations:
		Must be of type int or float.