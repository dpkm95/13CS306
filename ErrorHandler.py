class ParanMismatchError(Exception):
    pass

class VariableNotDeclaredError(Exception):
    pass

class VariableRedeclarationError(Exception):
    pass

class VariableTypeError(Exception):
    pass

class FunctionRedeclarationError(Exception):
    pass

class FunctionNotDeclaredError(Exception):
	pass

class InvalidOperandError(Exception):
	pass

class ReturnTypeMismatchError(Exception):
	pass

class AssignmentTypeMismatchError(Exception):
	pass

class FunctionOverloadingError(Exception):
	pass