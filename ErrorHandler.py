class ParanMismatchError(Exception):
	def __init__(self,lineno,err_msg):
		self.lineno = lineno
		self.err_msg = err_msg
	def __str__(self):
		return 'ERROR: ParanMismatchError at line '+str(self.lineno)+\
			'\nERR MSG: '+self.err_msg

class VariableNotDeclaredError(Exception):
	def __init__(self,lineno,var):
		self.lineno = lineno
		self.var = var
	def __str__(self):
		if self.var is not None:
			return 'ERROR: VariableNotDeclaredError at line '+str(self.lineno)+\
				'\nERR MSG: variable '+self.var+' not declared'
		else:
			return 'ERROR: VariableNotDeclaredError at line '+str(self.lineno)+\
				'\nERR MSG: variable not declared'

class VariableRedeclarationError(Exception):
	def __init__(self,lineno,var):
		self.lineno = lineno
		self.var = var
	def __str__(self):
		return 'ERROR: VariableRedeclarationError at line '+str(self.lineno)+\
			'\nERR MSG: variable '+self.var+' re-declared'

class VariableTypeError(Exception):
	def __init__(self,lineno,type1,type2=None):
		self.lineno = lineno
		self.type1 = type1
		self.type2 = type2
	def __str__(self):
		if self.type2 is None:
			return 'ERROR: VariableTypeError at line '+str(self.lineno)+\
				'\nERR MSG: mismatch for type '+self.type1
		return 'ERROR: VariableTypeError at line '+str(self.lineno)+\
			'\nERR MSG: mismatch between types '+self.type1+' and '+self.type2

class FunctionRedeclarationError(Exception):
	def __init__(self,lineno,fun):
		self.lineno = lineno
		self.fun = fun
	def __str__(self):
		return 'ERROR: FunctionRedeclarationError at line '+str(self.lineno)+\
			'\nERR MSG: function '+self.fun+' re-declared'

class FunctionNotDeclaredError(Exception):
	def __init__(self,lineno,fun):
		self.lineno = lineno
		self.fun = fun
	def __str__(self):
		return 'ERROR: FunctionNotDeclaredError at line '+str(self.lineno)+\
			'\nERR MSG: function '+self.fun+' not declared'

class InvalidOperandError(Exception):
	def __init__(self,lineno,op=None):
		self.lineno = lineno
		self.op = op
	def __str__(self):
		if self.op is None:
			return 'ERROR: InvalidOperandError at line '+str(self.lineno)+\
			'\nERR MSG: invalid operand'
		return 'ERROR: InvalidOperandError at line '+str(self.lineno)+\
			'\nERR MSG: invalid operand for '+self.op+' operation'

class ReturnTypeMismatchError(Exception):
	def __init__(self,lineno,type1,type2=None):
		self.lineno = lineno
		self.type1 = type1
		self.type2 = type2
	def __str__(self):
		if self.type2 is None:
			return 'ERROR: ReturnTypeMismatchError at line '+str(self.lineno)+\
				'\nERR MSG: mismatch for type '+self.type1
		return 'ERROR: ReturnTypeMismatchError at line '+str(self.lineno)+\
			'\nERR MSG: mismatch between types '+self.type1+' and '+self.type2

class AssignmentTypeMismatchError(Exception):
	def __init__(self,lineno,type1,type2=None):
		self.lineno = lineno
		self.type1 = type1
		self.type2 = type2
	def __str__(self):
		if self.type2 is None:
			return 'ERROR: AssignmentTypeMismatchError at line '+str(self.lineno)+\
				'\nERR MSG: mismatch for type '+self.type1
		return 'ERROR: AssignmentTypeMismatchError at line '+str(self.lineno)+\
			'\nERR MSG: mismatch between types '+self.type1+' and '+self.type2

class FunctionOverloadingError(Exception):
	def __init__(self,lineno,fun):
		self.lineno = lineno
		self.fun = fun
	def __str__(self):
		return 'ERROR: FunctionOverloadingError at line '+str(self.lineno)+\
			'\nERR MSG: function '+self.fun+' overloaded'