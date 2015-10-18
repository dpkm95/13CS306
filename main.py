"""
	Limited Semantic Analyser for C program. Performing the following checks-
	i. Function semantics
	ii. Parameter matching

	Assumptions:
		> there is no pointers involved in function signatures.

	ToDo:
		> move all handlers to FunctionAnalyser module
		> move all exceptions to SemanticAnalyserExceptions module
		> create regex to find error line and line_no
"""

#Imports
import sys
import re
import FunctionAnalyser as f_analyser
import ParanthesisAnalyser as p_analyser
import SemanticAnalyserExceptions as sa_exceptions

#Exceptions
class InvalidArgumentError(Exception):
	"""
		main.py expects source code as command line argument.
	"""
	def __str__(self):
		return "\bError message: Invalid arguments."

class FunctionDeclarationError(Exception):
	"""
		Raised if-
		i. return type is invalid.
		ii. parameter type is invalid.
		iii. function name is invalid.
	"""
	def __init__(self,error_msg,line_no=-1,line=""):
		self.error_msg = "FunctionDeclarationError: at "+str(line_no)+") "+line+\
				"\nError message: "+error_msg
	def __str__(self):
		return self.error_msg

#Globals

#Main
if __name__=='__main__':
	print('semantic analysis started')
	try:
		#It is mandatory to enter C program source file.
		#eg: python3 main.py test.c	
		if len(sys.argv)==2:
			#Variables
			variable_data_types = ['int','float','double','void','char']
			line_no = 0
			function_table = {}
			variable_table = {}

			#Regex
			re_function_declaration_call = re.compile(r'(.*?)\s*([a-z][\w]*?)[(](.*?)[)]\s*;')
			re_function_definition = re.compile(r'(.*?)\s*?([a-z][\w]*?)[(](.*?)[)]\s*?[{]')
			re_variable_declaration = re.compile(r'[\w* ,]*;')
			
			with open(sys.argv[1],'r') as f:
				src = f.read()

				vars_info = re_variable_declaration.findall(src)
				#variable declaration handling
				if vars_info:
					for var_info in vars_info:
						var_info = re.split('\s*',var_info.strip())
						if var_info[0] in variable_data_types:
							# print(var_info)
							for var in var_info[1:]:
								id = re.search('\w*',var)
								if id:
									id = re.search('\w*',var).group()
									variable_table[id]=f_analyser.Record(type='variable',name=id,variable_type=var_info[0])

				funcs_info = re_function_definition.findall(src)
				#function definition handling
				if funcs_info:
					# print(funcs_info)
					# function definition handler code
					pass
				
				func_info = re_function_declaration_call.findall(src)
				#function declaration handling
				if func_info:
					for i in range(len(func_info)):
						#print(func_info[i])
						#check for function declaration
						arg = func_info[i][2].split(',')[0]
						#if first lexeme in paranthesis is a data type, it is a function declaration
						if re.split('\s*',arg.strip())[0] in variable_data_types:
							if func_info[i][0] not in variable_data_types:
								raise FunctionDeclarationError("invalid return type "+func_info[i][0])
								# raise FunctionDeclarationError("invalid return type "+func_info[i][0],line_no,line)
							params = []
							for arg in func_info[i][2].split(','):
								arg_type = re.split('\s*',arg.strip())[0]
								if arg_type not in variable_data_types:
									raise FunctionDeclarationError("invalid parameter type "+arg)
									# raise FunctionDeclarationError("invalid parameter type "+arg,line_no,line)
								params.append(arg_type)
							function_table[func_info[i][1]] = f_analyser.Record(type='function',name=func_info[i][1],params=params,return_type=func_info[i][0])
				#function call handling
				else:
					#function call handler code
					pass
			#func table contents
			for i in function_table:
				print(function_table[i].name)
			#var table contents
			for i in variable_table:
				print(variable_table[i].name)
			print('semantic analysis ended')
		else:
			raise InvalidArgumentError
	except InvalidArgumentError:
		print("ERROR: InvalidArgumentError\n",sys.exc_info()[1])
	except FunctionDeclarationError:
		print(sys.exc_info()[1])
	except FileNotFoundError:
		print("ERROR: FileNotFoundError\nError message: File",input_file,"not found.")