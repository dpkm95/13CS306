"""
	C program is logically divided into 3 sections	
	section 1 - header
		function declaration, function definition, var declaration

	section 2 - main function
		variable declaration, function declaration, func calls

	section 3 - after main
		var declaration, function definition
""" 
class Record:
	def __init__(self,**kwargs):
		if kwargs['type'] == 'function':
			self.record_type = kwargs['type']
			self.name = kwargs['name']
			self.params = kwargs['params']
			self.return_type = kwargs['return_type']
		elif kwargs['type'] == 'variable':
			self.record_type = kwargs['type']
			self.name = kwargs['name']
			self.variable_type = kwargs['variable_type']