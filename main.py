import ply.yacc as yacc
import ply.lex as lex
import ErrorHandler as err_handler
import os

# File object for writing the output of the symbol table
file_symbol = None

# File object for writing the output of the Semantic Analysis of C Program
file_writer = None

#globals
g_symbol_table = {'var':{},'func':{}}
l_symbol_table = {'var':{},'func':{}}
paran_stack = []

g_name = None
g_type = None
v_val = None
n_type = None
f_param_type = []
f_isfunc = False
f_make_global_entry = False
f_ret_type = None
assignment_lhs_type = None
f_isdef = False
f_iscall = False
f_name = None
e_type = []
f_param_name = []
f_func_call = {''}
e_lineno = None

keywords = {
        'if':'KW_IF',
        'else':'KW_ELSE',
        'while':'KW_WHILE',
        'return':'KW_RETURN',
        'int':'T_INT',
        'float':'T_FLOAT'
    }
    
operators = {
        '=' : 'OP_EQU',
        '+' : 'OP_PLU',
        '-' : 'OP_MIN',
        '*' : 'OP_MUL',
        '/' : 'OP_DIV',
        '<' : 'RELOP_LT',
        '<=' : 'RELOP_LE',
        '>' : 'RELOP_GT',
        '>=' : 'RELOP_GE',
        '!=' : 'RELOP_NE',
        '==' : 'RELOP_EQ',
        '&&' : 'LOGIC_AND',
        '||' : 'LOGIC_OR'
    }

precedence = (
	('left','LOGIC_AND','LOGIC_OR'),
	('left','RELOP_EQ','RELOP_NE'),
	('left','RELOP_GT','RELOP_LT','RELOP_GE','RELOP_LE'),
	( 'left', 'OP_PLU', 'OP_MIN' ),
	( 'left', 'OP_MUL', 'OP_DIV' ),
	( 'nonassoc', 'UMINUS' )
)


markers = {
    ';' : 'MK_SC',
    ',' : 'MK_CM',
    '(' : 'MK_LPARAN',
    ')' : 'MK_RPARAN',
    '{' : 'MK_LBRACE',
    '}' : 'MK_RBRACE'
}

tokens = ['ID','NUM_FLOAT','NUM_INT'] + list(keywords.values()) + list(operators.values()) + list(markers.values()) 
	
def t_OP(t):
    r'(\+|\*|-|/|=|/|<|<=|>|>=|!=|==|&&|\|\|)'
    t.type = operators[t.value]    
    return t

def t_MK(t):
	r'(;|,|\(|\)|\{|\})'
	t.type = markers[t.value]
	return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    global f_param_name
    t.type = keywords.get(t.value,'ID')
    if (f_isfunc or f_iscall) and t.value not in keywords:
    	f_param_name.append(t.value)
    return t

def t_NUM(t):
    r'[0-9]+(\.[0-9]+)?'
    global n_type,f_param_type,f_param_name
    if '.' in t.value:
        n_type = 'T_FLOAT'
        t.type = 'NUM_FLOAT'
        t.value = float(t.value)
        if f_iscall:
        	f_param_type.append('float')
        	f_param_name.append(t.value)
    else:
        n_type = 'T_INT'
        t.type = 'NUM_INT'
        t.value = int(t.value)
        if f_iscall:
        	f_param_type.append('int')
        	f_param_name.append(t.value)
    return t

def t_newline(t):
    r'\n+'
    global e_lineno
    t.lexer.lineno += len(t.value)
    e_lineno = t.lexer.lineno
    t.lexer.skip(len(t.value))

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

t_ignore = ' \t'

def t_error(t):
	print('Illegal character',t.value[0],'at line:'+str(t.lineno)+' pos:'+str(find_column(src.read(),t)))
	illegal_character_statement = 'Illegal Character',t.value[0],'at line:'+str(t.lineno)+' pos:'+str(find_column(src.read(), t))
	file_writer.write(str(illegal_character_statement)+"\n")
	t.lexer.skip(1)

def p_start(p):
	'''
		_S : S seen_eof
	'''

def p_seen_eof(p):
	'seen_eof :'
	check_paran_mismatch()
	print('Semantic Analysis Completed.')
	file_writer.write('Semantic Analysis Completed.\n')

# CONSTRUCT - definition of function or declaration of function/var
def p_construct(p):
    '''
        S : S CONSTRUCT seen_eoc
            | empty
    '''

def p_seen_eoc(p):
	'seen_eoc :'
	check_paran_mismatch()

# Y - branch b/w function and variable
# VAG - variable assignment in global space, RHS can only be constants.
# F - function(declaration/definition)
# FP - formal parameters
# P - parameter
def p_base(p):
    '''
        CONSTRUCT : DT ID set_name Y 
        Y : MK_LPARAN seen_lparan set_isfunc FP MK_RPARAN seen_rparan F
            | VA_G MK_SC make_var_entry
        VA_G : OP_EQU EXPR set_v_val
            | empty
        F : MK_SC make_func_entry
            | MK_LBRACE seen_lbrace set_isdef make_func_entry reset_make_global_entry STMTS RCALL reset_isdef MK_RBRACE seen_rbrace
        FP : P MK_CM FP
            | P
        P : DT ID
    '''

# VAL - variable assignment in local, RHS can be number/expression/function call
# YS - branch in statements
# PS - statement parameters
# CP - function call parameters
def p_statements(p):
    '''
        STMTS : STMT STMTS
            | empty
        STMT : KW_IF MK_LPARAN seen_lparan EXPR MK_RPARAN seen_rparan MK_LBRACE seen_lbrace STMTS MK_RBRACE seen_rbrace KW_ELSE MK_LBRACE seen_lbrace STMTS MK_RBRACE seen_rbrace
            | KW_WHILE MK_LPARAN seen_lparan EXPR MK_RPARAN seen_rparan MK_LBRACE seen_lbrace STMTS MK_RBRACE seen_rbrace
            | DT ID set_is_dec_assignment YB MK_SC
            | ID set_is_assignment OP_EQU VA_RHS check_assignment_semantics MK_SC
            | FCALL MK_SC
        YB : MK_LPARAN seen_lparan set_isfunc FP MK_RPARAN seen_rparan make_func_entry
            | VA_L make_var_entry
        VA_L : OP_EQU VA_RHS
            | empty
        VA_RHS : EXPR set_v_val
        	| FCALL check_assignment_semantics
    '''

def p_function_call(p):
	'''
		FCALL : ID set_name set_iscall MK_LPARAN seen_lparan AP MK_RPARAN seen_rparan reset_iscall check_func_call_semantics        
	'''
	global e_type
	try:
		func_details = l_symbol_table['func'].get(g_name,-1)
		if func_details == -1:
			func_details = g_symbol_table['func'].get(g_name,-1)
		if func_details == -1: 
			raise FunctionNotDeclaredError(e_lineno-1,g_name)
		else:

			e_type.append(func_details['return_type'])
	except err_handler.FunctionNotDeclaredError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)


def p_function_call_param(p):
	'''
		AP : AP MK_CM P2
        	| P2
        P2 : ID
            | NUM
	'''

def p_return_call(p):
    '''
        RCALL : KW_RETURN EXPR MK_SC check_return_semantics
        	| empty
    '''

def p_numeric(p):
    '''
        NUM : NUM_INT
            | NUM_FLOAT
    '''
    p[0]=p[1]

def p_datatype(p):
    '''
        DT : T_INT set_type
            | T_FLOAT set_type
    '''


def p_set_is_dec_assignment(p):
	'set_is_dec_assignment :'
	global assignment_lhs_type, f_name
	assignment_lhs_type = g_type
	f_name = p[-1]

def p_set_is_assignment(p):
	'set_is_assignment :'
	global assignment_lhs_type, f_name,e_lineno
	#check if lhs exists
	try:
		var_details = l_symbol_table['var'].get(p[-1],-1)
		if var_details == -1:
			var_details = g_symbol_table['var'].get(p[-1],-1)
		if var_details == -1: raise err_handler.VariableNotDeclaredError(e_lineno-1,p[-1])
	except err_handler.VariableNotDeclaredError as e:
		e_lineno.append(str(e))
	else:
		#save assignment type
		assignment_lhs_type = var_details['type']
		f_name = p[-1]

def p_check_assignment_semantics(p):
	'check_assignment_semantics :'
	global assignment_lhs_type,e_type
	try:
		if assignment_lhs_type != e_type[0]: raise err_handler.AssignmentTypeMismatchError(e_lineno-1,assignment_lhs_type,e_type[0])
	except err_handler.AssignmentTypeMismatchError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)

	p[0] = v_val
	e_type = []

def p_add( p ) :
	'''
		EXPR : EXPR OP_PLU EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	p[0]=p[1]+p[3]

def p_sub( p ) :
	'''
		EXPR : EXPR OP_MIN EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	p[0]=p[1]-p[3]

def p_mul( p ) :
	'''
		EXPR : EXPR OP_MUL EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	p[0]=p[1]*p[3]

def p_div( p ) :
	'''
		EXPR : EXPR OP_DIV EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	p[0]=p[1]/p[3]

def p_eq( p ) :
	'''
		EXPR : EXPR RELOP_EQ EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1] == p[3]:
		p[0]=1
	else:
		p[0]=0

def p_ne( p ) :
	'''
		EXPR : EXPR RELOP_NE EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1] != p[3]:
		p[0]=1
	else:
		p[0]=0

def p_ge( p ) :
	'''
		EXPR : EXPR RELOP_GE EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1] >= p[3]:
		p[0]=1
	else:
		p[0]=0

def p_le( p ) :
	'''
		EXPR : EXPR RELOP_LE EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1] <= p[3]:
		p[0]=1
	else:
		p[0]=0

def p_gt( p ) :
	'''
		EXPR : EXPR RELOP_GT EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1] > p[3]:
		p[0]=1
	else:
		p[0]=0

def p_lt( p ) :
	'''
		EXPR : EXPR RELOP_LT EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1] < p[3]:
		p[0]=1
	else:
		p[0]=0

def p_and( p ) :
	'''
		EXPR : EXPR LOGIC_AND EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1]!=0 and p[3]!=0:
		p[0]=1
	else:
		p[0]=0

def p_or( p ) :
	'''
		EXPR : EXPR LOGIC_OR EXPR
	'''
	global e_type
	try:	
		try:
			if e_type[0] != e_type[1]:
				raise err_handler.InvalidOperandError(e_lineno-1)
		except IndexError as e2:
			raise err_handler.InvalidOperandError(e_lineno-1,p[2])
	except err_handler.InvalidOperandError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.pop()
	if p[1]!=0 or p[3]!=0:
		p[0]=1
	else:
		p[0]=0

def p_parens( p ) :
	'EXPR : MK_LPARAN EXPR MK_RPARAN'
	p[0] = p[2]

def p_expr2uminus( p ) :
	'EXPR : OP_MIN EXPR %prec UMINUS'
	p[0] = -p[2]

def p_expr2NUM( p ) :
	'EXPR : NUM'
	global e_type
	type_translate = {'T_INT':'int','T_FLOAT':'float'}
	e_type.append(type_translate[n_type])
	p[0] = p[1]

def p_expr2ID( p ) :
	'EXPR : ID'
	global e_type,l_symbol_table,g_symbol_table
	try:
		var_details = l_symbol_table['var'].get(p[1],-1)
		if var_details == -1:
			var_details = g_symbol_table['var'].get(p[1],-1)
		if var_details == -1: raise err_handler.VariableNotDeclaredError(e_lineno-1,p[1])
	except err_handler.VariableNotDeclaredError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	else:
		e_type.append(var_details['type'])
		p[0] = var_details['value']

def p_seen_lparan(p):
	'seen_lparan :'
	global paran_stack	
	try:
		if p[-1] != '(':
			raise err_handler.ParanMismatchError(e_lineno-1,"missing '('")
		else:
			paran_stack.append('(')
	except err_handler.ParanMismatchError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)	

def p_seen_rparan(p):
	'seen_rparan :'
	global paran_stack
	try:
		if len(paran_stack) == 0: raise err_handler.ParanMismatchError(e_lineno-1,"match not found for ')'")
		else:
			if paran_stack[len(paran_stack)-1] == '(': paran_stack.pop()
			else: raise err_handler.ParanMismatchError(e_lineno-1,"match not found for ')'")
	except err_handler.ParanMismatchError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)

def p_seen_lbrace(p):
	'seen_lbrace :'
	try:
		if p[-1] != '{':
			raise err_handler.ParanMismatchError(e_lineno-1,"missing '{'")
		else:
			paran_stack.append('{')
	except err_handler.ParanMismatchError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)	

def p_seen_rbrace(p):
	'seen_rbrace :'
	global paran_stack
	try:
		if len(paran_stack) == 0: raise err_handler.ParanMismatchError(e_lineno-1,"match not found for '}'")
		else:
			if paran_stack[len(paran_stack)-1] == '{': paran_stack.pop()
			else: raise err_handler.ParanMismatchError(e_lineno-1,"match not found for '}'")
	except err_handler.ParanMismatchError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)

def p_check_func_call_semantics(p):
	'check_func_call_semantics :'
	global l_symbol_table,g_symbol_table,g_name
	global f_param_name,f_param_type
	# check func scope in local -> global -> error
	try:
		func_details = l_symbol_table['func'].get(g_name,-1)
		if func_details == -1:
			func_details = g_symbol_table['func'].get(g_name,-1)
		if func_details == -1:
			raise err_handler.FunctionNotDeclaredError(e_lineno-1,g_name)
		else:
			# check func param count
			if len(f_param_name) != len(func_details['param']):
				raise err_handler.FunctionNotDeclaredError(e_lineno-1,g_name)
			# for each actual param in 'f_param_name'-
			# check if it exists in scope
			j=0
			for i,param in enumerate(f_param_name):
				if type(param) is str:					
					var_details = l_symbol_table['var'].get(p[-1],-1)
					if var_details == -1:
						var_details = g_symbol_table['var'].get(p[-1],-1)
					if var_details == -1: raise err_handler.VariableNotDeclaredError(e_lineno-1,p[-1])
					else:
						# check type against 'g_name' function param type
						if func_details['param'][i] != var_details['type']:
							raise err_handler.FunctionNotDeclaredError(e_lineno-1,g_name)
				else:
					if func_details['param'][i] != f_param_type[j]:
						raise err_handler.FunctionNotDeclaredError(e_lineno-1,g_name)
						j+=1
		f_param_name = []
		f_param_type = []
	except err_handler.VariableNotDeclaredError as e:
		print(str(e))
		file.writer.write(str(e)+"\n")
		exit(0)
	except err_handler.FunctionNotDeclaredError as e2:
		print(str(e2))	
		file_writer.write(str(e2)+"\n")
		exit(0)

def p_check_return_semantics(p):
	'check_return_semantics :'
	global f_ret_type, e_type
	try:
		if len(e_type)==0: raise err_handler.ReturnTypeMismatchError(e_lineno-1,f_ret_type)
		elif f_ret_type != e_type[0]: raise err_handler.ReturnTypeMismatchError(e_lineno-1,f_ret_type,e_type[0])
			
	except err_handler.ReturnTypeMismatchError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	f_ret_type = None
	e_type = []

def p_set_isfunc(p):
    'set_isfunc :'
    global f_isfunc
    f_isfunc = True

def p_set_iscall(p):
    'set_iscall :'
    global f_iscall
    f_iscall = True

def p_reset_iscall(p):
    'reset_iscall :'
    global f_iscall
    f_iscall = False

def p_set_isdef(p):
	'set_isdef :'
	global f_isdef, l_symbol_table,f_param_name,f_param_type, f_make_global_entry
	f_make_global_entry = True
	f_isdef = True
	for  v_type,v_name in zip(f_param_type,f_param_name):
		if v_name not in l_symbol_table['var']:
			l_symbol_table['var'][v_name] = {'type': v_type, 'value':{'int':0,'float':0.0}[v_type]}
			local_writer.write(str(l_symbol_table['var'][v_name])+"\n")
		else:
			try:
				raise err_handler.VariableRedeclarationError(e_lineno-1,v_name)
			except err_handler.VariableRedeclarationError as e:
 				print(str(e))
				file_writer.write(str(e)+"\n")
 				exit(0)

def p_reset_make_global_entry(p):
	'reset_make_global_entry :'
	global f_make_global_entry
	f_make_global_entry = False

def p_reset_isdef(p):
	'reset_isdef :'
	global f_isdef, l_symbol_table, e_type
	f_isdef = False
	e_type = []
	l_symbol_table = {'var':{},'func':{}}
	local_writer.write("Local symbol reset\n")

def p_set_type(p):
    'set_type :'
    global g_type,f_param_type
    if f_isfunc:
        f_param_type.append(p[-1])
    else:
        g_type = p[-1]    

def p_set_name(p):
    'set_name :'
    global g_name
    g_name = p[-1]

def p_set_v_val(p):
    'set_v_val :'
    global v_val
    v_val = p[-1]


def p_make_var_entry(p):
    'make_var_entry :'
    global g_name, f_name
    if f_name is not None:
    	g_name = f_name
    	f_name = None
    check_var_semantics()
    if f_isdef:
        if n_type is not None: 
			l_symbol_table['var'][g_name] = {'type':g_type,'value':v_val}
			local_writer.write(str(l_symbol_table['var'][g_name])+"\n")
        else: 
			l_symbol_table['var'][g_name] = {'type':g_type,'value':{'int':0,'float':0.0}[g_type]}
			local_writer.write(str(l_symbol_table['var'][g_name])+"\n")
    else:
        if n_type is not None: 
			g_symbol_table['var'][g_name] = {'type':g_type,'value':v_val}
			symbol_writer.write(str(g_symbol_table['var'][g_name])+"\n")
        else: 
			g_symbol_table['var'][g_name] = {'type':g_type,'value':{'int':0,'float':0.0}[g_type]}
			symbol_writer.write(str(g_symbol_table['var'][g_name])+"\n")
    
    reset_var_globals()

def p_make_func_entry(p):
	'make_func_entry :'
	global f_ret_type
	check_func_semantics()
	if f_isdef and not f_make_global_entry:
		if g_name in l_symbol_table['func']: 
			raise err_handler.FunctionRedeclarationError
		else: 
			l_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param_type}
			local_writer.write(str(l_symbol_table['func'][g_name])+"\n")
	else:
		if g_name not in g_symbol_table['func']:
			g_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param_type}
			symbol_writer.write(str(g_symbol_table['func'][g_name])+"\n")
		else:
			try:
				func_details = g_symbol_table['func'][g_name]
				if g_type != func_details['return_type']: raise err_handler.FunctionOverloadingError(e_lineno-1,g_name)
				for i,param in enumerate(f_param_type):
					if param != func_details['param'][i]: raise err_handler.FunctionOverloadingError(e_lineno-1,g_name)
			except err_handler.FunctionOverloadingError as e:
				print(str(e))
				file_writer.write(str(e)+"\n")
				exit(0)
		f_ret_type = g_type

	reset_func_globals()

def p_error(p):
	print('syntax error at line',e_lineno-1)
	syntax_error_print = 'syntax error at line',e_lineno-1
	file_writer.write(str(syntax_error_print)+"\n")
	if p.type in ['MK_SC','T_INT','T_FLOAT']:
		print('parenthesis not found near',p.value)
		parenthesis_error_print = 'parenthesis not found near',p.value
		file_writer.write(str(parenthesis_error_print)+"\n")
	if p.type in ['ID']:
		print('Invalid Identifier')
		file_writer.write('Invalid Identifier\n')
	exit(0)

def check_paran_mismatch():
	if len(paran_stack)!=0:
		try:
			raise err_handler.ParanMismatchError(e_lineno-1,'closing paranthesis not found')
		except err_handler.ParanMismatchError as e:
			print(str(e))
			file_writer.write(str(e)+"\n")
			exit(0)

def p_empty(p):
    'empty :'
    pass 

def check_var_semantics():
	try:
		#check variable type, rhs type match
		if len(e_type) != 0:
			if g_type != e_type[0]: raise err_handler.VariableTypeError(e_lineno-1,g_type,e_type[0])
		#check is variable is already defined
		if f_isdef:
			if g_name in l_symbol_table['var']: raise err_handler.VariableRedeclarationError(e_lineno-1,g_name)
		else:
			if g_name in g_symbol_table['var']: raise err_handler.VariableRedeclarationError(e_lineno-1,g_name)
		#check if declared var_type matches num_type
		if n_type is not None:
			if n_type is not keywords[g_type]: raise err_handler.VariableTypeError(e_lineno-1,n_type)
	except err_handler.VariableTypeError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	except err_handler.VariableRedeclarationError as e2:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)
	except Exception as e3:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)

def reset_var_globals():
    global v_val,n_type,e_type
    v_val = 0
    e_type = []
    n_type = None

def check_func_semantics():
	#check is func is already defined
	try:
		if not f_make_global_entry:	
			if f_isdef:
				if g_name in l_symbol_table['func']: raise err_handler.FunctionRedeclarationError(e_lineno-1,g_name)
			if g_name in g_symbol_table['func']: raise err_handler.FunctionRedeclarationError(e_lineno-1,g_name)
	except err_handler.FunctionRedeclarationError as e:
		print(str(e))
		file_writer.write(str(e)+"\n")
		exit(0)

def reset_func_globals():
    global g_name,f_param_name,f_param_type,g_type,f_isfunc
    g_name = None
    g_type = None
    f_param_name = []
    f_param_type = []
    f_isfunc = False

def called_main(global_name):
	# global_name is the path of the C Program passed as input
	global file_writer
	global symbol_writer
	global local_writer
	
	output_filepath = "C:\\Users\\Aditya\\Desktop\\13CS306\\13CS306\\semantic_analyzer_output.txt"
	symbol_filepath = "C:\\Users\\Aditya\\Desktop\\13CS306\\13CS306\\semantic_symbol_table.txt"
	local_filepath  = "C:\\Users\\Aditya\\Desktop\\13CS306\\13CS306\\semantic_local_table.txt"
	
	# Create the files, overwrites the existing files
	os.system("touch "+output_filepath)
	os.system("touch "+symbol_filepath)
	os.system("touch "+local_filepath)
	
	symbol_writer = open(symbol_filepath, 'w')
	file_writer = open(output_filepath, 'w')
	local_writer = open(local_filepath, 'w')
	
	# File object for the C Program
	file_readprogram = open(global_name, 'r')
	lexer = lex.lex()
	parser = yacc.yacc()
	parser.parse(file_readprogram.read())
	
	# Close the files
	file_readprogram.close()
	symbol_writer.close()
	file_writer.close()
	local_writer.close()