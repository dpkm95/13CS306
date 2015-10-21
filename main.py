import ply.yacc as yacc
import ply.lex as lex
import ErrorHandler as err_handler

#globals
g_symbol_table = {'var':{},'func':{}}
l_symbol_table = {'var':{},'func':{}}
paran_stack = []

g_name = None
g_type = None
v_val = None
n_type = None
f_param = []
f_isfunc = 0

keywords = {
        'if':'KW_IF',
        'else':'KW_ELSE',
        'while':'KW_WHILE',
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
		'==' : 'RELOP_EQ'
	}
	
markers = {
	';' : 'MK_SC',
	',' : 'MK_CM',
	'(' : 'MK_LPARAN',
	')' : 'MK_RPARAN',
	'{' : 'MK_LBRACE',
	'}' : 'MK_RBRACE'
}

tokens = ['ID','NUM_FLOAT','NUM_INT','eof'] + list(keywords.values()) + list(operators.values()) + list(markers.values()) 

def t_OP(t):
	r'(\+|\*|-|/|=)'
	t.type = operators[t.value]
	print(t)
	return t

def t_MK(t):
	r'(;|,|\(|\)|\{|\})'
	t.type = markers[t.value]
	if t.value == '(' or t.value == '{':
		paran_stack.append(t)
	elif t.value == ')':
		if len(paran_stack)!=0: 
			top = paran_stack[len(paran_stack)-1]
		else: 
			raise err_handler.ParanMismatchError
		if top.value is '(':
			paran_stack.pop()
		else:
			raise err_handler.ParanMismatchError
	elif t.value == '}':
		if len(paran_stack)!=0: 
			top = paran_stack[len(paran_stack)-1]
		else: 
			raise err_handler.ParanMismatchError
		if top.value is '{':
			paran_stack.pop()
		else:
			raise err_handler.ParanMismatchError
	return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = keywords.get(t.value,'ID')
    print(t)
    return t

def t_NUM(t):
    r'[0-9]+(\.[0-9]+)?'
    global n_type
    if '.' in t.value:
        n_type = 'T_FLOAT'
        t.type = 'NUM_FLOAT'
        t.value = float(t.value)
    else:
        n_type = 'T_INT'
        t.type = 'NUM_INT'
        t.value = int(t.value)
    print(t)
    return t

def t_eof(t):
    r'<<EOF>>'
    print(t)
    if len(paran_stack)!=0:
	    top = paran_stack[len(paran_stack)-1]
	    raise err_handler.ParanMismatchError()
	    #print('Paranthesis mismatch at:',str((top.lineno,find_column(f.read(),top))))

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    t.lexer.skip(len(t.value))

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

t_ignore = ' \t'

def t_error(t):
    print('Illegal character',t.value[0],'at line:'+str(t.lineno)\
          +' pos:'+str(find_column(src.read(),t)))
    t.lexer.skip(1)

# CONSTRUCT - definition of function or declaration of function/var
def p_start(p):
    '''
        S : CONSTRUCT S
            | empty
    '''

# Y - branch b/w function and variable
# VAG - variable assignment in global space, RHS can only be constants.
# F - function(declaration/definition)
# FP - formal parameters
# P - parameter
def p_base(p):
    '''
        CONSTRUCT : DT ID set_name Y 
        Y : MK_LPARAN set_isfunc FP MK_RPARAN F
            | VA_G MK_SC make_var_entry
        VA_G : OP_EQU NUM
            | empty
        F : MK_SC make_func_entry set_isdec
        	| MK_LBRACE make_func_entry set_isdef STMTS MK_RBRACE
        FP : P MK_CM FP
            | P
        P : DT ID
    '''

# VAL - variable assignment in local, RHS can be number/expression/function call
def p_statements(p):
	'''
		STMTS : STMT STMTS
			| empty
		STMT : KW_IF MK_LPARAN EXPR MK_RPARAN MK_LBRACE STMT MK_RBRACE KW_ELSE MK_LBRACE STMT MK_RBRACE
			| KW_WHILE MK_LPARAN EXPR MK_RPARAN MK_LBRACE STMT MK_RBRACE
			| DT ID set_name VA_L MK_SC make_var_entry
			| FCALL
		VA_L : OP_EQU VA_RHS
			| empty
		VA_RHS : NUM
			| EXPR
			| FCALL
	'''

def p_expression(p):
	'''
		EXPR : ID
	'''

def p_function_call(p):
	'''
		FCALL : ID MK_LPARAN AP MK_RPARAN MK_SC
		AP : ID MK_CM AP
			| ID
	'''

def p_numeric(p):
    '''
        NUM : NUM_INT set_v_val
            | NUM_FLOAT set_v_val
    '''

def p_datatype(p):
    '''
        DT : T_INT set_type
            | T_FLOAT set_type
    '''

def p_set_isfunc(p):
    'set_isfunc :'
    global f_isfunc
    f_isfunc = 1

def p_set_isdec(p):
    'set_isdec :'
    global f_isfunc
    f_isfunc = 2

def p_set_isdef(p):
    'set_isdef :'
    global f_isfunc
    f_isfunc = 3

def p_set_type(p):
    'set_type :'
    global g_type,f_isfunc,f_param
    if f_isfunc == 0 or f_isfunc == 3:
        g_type = p[-1]
    else:
        f_param.append(p[-1])

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
    print('in var',f_isfunc)
    check_var_semantics()
    if f_isfunc == 3:
    	if n_type is not None: l_symbol_table['var'][g_name] = {'type':g_type,'value':v_val}
    	else: l_symbol_table['var'][g_name] = {'type':g_type,'value':{'int':0,'float':0.0}[g_type]}
    else:
    	if n_type is not None: g_symbol_table['var'][g_name] = {'type':g_type,'value':v_val}
    	else: g_symbol_table['var'][g_name] = {'type':g_type,'value':{'int':0,'float':0.0}[g_type]}
    
    reset_var_globals()

def p_error(p):
    print('syntax error',p)

def check_var_semantics():
    #check is variable is already defined
    if f_isfunc == 3:
    	if g_name in l_symbol_table['var']: raise err_handler.VariableRedeclarationError
    else:
    	if g_name in g_symbol_table['var']: raise err_handler.VariableRedeclarationError
    #check if declared var_type matches num_type
    if n_type is not None:
        if n_type is not keywords[g_type]: raise err_handler.VariableTypeError

def reset_var_globals():
    global v_val,g_name,g_type
    v_val = 0
    g_name = None
    g_type = None
    n_type = None

def p_make_func_entry(p):
	'make_func_entry :'
	print('in func',f_isfunc)
	if f_isfunc == 3:
		if g_name in l_symbol_table['func']: 
			raise err_handler.FunctionRedeclarationError
		else: l_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param}
	else:
		if g_name in g_symbol_table['func']: 
			raise err_handler.FunctionRedeclarationError
		else:
			g_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param}	                
	reset_func_globals()


def p_empty(p):
    'empty :'
    pass    

def reset_func_globals():
    global g_name,f_param,g_type,f_isfunc
    g_name = None
    g_type = None
    f_param = []
    f_isfunc = 0

lexer = lex.lex()
# src = open('test.c')
# lexer.input('int a=10;')
# for i in lexer:
#     pass

parser = yacc.yacc()
parser.parse('''
		int a=10;
		int sum(int a, int b){
			int a=20;
			float b = 10.3;
		}
	''')
print(g_symbol_table)
print(l_symbol_table)