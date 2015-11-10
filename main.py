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
f_param_type = []
f_isfunc = False
f_make_global_entry = False
f_ret_type = None
f_isdef = False
f_iscall = False
e_type = []
f_param_name = []
f_func_call = {''}

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

def t_eof(t):
    r'<<EOF>>'
    t.type = 'eof'
    if len(paran_stack)!=0:
        top = paran_stack[len(paran_stack)-1]
        raise err_handler.ParanMismatchError()
        #print('Paranthesis mismatch at:',str((top.lineno,find_column(f.read(),top))))
    print('Semantic Analysis Successful. No errors found.')

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
        VA_G : OP_EQU EXPR set_v_val
            | empty
        F : MK_SC make_func_entry
            | MK_LBRACE set_isdef make_func_entry reset_make_global_entry STMTS RCALL reset_isdef MK_RBRACE
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
        STMT : KW_IF MK_LPARAN EXPR MK_RPARAN MK_LBRACE STMTS MK_RBRACE KW_ELSE MK_LBRACE STMTS MK_RBRACE
            | KW_WHILE MK_LPARAN EXPR MK_RPARAN MK_LBRACE STMTS MK_RBRACE
            | DT ID set_name YB MK_SC
            | ID set_is_assignment OP_EQU VA_RHS check_assignment_semantics MK_SC
            | FCALL
        YB : MK_LPARAN set_isfunc FP MK_RPARAN make_func_entry
            | VA_L make_var_entry
        VA_L : OP_EQU VA_RHS
            | empty
        VA_RHS : EXPR set_v_val
    '''

def p_function_call(p):
    '''
        FCALL : ID set_name set_iscall MK_LPARAN AP MK_RPARAN reset_iscall check_func_call_semantics MK_SC
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

def p_set_is_assignment(p):
	'set_is_assignment :'
	global assignment_lhs_type
	#check if lhs exists
	var_details = l_symbol_table['var'].get(p[-1],-1)
	if var_details == -1:
		var_details = g_symbol_table['var'].get(p[-1],-1)
	if var_details == -1: raise err_handler.VariableNotDeclaredError
	else:
		#save assignment type
		assignment_lhs_type = var_details['type']

def p_check_assignment_semantics(p):
	'check_assignment_semantics :'
	global assignment_lhs_type,e_type
	if assignment_lhs_type != e_type[0]: raise err_handler.AssignmentTypeMismatchError
	p[0] = v_val
	e_type = []

def p_add( p ) :
	'''
		EXPR : EXPR OP_PLU EXPR
	'''
	global e_type
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
	else:
		e_type.pop()
	p[0]=p[1]+p[3]

def p_sub( p ) :
	'''
		EXPR : EXPR OP_MIN EXPR
	'''
	global e_type
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
	else:
		e_type.pop()
	p[0]=p[1]-p[3]

def p_mul( p ) :
	'''
		EXPR : EXPR OP_MUL EXPR
	'''
	global e_type
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
	else:
		e_type.pop()
	p[0]=p[1]*p[3]

def p_div( p ) :
	'''
		EXPR : EXPR OP_DIV EXPR
	'''
	global e_type
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
	else:
		e_type.pop()
	p[0]=p[1]/p[3]

def p_eq( p ) :
	'''
		EXPR : EXPR RELOP_EQ EXPR
	'''
	global e_type
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
	else:
		e_type.pop()
	if p[1]!=1 and p[3]!=0:
		p[0]=1
	else:
		p[0]=0

def p_or( p ) :
	'''
		EXPR : EXPR LOGIC_OR EXPR
	'''
	global e_type
	if e_type[0] != e_type[1]:
		raise err_handler.InvalidOperandError
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
	var_details = l_symbol_table['var'].get(p[1],-1)
	if var_details == -1:
		var_details = g_symbol_table['var'].get(p[1],-1)
	if var_details == -1: raise err_handler.VariableNotDeclaredError
	else:
		e_type.append(var_details['type'])
		p[0] = var_details['value']

def p_check_func_call_semantics(p):
    'check_func_call_semantics :'
    global l_symbol_table,g_symbol_table,g_name
    global f_param_name,f_param_type
    # check func scope in local -> global -> error
    func_details = l_symbol_table['func'].get(g_name,-1)
    if func_details == -1:
    	func_details = g_symbol_table['func'].get(g_name,-1)
    if func_details == -1:
    	raise err_handler.FunctionNotDeclaredError
    else:
	    # check func param count
	    if len(f_param_name) != len(func_details['param']):
	    	raise err_handler.FunctionNotDeclaredError
	    # for each actual param in 'f_param_name'-
	    # check if it exists in scope
	    j=0
	    for i,param in enumerate(f_param_name):
	    	if type(param) is str:	    		
	    		var_details = l_symbol_table['var'].get(param,-1)
	    		if var_details == -1:
	    			var_details = g_symbol_table['var'].get(param,-1)
	    			if var_details == -1:
	    				raise err_handler.VariableNotDeclaredError
	    			else:
	    				# check type against 'g_name' function param type
	    				if func_details['param'][i] != var_details['type']:
	    					raise err_handler.FunctionNotDeclaredError
	    	else:
	    		if func_details['param'][i] != f_param_type[j]:
	    			raise err_handler.FunctionNotDeclaredError
	    			j+=1
    f_param_name = []
    f_param_type = []

def p_check_return_semantics(p):
	'check_return_semantics :'
	global f_ret_type
	if f_ret_type != e_type[0]:
		raise err_handler.ReturnTypeMismatchError
	f_ret_type = None

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
        else:
            raise err_handler.VariableRedeclarationError

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
    check_var_semantics()
    if f_isdef:
        if n_type is not None: l_symbol_table['var'][g_name] = {'type':g_type,'value':v_val}
        else: l_symbol_table['var'][g_name] = {'type':g_type,'value':{'int':0,'float':0.0}[g_type]}
    else:
        if n_type is not None: g_symbol_table['var'][g_name] = {'type':g_type,'value':v_val}
        else: g_symbol_table['var'][g_name] = {'type':g_type,'value':{'int':0,'float':0.0}[g_type]}
    
    reset_var_globals()

def p_make_func_entry(p):
	'make_func_entry :'
	global f_ret_type
	check_func_semantics()
	if f_isdef and not f_make_global_entry:
		if g_name in l_symbol_table['func']: 
			raise err_handler.FunctionRedeclarationError
		else: l_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param_type}
	else:
		if g_name not in g_symbol_table['func']:
			g_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param_type}
		else:
			func_details = g_symbol_table['func'][g_name]
			if g_type != func_details['return_type']: raise err_handler.FunctionOverloadingError
			for i,param in enumerate(f_param_type):
				if param != func_details['param'][i]: raise err_handler.FunctionOverloadingError
		f_ret_type = g_type

	reset_func_globals()

def p_error(p):
    print('syntax error',p)

def p_empty(p):
    'empty :'
    pass 

def check_var_semantics():
	#check variable type, rhs type match
	if len(e_type) != 0:
		if g_type != e_type[0]: raise err_handler.VariableTypeError
	#check is variable is already defined
	if f_isdef:
		if g_name in l_symbol_table['var']: raise err_handler.VariableRedeclarationError
	else:
		if g_name in g_symbol_table['var']: raise err_handler.VariableRedeclarationError
	#check if declared var_type matches num_type
	if n_type is not None:
		if n_type is not keywords[g_type]: raise err_handler.VariableTypeError

def reset_var_globals():
    global v_val,n_type,e_type
    v_val = 0
    e_type = []
    n_type = None

def check_func_semantics():
	#check is func is already defined
	if not f_make_global_entry:	
		if f_isdef:
			if g_name in l_symbol_table['func']: raise err_handler.FunctionRedeclarationError
		if g_name in g_symbol_table['func']: raise err_handler.FunctionRedeclarationError

def reset_func_globals():
    global g_name,f_param_name,f_param_type,g_type,f_isfunc
    g_name = None
    g_type = None
    f_param_name = []
    f_param_type = []
    f_isfunc = False

lexer = lex.lex()
parser = yacc.yacc()
parser.parse('''
	int add(int a,int b);
    int sum(int a, int b){
        int p = 10 && 3 + 20 / 200;
        int r = 40 + (a*b)/p;
        return p;
    }
    int add(int a, int b){
    	return a+b;
    }
    ''')