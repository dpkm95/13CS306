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
f_isdef = False
f_iscall = False
f_param_name = []
f_func_call = {''}

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
    global n_type
    if '.' in t.value:
        n_type = 'T_FLOAT'
        t.type = 'NUM_FLOAT'
        t.value = float(t.value)
    else:
        n_type = 'T_INT'
        t.type = 'NUM_INT'
        t.value = int(t.value)    
    return t

def t_eof(t):
    r'<<EOF>>'
    t.type = 'eof'
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
        F : MK_SC make_func_entry
            | MK_LBRACE make_func_entry set_isdef STMTS reset_isdef MK_RBRACE
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
            | FCALL
        YB : MK_LPARAN set_isfunc FP MK_RPARAN make_func_entry
            | VA_L make_var_entry
        VA_L : OP_EQU VA_RHS
            | empty
        VA_RHS : NUM
            | EXPR
    '''

def p_expression(p):
    '''
        EXPR : ID
    '''

def p_function_call(p):
    '''
        FCALL : ID set_name MK_LPARAN set_iscall AP MK_RPARAN MK_SC
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

def p_check_func_semantics(p):
    'set_func_semantics :'
    global l_symbol_table,g_symbol_table,g_name
    global f_param_name    

    # check func scope in local -> global -> error
    # check func param count 
    # for each actual param in 'f_param_name':
    # check if it exists in scope
    # check type against 'g_name' function param type

def p_set_isfunc(p):
    'set_isfunc :'
    global f_isfunc
    f_isfunc = True

def p_set_iscall(p):
    'set_iscall :'
    global f_iscall
    f_iscall = True

def p_set_isdef(p):
    'set_isdef :'
    global f_isdef, l_symbol_table,f_param_name,f_param_type
    f_isdef = True
    for type,name in zip(f_param_type,f_param_name):
        if name not in l_symbol_table['var']:
            l_symbol_table['var'][name] = {'type': type, 'value':{'int':0,'float':0.0}[type]}
        else:
            raise err_handler.VariableRedeclarationError
    f_param_type = []
    f_param_name = []

def p_reset_isdef(p):
    'reset_isdef :'
    global f_isdef, l_symbol_table
    f_isdef = False
    #l_symbol_table = {'var':{},'func':{}}

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
    check_func_semantics()
    if f_isdef:
        if g_name in l_symbol_table['func']: 
            raise err_handler.FunctionRedeclarationError
        else: l_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param_type}
    else:
        if g_name in g_symbol_table['func']: 
            raise err_handler.FunctionRedeclarationError
        else:
            g_symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : f_param_type}                    
    reset_func_globals()

def p_error(p):
    print('syntax error',p)

def p_empty(p):
    'empty :'
    pass 

def check_var_semantics():
    #check is variable is already defined
    if f_isdef:
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

def check_func_semantics():
    #check is func is already defined
    if f_isdef:
        if g_name in l_symbol_table['func']: raise err_handler.VariableRedeclarationError
    if g_name in g_symbol_table['func']: raise err_handler.VariableRedeclarationError

def reset_func_globals():
    global g_name,f_param_type,g_type,f_isfunc
    g_name = None
    g_type = None
    if f_isdef:
        f_param_type = []
        f_param_name = []
    f_isfunc = False

lexer = lex.lex()
# src = open('test.c')
# lexer.input('int a=10;')
# for i in lexer:
#     pass

parser = yacc.yacc()
parser.parse('''
        int sum(int a, int b){
            int p = 10;
            add(i,j);
            int sub(float x, float y);
            int r = 40;
            while(d){
                int q=10;
            }
        }
    ''')
print(g_symbol_table)
print(l_symbol_table)