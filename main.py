import ply.yacc as yacc
import ply.lex as lex
import ErrorHandler as err_handler

#globals
symbol_table = {'var':{},'func':{}}
paran_stack = []

g_name = ""
g_type = ""
vval = ""
ntype = ""
fparam = []
isfunc = 0

data_types = {
        'int':'T_INT',
        'float':'T_FLOAT'
    }

tokens = ['ID','SC','OP_EQ','NUM_FLOAT','NUM_INT','COMMA',
          'LBRACE','RBRACE','LPARAN','RPARAN','eof'] + list(data_types.values())

def t_SC(t):
    r';'
    return t

def t_COMMA(t):
    r','
    return t

def t_LPARAN(t):
    r'[\(]'
    t.type = 'LPARAN'
    paran_stack.append(t)
    return t

def t_LBRACE(t):
    r'[\{]'
    t.type = 'LBRACE'
    paran_stack.append(t)
    return t

def t_RPARAN(t):
    r'[\)]'
    t.type = 'RPARAN'

    if len(paran_stack)!=0: top = paran_stack[len(paran_stack)-1]
    else: raise err_handler.ParanMismatchError()
    
    if top.value is '(':
        paran_stack.pop()
    else:
        #print('Paranthesis mismatch at:',str((top.lineno,find_column(src.read(),top))))
        raise err_handler.ParanMismatchError()
    return t

def t_RBRACE(t):
    r'[\}]'
    t.type = 'RBRACE'

    if len(paran_stack)!=0: top = paran_stack[len(paran_stack)-1]
    else: raise err_handler.ParanMismatchError()
    
    if top.value is '{':
        paran_stack.pop()
    else:
        #print('Paranthesis mismatch at:',str((top.lineno,find_column(src.read(),top))))
        raise err_handler.ParanMismatchError()
    return t
    
def t_OP_EQ(t):
    r'='
    return t

def t_NUM(t):
    r'[0-9]+(\.[0-9]+)?'
    global ntype
    if '.' in t.value:
        ntype = 'T_FLOAT'
        t.type = 'NUM_FLOAT'
        t.value = float(t.value)
    else:
        ntype = 'T_INT'
        t.type = 'NUM_INT'
        t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = data_types.get(t.value,'ID')
    return t

#tried to detect eof, but failed
def t_eof(t):
    r'<<EOF>>'
    print('eof')

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
##    print(len(t.value))
##    t.lexer.skip(len(t.value))

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

def p_code(p):
    '''
        S : DECL DECL
            | empty
    '''

def p_dec(p):
    '''
        DECL : DT ID set_name Y 
        Y : F SC make_func_entry
            | V SC make_var_entry
        V : OP_EQ NUM
            | empty
        F : LPARAN set_isfunc P RPARAN
        P : A COMMA P
            | A
        A : DT ID
            | DT
    '''

def p_set_isfunc(p):
    'set_isfunc :'
    global isfunc
    isfunc = 1

def p_num(p):
    '''
        NUM : NUM_INT set_vval
            | NUM_FLOAT set_vval
    '''

def p_dt(p):
    '''
        DT : T_INT set_type
            | T_FLOAT set_type
    '''

def p_set_type(p):
    'set_type :'
    global g_type,isfunc,fparam
    if isfunc == 0:
        g_type = p[-1]
    else:
        fparam.append(p[-1])

def p_set_name(p):
    'set_name :'
    global g_name
    g_name = p[-1]

def p_set_vval(p):
    'set_vval :'
    global vval
    vval = p[-1]

def p_make_var_entry(p):
    'make_var_entry :'
    check_var_semantics()
    symbol_table['var'][g_name] = {'type':g_type,'value':vval}
    reset_var_globals()
    print(symbol_table)

def p_error(p):
    print('syntax error',p)

def check_var_semantics():
    #check is variable is already defined
    if g_name in symbol_table['var']: raise err_handler.VariableRedeclarationError
    #check if declared var_type matches num_type
    if ntype is not data_types[g_type]: raise err_handler.VariableTypeError

def reset_var_globals():
    global vval,g_name,g_type
    vval = 0
    g_name = ""
    g_type = ""
    ntype = ""

def p_make_func_entry(p):
    'make_func_entry :'
    if g_name in symbol_table['func']: raise err_handler.FunctionRedeclarationError
    else:
        symbol_table['func'][g_name] = {'return_type' : g_type, 'param' : fparam}
        print(symbol_table)
        reset_func_globals()

def p_empty(p):
    'empty :'
    pass    

def reset_func_globals():
    global g_name,fparam,g_type,isfunc
    g_name = ""
    g_type = ""
    fparam = []
    isfunc = 0

lexer = lex.lex()
src = open('test.c')
##lexer.input(src.read())
##for i in lexer:
##    pass
##if len(paran_stack)!=0:
##    top = paran_stack[len(paran_stack)-1]
##    raise err_handler.ParanMismatchError()
##    #print('Paranthesis mismatch at:',str((top.lineno,find_column(f.read(),top))))
parser = yacc.yacc()
parser.parse('int a=10;\n int sum(int,int);')
