import ply.yacc as yacc
import ply.lex as lex
import ErrorHandler as err_handler

#globals
symbol_table = {'var':{},'func':{}}
paran_stack = []

vname = ""
vtype = ""
vval = ""

ntype = ""

data_types = {
        'int':'T_INT',
        'float':'T_FLOAT'
    }

tokens = ['ID','SC','OP_EQ','NUM_FLOAT','NUM_INT',
          'lbrace','rbrace','lparan','rparan','eof'] + list(data_types.values())

def t_SC(t):
    r';'
    return t

def t_lbracket(t):
    r'[\{\(]'
    if t.value is '{': t.type = 'lbrace'
    else: t.type = 'lparan'
    
    paran_stack.append(t)
    return t

def t_rbracket(t):
    r'[\}\)]'
    if t.value is '}': t.type = 'rbrace'
    else: t.type = 'rparan'

    if len(paran_stack)!=0: top = paran_stack[len(paran_stack)-1]
    else: raise err_handler.ParanMismatchError()
    
    if t.value is '}':
        if top.value is '{':
            paran_stack.pop()
        else:
            #print('Paranthesis mismatch at:',str((top.lineno,find_column(f.read(),top))))
            raise err_handler.ParanMismatchError()
    elif t.value is ')':
        if top.value is '(':
            paran_stack.pop()
        else:
            #print('Paranthesis mismatch at:',str((top.lineno,find_column(f.read(),top))))
            raise err_handler.ParanMismatchError()
    return t
    
def t_OP_EQ(t):
    r'='
    print(t)
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
    print(t)
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = data_types.get(t.value,'ID')
    print(t)
    return t

#tried to detect eof, but failed
def t_eof(t):
    r'<<EOF>>'
    print('eof')

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

t_ignore = ' \t'

def t_error(t):
    print('Illegal character',t.value[0],'at line:'+str(t.lineno)\
          +' pos:'+str(find_column(f.read(),t)))
    t.lexer.skip(1)

def p_var_dec(p):
    '''
        var_dec : DT ID set_vname SC make_var_entry
                | DT ID set_vname OP_EQ NUM SC make_var_entry
        DT : T_INT set_vtype
            | T_FLOAT set_vtype
        NUM : NUM_INT set_vval
            | NUM_FLOAT set_vval
    '''
    print('-----')
    for i in p: print(i)
    print('-----')

def p_set_vtype(p):
    'set_vtype :'
    global vtype
    print('set_vtype :',p[-1])
    vtype = p[-1]

def p_set_vname(p):
    'set_vname :'
    global vname
    print('set_vname :',p[-1])
    vname = p[-1]

def p_set_vval(p):
    'set_vval :'
    global vval
    print('set_vval :',p[-1])
    vval = p[-1]

def p_make_var_entry(p):
    'make_var_entry :'
    check_var_semantics()
    symbol_table['var'][vname] = {'type':vtype,'value':vval}
    reset_var_globals()
    print(symbol_table)

def p_error(p):
    print('syntax error')

def check_var_semantics():
    #check is variable is already defined
    if vname in symbol_table['var']: raise err_handler.VariableRedeclarationError
    #check if declared var_type matches num_type
    if ntype is not data_types[vtype]: raise err_handler.VariableTypeError

def reset_var_globals():
    global vval,vname,vtype
    vval = 0
    vname = ""
    vtype = ""
    ntype = ""

lexer = lex.lex()
##f=open('test.c')
##lexer.input(f.read())
##for i in lexer:
##    pass
##if len(paran_stack)!=0:
##    top = paran_stack[len(paran_stack)-1]
##    raise err_handler.ParanMismatchError()
##    #print('Paranthesis mismatch at:',str((top.lineno,find_column(f.read(),top))))
parser = yacc.yacc()
##res = parser.parse('int a = 19.4;')
res = parser.parse('int a = 19;')
