import ply.lex as lex
import ply.yacc as yacc
tokens = (
   'NUMBER',
   'STRING',
   'PLUS',
   'MINUS',
   'MUL',
   'DIVIDE',
   'LPAREN',
   'RPAREN',
   'WHILE',
   'COLON',
   'IF',
   'ELSE',
   'FOR',
   'LBRACE',
   'RBRACE',
   'ID',
   'EQUALS',
   'DEQUALS',
   'LESSTHAN',
   'GREATERTHAN',
   'LESSTHANEQ',
   'GREATERTHANEQ',
   'COMMA',
   'INDENT',
   'DEDENT',
   'IN',
   'FLOAT',
   'RANGE',
   'SQBR',
   'SQBL',
   'NEQUALS'
)

t_COLON   = r':'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MUL     = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_EQUALS = r'='
t_DEQUALS = r'=='
t_LESSTHAN = r'<'
t_GREATERTHAN = r'>'
t_LESSTHANEQ = r'<='
t_GREATERTHANEQ = r'>='
t_NEQUALS = r'!='
t_COMMA = r','
t_SQBR = r'\['
t_SQBL = r'\]'

reserved = {
   'while' : 'WHILE',
   'if' : 'IF',
   'else' : 'ELSE',
   'for' : 'FOR',
   'in' : 'IN'
}

indentation_stack = [0]

def t_INDENT(t):
    r'\n[ \t]*'
    t.lexer.lineno += 1
    new_indentation = len(t.value) - 1
    if new_indentation > indentation_stack[-1]:
        t.type = 'INDENT'
        indentation_stack.append(new_indentation)
        return t
    elif new_indentation < indentation_stack[-1]:
        indentation_stack.pop()
        t.type = 'DEDENT'
        return t
def t_RANGE(t):
    r'range'
    return t
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t
def t_STRING(t):   #String declaration
    r'\".*?\"'
    t.type = 'STRING'
    return t
def t_ID(t):   #Variable/Identifier declaration
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_ignore = ' \t'
t_ignore_COMMENT = r'\#.*'
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MUL', 'DIVIDE'),
)

def p_program(p):
    '''program : statements
    '''
    p[0] = p[1]
def p_statements(p):
    '''statements : statement
                  | statement statements
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]
def p_statement(p):
    '''statement : IF LPAREN expression RPAREN COLON INDENT program DEDENT ELSE COLON INDENT program DEDENT
                 | IF LPAREN expression RPAREN COLON  INDENT program DEDENT
                 | FOR ID IN ID COLON INDENT program DEDENT
                 | WHILE LPAREN expression RPAREN COLON INDENT program DEDENT
                 | WHILE expression COLON INDENT program DEDENT
                 | expression
                 | ID EQUALS NUMBER
                 | ID EQUALS STRING
                 | ID EQUALS FLOAT
                 | tuple_declaration
                 | list_declaration
                 | dictionary_declaration
                 | STRING
                 | FOR ID IN STRING COLON INDENT program DEDENT
                 | FOR ID IN RANGE LPAREN expression RPAREN COLON INDENT program DEDENT
                 | ID EQUALS LBRACE STUFF RBRACE
    STUFF : NUMBER COLON NUMBER
          | NUMBER COLON NUMBER COMMA STUFF
          | NUMBER COLON FLOAT
          | NUMBER COLON FLOAT COMMA STUFF
          | NUMBER COLON STRING
          | NUMBER COLON STRING COMMA STUFF
          | FLOAT COLON NUMBER
          | FLOAT COLON NUMBER COMMA STUFF
          | FLOAT COLON FLOAT
          | FLOAT COLON FLOAT COMMA STUFF
          | FLOAT COLON STRING
          | FLOAT COLON STRING COMMA STUFF
          | STRING COLON NUMBER
          | STRING COLON NUMBER COMMA STUFF
          | STRING COLON FLOAT
          | STRING COLON FLOAT COMMA STUFF
          | STRING COLON STRING
          | STRING COLON STRING COMMA STUFF


'''
    if len(p) == 2 and p[1] == 'expression':
        p[0] = ('expression', p[1])
    elif len(p) == 9:
        p[0] = ('for', p[3], p[7])
    elif len(p) == 14:
        p[0] = ('if-else', p[3], p[7], p[12])
    elif len(p) == 8:
        p[0] = ('for', p[2], p[4], p[7])
    elif p[1]=="WHILE" and len(p) == 6:
        p[0] = ('while', p[2], p[5])
    elif p[1]=="WHILE" and len(p) ==8 :
        p[0] = ('while', p[3], p[7])
    elif len(p) == 2:
        p[0] = (p[1])
    elif p[1] == "ID":
        p[0] = ("declaration",p[1], p[3])
def p_tuple_declaration(p):
    '''tuple_declaration : ID EQUALS LPAREN element RPAREN
    '''
    p[0] = (p[1],p[4])

def p_list_declaration(p):
    '''list_declaration : ID EQUALS SQBR element SQBL
    '''
    p[0] = (p[1], p[4])


def p_element(p):
    '''element : STRING COMMA element
                 | STRING
                 | NUMBER COMMA element
                 | NUMBER
                 | FLOAT COMMA element
                 | FLOAT'''
    if len(p) >2:
        p[0] = (p[1],p[3])
    else:
        p[0] = (p[1])
def p_expression(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression MUL expression
                  | expression DIVIDE expression
                  | LPAREN expression RPAREN
                  | expression LESSTHAN expression
                  | expression GREATERTHAN expression
                  | expression LESSTHANEQ expression
                  | expression EQUALS expression
                  | expression GREATERTHANEQ expression
                  | expression DEQUALS expression
                  | expression NEQUALS expression
                  | expression EQUALS expression PLUS expression
                  | ID
                  | NUMBER
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_dictionary_declaration(p):
    '''dictionary_declaration : ID EQUALS LBRACE key_value_pairs RBRACE'''
    p[0] = ('dictionary', p[1], p[4])

def p_key_value_pairs(p):
    '''key_value_pairs : key_value_pairs COMMA key_value_pair
                      | key_value_pair
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_key_value_pair(p):
    '''key_value_pair : STRING COLON expression
                     | ID COLON expression
    '''
    p[0] = (p[1], p[3])

def p_error(p):
    print("Syntax incorrect, Rejected!")
    print(f"Syntax error at '{p.value}'")
    raise SyntaxError

parser = yacc.yacc()
print("Enter the input code and type 'exit' to stop")
input_code = ''
while True:
    line = input()
    if line.lower() == 'exit':
        break
    input_code += line + '\n'

lexer.input(input_code)

while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)
print("\n")

result = parser.parse(input_code)
print("\n")

if (result):
    print("Syntax validated, Accepted!")


