;*=====================================================================*/
;*    serrano/prgm/project/bigloo/etc/gram.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 31 17:32:32 1996                          */
;*    Last change :  Wed Jan 31 17:32:33 1996 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An example of C grammar using the Bigloo's lalr-grammar          */
;*    facility.                                                        */
;*=====================================================================*/

(module cparser)

(lalr-grammar
   
  (file ((external-definition) external-definition)
        ((file external-definition)
         `(,file ,external-definition)))

   
  (primary-expr
    ((identifier) identifier)
    ((CONSTANT) CONSTANT)
    ((STRING-LITERAL) STRING-LITERAL)
    ((PAR-OPEN expr PAR-CLO)
     `(,PAR-OPEN ,expr ,PAR-CLO)))
  (postfix-expr
    ((primary-expr) primary-expr)
    ((postfix-expr ANGLE-OPEN expr ANGLE-CLO)
     `(,postfix-expr ,ANGLE-OPEN ,expr ,ANGLE-CLO))
    ((postfix-expr PAR-OPEN PAR-CLO)
     `(,postfix-expr ,PAR-OPEN ,PAR-CLO))
    ((postfix-expr
       PAR-OPEN
       argument-expr-list
       PAR-CLO)
     `(,postfix-expr
       ,PAR-OPEN
       ,argument-expr-list
       ,PAR-CLO))
    ((postfix-expr DOT identifier)
     `(,postfix-expr ,DOT ,identifier))
    ((postfix-expr PTR-OP identifier)
     `(,postfix-expr ,PTR-OP ,identifier))
    ((postfix-expr INC-OP) `(,postfix-expr ,INC-OP))
    ((postfix-expr DEC-OP) `(,postfix-expr ,DEC-OP)))
  (argument-expr-list
    ((assignment-expr) assignment-expr)
    ((argument-expr-list COMMA assignment-expr)
     `(,argument-expr-list ,COMMA ,assignment-expr)))
  (unary-expr
    ((postfix-expr) postfix-expr)
    ((INC-OP unary-expr) `(,INC-OP ,unary-expr))
    ((DEC-OP unary-expr) `(,DEC-OP ,unary-expr))
    ((unary-operator cast-expr)
     `(,unary-operator ,cast-expr))
    ((SIZEOF unary-expr) `(,SIZEOF ,unary-expr))
    ((SIZEOF PAR-OPEN type-name PAR-CLO)
     `(,SIZEOF ,PAR-OPEN ,type-name ,PAR-CLO)))
  (unary-operator
    ((&) &)
    ((*) *)
    ((+) +)
    ((-) -)
    ((~) ~)
    ((!) !))
  (cast-expr
    ((unary-expr) unary-expr)
    ((PAR-OPEN type-name PAR-CLO cast-expr)
     `(,PAR-OPEN ,type-name ,PAR-CLO ,cast-expr)))
  (multiplicative-expr
    ((cast-expr) cast-expr)
    ((multiplicative-expr * cast-expr)
     `(,multiplicative-expr ,* ,cast-expr))
    ((multiplicative-expr / cast-expr)
     `(,multiplicative-expr ,/ ,cast-expr))
    ((multiplicative-expr % cast-expr)
     `(,multiplicative-expr ,% ,cast-expr)))
  (additive-expr
    ((multiplicative-expr) multiplicative-expr)
    ((additive-expr + multiplicative-expr)
     `(,additive-expr ,+ ,multiplicative-expr))
    ((additive-expr - multiplicative-expr)
     `(,additive-expr ,- ,multiplicative-expr)))
  (shift-expr
    ((additive-expr) additive-expr)
    ((shift-expr LEFT-OP additive-expr)
     `(,shift-expr ,LEFT-OP ,additive-expr))
    ((shift-expr RIGHT-OP additive-expr)
     `(,shift-expr ,RIGHT-OP ,additive-expr)))
  (relational-expr
    ((shift-expr) shift-expr)
    ((relational-expr < shift-expr)
     `(,relational-expr ,< ,shift-expr))
    ((relational-expr > shift-expr)
     `(,relational-expr ,> ,shift-expr))
    ((relational-expr LE-OP shift-expr)
     `(,relational-expr ,LE-OP ,shift-expr))
    ((relational-expr GE-OP shift-expr)
     `(,relational-expr ,GE-OP ,shift-expr)))
  (equality-expr
    ((relational-expr) relational-expr)
    ((equality-expr EQ-OP relational-expr)
     `(,equality-expr ,EQ-OP ,relational-expr))
    ((equality-expr NE-OP relational-expr)
     `(,equality-expr ,NE-OP ,relational-expr)))
  (and-expr
    ((equality-expr) equality-expr)
    ((and-expr & equality-expr)
     `(,and-expr ,& ,equality-expr)))
  (exclusive-or-expr
    ((and-expr) and-expr)
    ((exclusive-or-expr ^ and-expr)
     `(,exclusive-or-expr ,^ ,and-expr)))
  (inclusive-or-expr
    ((exclusive-or-expr) exclusive-or-expr)
    ((inclusive-or-expr | exclusive-or-expr)
     `(,inclusive-or-expr ,| ,exclusive-or-expr)))
  (logical-and-expr
    ((inclusive-or-expr) inclusive-or-expr)
    ((logical-and-expr AND-OP inclusive-or-expr)
     `(,logical-and-expr ,AND-OP ,inclusive-or-expr)))
  (logical-or-expr
    ((logical-and-expr) logical-and-expr)
    ((logical-or-expr OR-OP logical-and-expr)
     `(,logical-or-expr ,OR-OP ,logical-and-expr)))
  (conditional-expr
    ((logical-or-expr) logical-or-expr)
    ((logical-or-expr
       ?
       logical-or-expr
       :
       conditional-expr)
     `(,logical-or-expr
       ,?
       ,logical-or-expr
       ,:
       ,conditional-expr)))
  (assignment-expr
    ((conditional-expr) conditional-expr)
    ((unary-expr assignment-operator assignment-expr)
     `(,unary-expr
       ,assignment-operator
       ,assignment-expr)))
  (assignment-operator
    ((=) =)
    ((MUL-ASSIGN) MUL-ASSIGN)
    ((DIV-ASSIGN) DIV-ASSIGN)
    ((MOD-ASSIGN) MOD-ASSIGN)
    ((ADD-ASSIGN) ADD-ASSIGN)
    ((SUB-ASSIGN) SUB-ASSIGN)
    ((LEFT-ASSIGN) LEFT-ASSIGN)
    ((RIGHT-ASSIGN) RIGHT-ASSIGN)
    ((AND-ASSIGN) AND-ASSIGN)
    ((XOR-ASSIGN) XOR-ASSIGN)
    ((OR-ASSIGN) OR-ASSIGN))
  (expr ((assignment-expr) assignment-expr)
        ((expr COMMA assignment-expr)
         `(,expr ,COMMA ,assignment-expr)))
  (constant-expr
    ((conditional-expr) conditional-expr))
  (declaration
    ((declaration-specifiers SEMI-COMMA)
     `(,declaration-specifiers ,SEMI-COMMA))
    ((declaration-specifiers
       init-declarator-list
       SEMI-COMMA)
     `(,declaration-specifiers
       ,init-declarator-list
       ,SEMI-COMMA)))
  (declaration-specifiers
    ((storage-class-specifier)
     storage-class-specifier)
    ((storage-class-specifier declaration-specifiers)
     `(,storage-class-specifier
       ,declaration-specifiers))
    ((type-specifier) type-specifier)
    ((type-specifier declaration-specifiers)
     `(,type-specifier ,declaration-specifiers)))
  (init-declarator-list
    ((init-declarator) init-declarator)
    ((init-declarator-list COMMA init-declarator)
     `(,init-declarator-list ,COMMA ,init-declarator)))
  (init-declarator
    ((declarator) declarator)
    ((declarator = initializer)
     `(,declarator ,= ,initializer)))
  (storage-class-specifier
    ((TYPEDEF) TYPEDEF)
    ((EXTERN) EXTERN)
    ((STATIC) STATIC)
    ((AUTO) AUTO)
    ((REGISTER) REGISTER))
  (type-specifier
    ((CHAR) CHAR)
    ((SHORT) SHORT)
    ((INT) INT)
    ((LONG) LONG)
    ((SIGNED) SIGNED)
    ((UNSIGNED) UNSIGNED)
    ((FLOAT) FLOAT)
    ((DOUBLE) DOUBLE)
    ((CONST) CONST)
    ((VOLATILE) VOLATILE)
    ((VOID) VOID)
    ((struct-or-union-specifier)
     struct-or-union-specifier)
    ((enum-specifier) enum-specifier)
    ((TYPE-NAME-TOK) TYPE-NAME-TOK))
  (struct-or-union-specifier
    ((struct-or-union
       identifier
       BRA-OPEN
       struct-declaration-list
       BRA-CLO)
     `(,struct-or-union
       ,identifier
       ,BRA-OPEN
       ,struct-declaration-list
       ,BRA-CLO))
    ((struct-or-union
       BRA-OPEN
       struct-declaration-list
       BRA-CLO)
     `(,struct-or-union
       ,BRA-OPEN
       ,struct-declaration-list
       ,BRA-CLO))
    ((struct-or-union identifier)
     `(,struct-or-union ,identifier)))
  (struct-or-union
    ((STRUCT) STRUCT)
    ((UNION) UNION))
  (struct-declaration-list
    ((struct-declaration) struct-declaration)
    ((struct-declaration-list struct-declaration)
     `(,struct-declaration-list ,struct-declaration)))
  (struct-declaration
    ((type-specifier-list
       struct-declarator-list
       SEMI-COMMA)
     `(,type-specifier-list
       ,struct-declarator-list
       ,SEMI-COMMA)))
  (struct-declarator-list
    ((struct-declarator) struct-declarator)
    ((struct-declarator-list COMMA struct-declarator)
     `(,struct-declarator-list
       ,COMMA
       ,struct-declarator)))
  (struct-declarator
    ((declarator) declarator)
    ((: constant-expr) `(,: ,constant-expr))
    ((declarator : constant-expr)
     `(,declarator ,: ,constant-expr)))
  (enum-specifier
    ((ENUM BRA-OPEN enumerator-list BRA-CLO)
     `(,ENUM ,BRA-OPEN ,enumerator-list ,BRA-CLO))
    ((ENUM identifier
           BRA-OPEN
           enumerator-list
           BRA-CLO)
     `(,ENUM
       ,identifier
       ,BRA-OPEN
       ,enumerator-list
       ,BRA-CLO))
    ((ENUM identifier) `(,ENUM ,identifier)))
  (enumerator-list
    ((enumerator) enumerator)
    ((enumerator-list COMMA enumerator)
     `(,enumerator-list ,COMMA ,enumerator)))
  (enumerator
    ((identifier) identifier)
    ((identifier = constant-expr)
     `(,identifier ,= ,constant-expr)))
  (declarator
    ((declarator2) declarator2)
    ((pointer declarator2) `(,pointer ,declarator2)))
  (declarator2
    ((identifier) identifier)
    ((PAR-OPEN declarator PAR-CLO)
     `(,PAR-OPEN ,declarator ,PAR-CLO))
    ((declarator2 ANGLE-OPEN ANGLE-CLO)
     `(,declarator2 ,ANGLE-OPEN ,ANGLE-CLO))
    ((declarator2 ANGLE-OPEN constant-expr ANGLE-CLO)
     `(,declarator2
       ,ANGLE-OPEN
       ,constant-expr
       ,ANGLE-CLO))
    ((declarator2 PAR-OPEN PAR-CLO)
     `(,declarator2 ,PAR-OPEN ,PAR-CLO))
    ((declarator2
       PAR-OPEN
       parameter-type-list
       PAR-CLO)
     `(,declarator2
       ,PAR-OPEN
       ,parameter-type-list
       ,PAR-CLO))
    ((declarator2
       PAR-OPEN
       parameter-identifier-list
       PAR-CLO)
     `(,declarator2
       ,PAR-OPEN
       ,parameter-identifier-list
       ,PAR-CLO)))
  (pointer
    ((*) *)
    ((* type-specifier-list)
     `(,* ,type-specifier-list))
    ((* pointer) `(,* ,pointer))
    ((* type-specifier-list pointer)
     `(,* ,type-specifier-list ,pointer)))
  (type-specifier-list
    ((type-specifier) type-specifier)
    ((type-specifier-list type-specifier)
     `(,type-specifier-list ,type-specifier)))
  (parameter-identifier-list
    ((identifier-list) identifier-list)
    ((identifier-list COMMA ELIPSIS)
     `(,identifier-list ,COMMA ,ELIPSIS)))
  (identifier-list
    ((identifier) identifier)
    ((identifier-list COMMA identifier)
     `(,identifier-list ,COMMA ,identifier)))
  (parameter-type-list
    ((parameter-list) parameter-list)
    ((parameter-list COMMA ELIPSIS)
     `(,parameter-list ,COMMA ,ELIPSIS)))
  (parameter-list
    ((parameter-declaration) parameter-declaration)
    ((parameter-list COMMA parameter-declaration)
     `(,parameter-list ,COMMA ,parameter-declaration)))
  (parameter-declaration
    ((type-specifier-list declarator)
     `(,type-specifier-list ,declarator))
    ((type-name) type-name))
  (type-name
    ((type-specifier-list) type-specifier-list)
    ((type-specifier-list abstract-declarator)
     `(,type-specifier-list ,abstract-declarator)))
  (abstract-declarator
    ((pointer) pointer)
    ((abstract-declarator2) abstract-declarator2)
    ((pointer abstract-declarator2)
     `(,pointer ,abstract-declarator2)))
  (abstract-declarator2
    ((PAR-OPEN abstract-declarator PAR-CLO)
     `(,PAR-OPEN ,abstract-declarator ,PAR-CLO))
    ((ANGLE-OPEN ANGLE-CLO)
     `(,ANGLE-OPEN ,ANGLE-CLO))
    ((ANGLE-OPEN constant-expr ANGLE-CLO)
     `(,ANGLE-OPEN ,constant-expr ,ANGLE-CLO))
    ((abstract-declarator2 ANGLE-OPEN ANGLE-CLO)
     `(,abstract-declarator2 ,ANGLE-OPEN ,ANGLE-CLO))
    ((abstract-declarator2
       ANGLE-OPEN
       constant-expr
       ANGLE-CLO)
     `(,abstract-declarator2
       ,ANGLE-OPEN
       ,constant-expr
       ,ANGLE-CLO))
    ((PAR-OPEN PAR-CLO) `(,PAR-OPEN ,PAR-CLO))
    ((PAR-OPEN parameter-type-list PAR-CLO)
     `(,PAR-OPEN ,parameter-type-list ,PAR-CLO))
    ((abstract-declarator2 PAR-OPEN PAR-CLO)
     `(,abstract-declarator2 ,PAR-OPEN ,PAR-CLO))
    ((abstract-declarator2
       PAR-OPEN
       parameter-type-list
       PAR-CLO)
     `(,abstract-declarator2
       ,PAR-OPEN
       ,parameter-type-list
       ,PAR-CLO)))
  (initializer
    ((assignment-expr) assignment-expr)
    ((BRA-OPEN initializer-list BRA-CLO)
     `(,BRA-OPEN ,initializer-list ,BRA-CLO))
    ((BRA-OPEN initializer-list COMMA BRA-CLO)
     `(,BRA-OPEN ,initializer-list ,COMMA ,BRA-CLO)))
  (initializer-list
    ((initializer) initializer)
    ((initializer-list COMMA initializer)
     `(,initializer-list ,COMMA ,initializer)))
  (statement
    ((labeled-statement) labeled-statement)
    ((compound-statement) compound-statement)
    ((expression-statement) expression-statement)
    ((selection-statement) selection-statement)
    ((iteration-statement) iteration-statement)
    ((jump-statement) jump-statement))
  (labeled-statement
    ((identifier : statement)
     `(,identifier ,: ,statement))
    ((CASE constant-expr : statement)
     `(,CASE ,constant-expr ,: ,statement))
    ((DEFAULT : statement) `(,DEFAULT ,: ,statement)))
  (compound-statement
    ((BRA-OPEN BRA-CLO) `(,BRA-OPEN ,BRA-CLO))
    ((BRA-OPEN statement-list BRA-CLO)
     `(,BRA-OPEN ,statement-list ,BRA-CLO))
    ((BRA-OPEN declaration-list BRA-CLO)
     `(,BRA-OPEN ,declaration-list ,BRA-CLO))
    ((BRA-OPEN
       declaration-list
       statement-list
       BRA-CLO)
     `(,BRA-OPEN
       ,declaration-list
       ,statement-list
       ,BRA-CLO)))
  (declaration-list
    ((declaration) declaration)
    ((declaration-list declaration)
     `(,declaration-list ,declaration)))
  (statement-list
    ((statement) statement)
    ((statement-list statement)
     `(,statement-list ,statement)))
  (expression-statement
    ((SEMI-COMMA) SEMI-COMMA)
    ((expr SEMI-COMMA) `(,expr ,SEMI-COMMA)))
  (selection-statement
    ((IF PAR-OPEN expr PAR-CLO statement)
     `(,IF ,PAR-OPEN ,expr ,PAR-CLO ,statement))
    ((IF PAR-OPEN
       expr
       PAR-CLO
       statement
       ELSE
       statement)
     `(,IF
       ,PAR-OPEN
       ,expr
       ,PAR-CLO
       ,statement
       ,ELSE
       ,statement))
    ((SWITCH PAR-OPEN expr PAR-CLO statement)
     `(,SWITCH ,PAR-OPEN ,expr ,PAR-CLO ,statement)))
  (iteration-statement
    ((WHILE PAR-OPEN expr PAR-CLO statement)
     `(,WHILE ,PAR-OPEN ,expr ,PAR-CLO ,statement))
    ((DO statement
         WHILE
       PAR-OPEN
       expr
       PAR-CLO
       SEMI-COMMA)
     `(,DO
       ,statement
       ,WHILE
       ,PAR-OPEN
       ,expr
       ,PAR-CLO
       ,SEMI-COMMA))
    ((FOR PAR-OPEN
          SEMI-COMMA
          SEMI-COMMA
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,SEMI-COMMA
       ,SEMI-COMMA
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          SEMI-COMMA
          SEMI-COMMA
          expr
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,SEMI-COMMA
       ,SEMI-COMMA
       ,expr
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          SEMI-COMMA
          expr
          SEMI-COMMA
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,SEMI-COMMA
       ,expr
       ,SEMI-COMMA
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          SEMI-COMMA
          expr
          SEMI-COMMA
          expr
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,SEMI-COMMA
       ,expr
       ,SEMI-COMMA
       ,expr
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          expr
          SEMI-COMMA
          SEMI-COMMA
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,expr
       ,SEMI-COMMA
       ,SEMI-COMMA
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          expr
          SEMI-COMMA
          SEMI-COMMA
          expr
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,expr
       ,SEMI-COMMA
       ,SEMI-COMMA
       ,expr
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          expr
          SEMI-COMMA
          expr
          SEMI-COMMA
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,expr
       ,SEMI-COMMA
       ,expr
       ,SEMI-COMMA
       ,PAR-CLO
       ,statement))
    ((FOR PAR-OPEN
          expr
          SEMI-COMMA
          expr
          SEMI-COMMA
          expr
          PAR-CLO
          statement)
     `(,FOR
       ,PAR-OPEN
       ,expr
       ,SEMI-COMMA
       ,expr
       ,SEMI-COMMA
       ,expr
       ,PAR-CLO
       ,statement)))
  (jump-statement
    ((GOTO identifier SEMI-COMMA)
     `(,GOTO ,identifier ,SEMI-COMMA))
    ((CONTINUE SEMI-COMMA) `(,CONTINUE ,SEMI-COMMA))
    ((BREAK SEMI-COMMA) `(,BREAK ,SEMI-COMMA))
    ((RETURN SEMI-COMMA) `(,RETURN ,SEMI-COMMA))
    ((RETURN expr SEMI-COMMA)
     `(,RETURN ,expr ,SEMI-COMMA)))

  (external-definition
    ((function-definition) function-definition)
    ((declaration) declaration))
  (function-definition
    ((declarator function-body)
     `(,declarator ,function-body))
    ((declaration-specifiers declarator function-body)
     `(,declaration-specifiers
       ,declarator
       ,function-body)))
  (function-body
    ((compound-statement) compound-statement)
    ((declaration-list compound-statement)
     `(,declaration-list ,compound-statement)))
  (identifier ((IDENTIFIER-TOK) IDENTIFIER-TOK)))
