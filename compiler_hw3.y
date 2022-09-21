/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_hw_common.h" //Extern variables that communicate with lex
    #define MAX_SYMBOL_NUM 1000

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    int yyscope = 0;
    int yyaddr = 0;
    int line_count = 0;
    FILE * fp;
    int cmp_scope = 0;
    int labelstack[256];
    int labeltop;
    int globallabel;
    int casestack[256];
    int caseend;
    int cased;
    int error_flag;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", line_count+1, s);
    }

    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    struct symbol{
        char *name;
        char *type;
        int addr;
        int line;
        char *sign;
        struct symbol *next;
    };
    typedef struct symbol symbol;
    symbol *symboltable[MAX_SYMBOL_NUM];

    static void create_symbol();
    static void insert_symbol(char *, int, int, char *, char *);
    static symbol * lookup_symbol(char *);
    static void dump_symbol();
%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 *  - you can add new fields if needed.
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token VAR NEWLINE
%token INT FLOAT BOOL STRING
%token INC DEC
%token GEQ LEQ EQL NEQ '>' '<'
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token IF ELSE FOR 
%token SWITCH CASE DEFAULT
%token PRINT PRINTLN
%token PACKAGE FUNC RETURN
%token TRUE FALSE

%token '+' '-' 
%token '*' '/' '%'
%token LOR LAND '!'

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT 
%token <s_val> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type  /* use in type declaration */
%type <s_val> FuncOpen /* function name */
%type <s_val> ReturnType /* function return type */
%type <s_val> ParameterList
%type <s_val> Expression Term1 Term2 Term3 Term4 ADDR    /* return type to check operation */
%type <s_val> UnaryExpr PrimaryExpr Operand Literal ConversionExpr  /* return type to check operation */
%type <s_val> unary_op com_op add_op mul_op assign_op   /* return type to check operation */

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%
/* Function */
Program
    : GlobalStatementList
;

GlobalStatementList 
    : GlobalStatementList GlobalStatement
    | GlobalStatement
;

GlobalStatement
    : PackageStmt NEWLINE { line_count = yylineno; }
    | FunctionDeclStmt  { line_count = yylineno+1; }
    | NEWLINE   { line_count = yylineno; }
;

PackageStmt
    : PACKAGE IDENT { }
;

FunctionDeclStmt
    : FuncInfo FuncBlock  { fprintf(fp, "\treturn\n");
                            fprintf(fp, ".end method\n\n");
                            yyaddr = 0; }
;

FuncInfo
    : FuncOpen '(' ParameterList ')' ReturnType { char *temp;
                                                    asprintf(&temp, "(%s)%s", $3, $5);
                                                    insert_symbol($1, -1, yyscope-1, "func", temp);
                                                    fprintf(fp, ".method public static %s%s\n", $1, temp);
                                                    fprintf(fp, ".limit stack 20\n");
                                                    fprintf(fp, ".limit locals 20\n");
                                                    yyaddr = 0; }
    | FuncOpen '(' ')' ReturnType   { char *temp;
                                        asprintf(&temp, "()%s", $4);
                                        insert_symbol($1, -1, yyscope-1, "func", temp);
                                        if ( strcmp($1, "main")== 0 )
                                        {
                                            fprintf(fp, ".method public static main([Ljava/lang/String;)%s\n", $4);
                                            fprintf(fp, ".limit stack 100\n");
                                            fprintf(fp, ".limit locals 100\n");
                                        }
                                        else
                                        {
                                            fprintf(fp, ".method public static %s()%s\n", $1, $4);
                                            fprintf(fp, ".limit stack 20\n");
                                            fprintf(fp, ".limit locals 20\n");
                                        }
                                        yyaddr = 0; }
;

FuncOpen
    : FUNC IDENT    { yyscope++;
                        create_symbol();
                        $$ = $2; }
;

ParameterList
    : IDENT Type { char *type;
                    switch($2[0])
                    {
                        case 'i': type = "I"; break;
                        case 'f': type = "F"; break;
                        case 'b': type = "B"; break;
                        case 's': type = "S"; break;
                        default: type = "V"; break;
                    }
                    insert_symbol($1, yyaddr, yyscope, $2, "-"); 
                    yyaddr++;
                    $$ = type; }
    | ParameterList ',' IDENT Type  { char *type;
                                        switch($4[0])
                                        {
                                            case 'i': type = "I"; break;
                                            case 'f': type = "F"; break;
                                            case 'b': type = "B"; break;
                                            case 's': type = "S"; break;
                                            default: type = "V"; break;
                                        }
                                        insert_symbol($3, yyaddr, yyscope, $4, "-"); 
                                        yyaddr++;
                                        char *temp;
                                        asprintf(&temp, "%s%s", $1, type);
                                        $$ = temp; }
;

ReturnType
    : INT   { $$ = "I"; }
    | FLOAT { $$ = "F"; }
    | BOOL  { $$ = "B"; }
    | STRING    { $$ = "S"; }
    | /* void */    { $$ = "V"; }
;

FuncBlock
    : '{' StatementList RBRACE {  }
;

StatementList
    : StatementList Statement
    |
;

Statement
    : DeclartionStmt NEWLINE    { line_count = yylineno;}
    | SimpleStmt NEWLINE    { line_count = yylineno; }
    | Block { line_count = yylineno+1; }
    | IfStmt    { line_count = yylineno+1; }
    | ForStmt   { line_count = yylineno+1; }
    | SwitchStmt    { line_count = yylineno+1; }
    | CaseStmt  { line_count = yylineno+1; }
    | PrintStmt NEWLINE { line_count = yylineno; }
    | ReturnStmt NEWLINE    { line_count = yylineno; }
    | NEWLINE   { line_count = yylineno; }
;

ReturnStmt
    : RETURN    { fprintf(fp, "\treturn\n"); }
    | RETURN Expression { fprintf(fp, "\t%creturn\n", $2[0]); }
;

SimpleStmt 
    : AssignStmt 
    | ExprStmt
    | IncDecStmt
;

DeclartionStmt
    : VAR IDENT Type '=' Expression { insert_symbol($2, yyaddr, yyscope, $3, "-"); 
                                        if(strcmp($3, "b")==0)
                                            fprintf(fp, "\tistore %d\n", yyaddr);
                                        else
                                            fprintf(fp, "\t%sstore %d\n", $3, yyaddr);
                                        yyaddr++;}
    | VAR IDENT Type    { insert_symbol($2, yyaddr, yyscope, $3, "-"); 
                            if(strcmp($3, "b")==0)
                            {
                                fprintf(fp, "\ticonst_0\n");
                                fprintf(fp, "\tistore %d\n", yyaddr);
                            }
                            else
                            {
                                if(strcmp($3, "f")==0)
                                    fprintf(fp, "\tldc 0.0\n");
                                else if(strcmp($3, "a")==0)
                                    fprintf(fp, "\tldc \"\"\n");
                                else if(strcmp($3, "i") == 0)
                                    fprintf(fp, "\tldc 0\n");
                                fprintf(fp, "\t%sstore %d\n", $3, yyaddr);
                            }
                            yyaddr++;}
;

AssignStmt
    : ADDR assign_op Expression  { symbol * temp = lookup_symbol($1);
                                    if(temp!=NULL)
                                    {
                                        if(strcmp(temp->type, $3) != 0)
                                        {
                                            char *str;
                                            asprintf(&str, "invalid operation: ASSIGN (mismatched types %s and %s)", $1, $3);
                                            yyerror(str);
                                            error_flag = 1;
                                        }
                                        fprintf(fp, "\t%s%s\n", temp->type, $2);
                                        if(strcmp(temp->type, "b")==0)
                                            fprintf(fp, "\tistore %d\n", temp->addr);
                                        else
                                            fprintf(fp, "\t%sstore %d\n", temp->type, temp->addr);
                                    } }
    | IDENT '=' Expression  { symbol * temp = lookup_symbol($1);
                                if(temp!=NULL)
                                {
                                    if(strcmp(temp->type, $3) != 0)
                                    {
                                        char *str;
                                        asprintf(&str, "invalid operation: ASSIGN (mismatched types %s and %s)", $1, $3);
                                        yyerror(str);
                                        error_flag = 1;
                                    }
                                    if(strcmp(temp->type, "b")==0)
                                        fprintf(fp, "\tistore %d\n", temp->addr); 
                                    else
                                        fprintf(fp, "\t%sstore %d\n", temp->type, temp->addr);
                                }
                                else
                                {
                                    char *str;
                                    asprintf(&str, "invalid operation: ASSIGN (mismatched types ERROR and %s)", $1, $3);
                                    yyerror(str);
                                } }
    | Expression '=' Expression { char *str;
                                    asprintf(&str, "invalid operation: ASSIGN (left-hand side operand must be addressable)", $1, $3);
                                    yyerror(str);
                                    error_flag = 1;}
    | Expression assign_op Expression   { char *str;
                                            asprintf(&str, "invalid operation: ASSIGN (left-hand side operand must be addressable)", $1, $3);
                                            yyerror(str);
                                            error_flag = 1; }
;

ADDR
    : IDENT { symbol * temp = lookup_symbol($1);
                fprintf(fp, "\t%sload %d\n", temp->type, temp->addr);
                $$ = $1; }
    | '(' ADDR ')'  { $$ = $2; }
;

assign_op
    : ADD_ASSIGN    { $$ = "add"; }
    | SUB_ASSIGN    { $$ = "sub"; }
    | MUL_ASSIGN    { $$ = "mul"; }
    | QUO_ASSIGN    { $$ = "div"; }
    | REM_ASSIGN    { $$ = "rem"; }
;

ExprStmt
    : Expression
;

IncDecStmt
    : ADDR INC  { symbol * temp = lookup_symbol($1);
                    if(temp!=NULL)
                    {
                        if(strcmp(temp->type, "f")==0)
                            fprintf(fp, "\tldc 1.0\n");
                        else
                            fprintf(fp, "\tldc 1\n");
                        fprintf(fp, "\t%sadd\n", temp->type);
                        fprintf(fp, "\t%sstore %d\n", temp->type, temp->addr);
                    } }
    | ADDR DEC  { symbol * temp = lookup_symbol($1);
                    if(temp!=NULL)
                    {
                        if(strcmp(temp->type, "f")==0)
                            fprintf(fp, "\tldc 1.0\n");
                        else
                            fprintf(fp, "\tldc 1\n");
                        fprintf(fp, "\t%ssub\n", temp->type);
                        fprintf(fp, "\t%sstore %d\n", temp->type, temp->addr);
                      } }
    | Expression INC    { 
                            if(strcmp($1, "f")==0)
                                fprintf(fp, "\tldc 1.0\n");
                            else
                                fprintf(fp, "\tldc 1\n");
                            fprintf(fp, "\t%sadd\n", $1); }
    | Expression DEC    { 
                            if(strcmp($1, "f")==0)
                                fprintf(fp, "\tldc 1.0\n");
                            else
                                fprintf(fp, "\tldc 1\n");
                            fprintf(fp, "\t%ssub\n", $1); }
;

IfStmt
    : IF Condition IfBlock  { fprintf(fp, "IF_false%d:\n", labelstack[labeltop]);
                                fprintf(fp, "EXIT%d:\n", labelstack[labeltop]);
                                labeltop-=1; }
    | IF Condition IfBlock ELSE { fprintf(fp, "IF_false%d:\n", labelstack[labeltop]); }
    IfStmt                      { fprintf(fp, "EXIT%d:\n", labelstack[labeltop]);
                                    labeltop-=1; }
    | IF Condition IfBlock ELSE { fprintf(fp, "IF_false%d:\n", labelstack[labeltop]); }
    Block                       { fprintf(fp, "EXIT%d:\n", labelstack[labeltop]);
                                    labeltop-=1; }
;

Condition
    : Expression    {   if(strcmp($1, "b") != 0)
                        {
                            char *str;
                            asprintf(&str, "non-bool (type %s) used as for condition", $1);
                            yyerror(str);
                            error_flag = 1;
                        } }
;

ForStmt
    : FOR   { labeltop++;
                labelstack[labeltop] = globallabel;
                globallabel++; 
                fprintf(fp, "FOR%d:\n", labelstack[labeltop]); }
    ForCondition    { fprintf(fp, "FOR_exit%d:\n", labelstack[labeltop]);
                        labeltop-=1; }
;

ForCondition
    : Condition ForBlock
    | ForClause ForBlock
;

ForClause
    : SimpleStmt ';' Condition ';' SimpleStmt
;

SwitchStmt
    : SWITCH Expression SwitchBlock
;

CaseStmt
    : CASECondition Block   { fprintf(fp, "\tgoto Switch_end%d\n", labelstack[labeltop]); }
;

CASECondition
    : CASE INT_LIT ':'  { fprintf(fp, "CASE%d_%d:\n", labelstack[labeltop], $2);
                            casestack[caseend] = $2;
                            caseend++; }
    | DEFAULT ':'   { fprintf(fp, "CASE%d_d:\n", labelstack[labeltop]);
                        cased = 1; }
;

IfBlock
    : LBRACE { labeltop++; 
                labelstack[labeltop] = globallabel;
                globallabel++;
                fprintf(fp, "\tifeq IF_false%d\n", labelstack[labeltop]); } 
    StatementList 
    RBRACE { fprintf(fp, "\tgoto EXIT%d\n", labelstack[labeltop]); }
;

ForBlock
    : LBRACE { fprintf(fp, "\tifeq FOR_exit%d\n", labelstack[labeltop]); }
    StatementList RBRACE { fprintf(fp, "\tgoto FOR%d\n", labelstack[labeltop]); }
;

SwitchBlock
    : LBRACE { labeltop++;
                labelstack[labeltop] = globallabel;
                globallabel++;
                fprintf(fp, "\tgoto Switch%d\n", labelstack[labeltop]); }
    StatementList   { fprintf(fp, "Switch%d:\n", labelstack[labeltop]);
                        fprintf(fp, "lookupswitch\n");
                        for(int i=0; i<caseend; i++)
                        {
                            fprintf(fp, "\t%d: CASE%d_%d\n", casestack[i], labelstack[labeltop], casestack[i]);
                        }
                        if(cased)
                            fprintf(fp, "\tdefault: CASE%d_d\n", labelstack[labeltop]);
                        caseend = 0;
                        cased = 0; }
    RBRACE    { fprintf(fp, "Switch_end%d:\n", labelstack[labeltop]);
                labeltop-=1; }
;

Block
    : LBRACE StatementList RBRACE {}
;


PrintStmt
    : PRINT '(' Expression ')'  { char * type;
                                    if(strcmp($3, "a")==0)
                                        type = "Ljava/lang/String;";
                                    else if(strcmp($3, "b")==0)
                                    {
                                        type = "Ljava/lang/String;";
                                        fprintf(fp, "\tifne L_cmp_%d\n", cmp_scope);
                                        fprintf(fp, "\tldc \"false\"\n");
                                        fprintf(fp, "\tgoto L_cmp_%d\n", cmp_scope+1);
                                        fprintf(fp, "L_cmp_%d:\n", cmp_scope);
                                        fprintf(fp, "\tldc \"true\"\n");
                                        fprintf(fp, "L_cmp_%d:\n", cmp_scope+1);
                                        cmp_scope+=2;
                                    }
                                    else if(strcmp($3, "i")==0)
                                        type = "I";
                                    else type = "F";
                                    fprintf(fp, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                    fprintf(fp, "\tswap\n");
                                    fprintf(fp, "\tinvokevirtual java/io/PrintStream/print(%s)V\n", type); }
    | PRINTLN '(' Expression ')'    { char * type;
                                        if(strcmp($3, "a")==0)
                                            type = "Ljava/lang/String;";
                                        else if(strcmp($3, "b")==0)
                                        {
                                            type = "Ljava/lang/String;";
                                            fprintf(fp, "\tifne L_cmp_%d\n", cmp_scope);
                                            fprintf(fp, "\tldc \"false\"\n");
                                            fprintf(fp, "\tgoto L_cmp_%d\n", cmp_scope+1);
                                            fprintf(fp, "L_cmp_%d:\n", cmp_scope);
                                            fprintf(fp, "\tldc \"true\"\n");
                                            fprintf(fp, "L_cmp_%d:\n", cmp_scope+1);
                                            cmp_scope+=2;
                                        }
                                        else if(strcmp($3, "i")==0)
                                            type = "I";
                                        else type = "F";
                                        fprintf(fp, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(fp, "\tswap\n");
                                        fprintf(fp, "\tinvokevirtual java/io/PrintStream/println(%s)V\n", type); }
;

/* 5-2 Expression */
Expression
    : Expression LOR Term1  { if( strcmp($1, "b") != 0 )
                                {
                                    char *str;
                                    asprintf(&str, "invalid operation: (operator LOR not defined on %s)", $1);
                                    yyerror(str);
                                    error_flag = 1;
                                }
                                else if( strcmp($3, "b") != 0 )
                                {
                                    char *str;
                                    asprintf(&str, "invalid operation: (operator LOR not defined on %s)", $3);
                                    yyerror(str);
                                    error_flag = 1;
                                }
                                $$ = "b";
                                fprintf(fp, "\tior\n"); }
    | Term1 {$$ = $1;}
;

Term1
    : Term1 LAND Term2  { if( strcmp($1, "b") != 0 )
                            {
                                char *str;
                                asprintf(&str, "invalid operation: (operator LAND not defined on %s)", $1);
                                yyerror(str);
                                error_flag = 1;
                            }
                            else if( strcmp($3, "b") != 0 )
                            {
                                char *str;
                                asprintf(&str, "invalid operation: (operator LAND not defined on %s)", $3);
                                yyerror(str);
                                error_flag = 1;
                            }
                            $$ = "b";
                            fprintf(fp, "\tiand\n"); }
    | Term2 { $$ = $1; }
;

Term2
    : Term2 com_op Term3    { if(strcmp($1, $3) != 0)
                                {
                                    char *str;
                                    asprintf(&str, "invalid operation: %s (mismatched types %s and %s)", $2, $1, $3);
                                    yyerror(str);
                                    error_flag = 1;
                                }
                                $$ = "b";
                                char * cmp;
                                if(strcmp($1, "i") == 0)
                                    cmp = "isub";
                                else cmp = "fcmpl";
                                fprintf(fp, "\t%s\n", cmp);
                                fprintf(fp, "\t%s L_cmp_%d\n", $2, cmp_scope);
                                fprintf(fp, "\ticonst_0\n");
                                fprintf(fp, "\tgoto L_cmp_%d\n", cmp_scope+1);
                                fprintf(fp, "L_cmp_%d:\n", cmp_scope);
                                fprintf(fp, "\ticonst_1\n");
                                fprintf(fp, "L_cmp_%d:\n", cmp_scope+1);
                                cmp_scope+=2; }
    | Term3 { $$ = $1; }
;

Term3
    : Term3 add_op Term4 { if( strcmp($1, $3) )
                            {
                                char *str;
                                asprintf(&str, "invalid operation: %s (mismatched types %s and %s)", $2, $1, $3);
                                yyerror(str);
                                error_flag = 1;
                                $$ = "ERROR";
                            }
                            else $$ = $1;
                            fprintf(fp, "\t%s%s\n", $1, $2); }
    | Term4 { $$ = $1; }
;

Term4
    : Term4 mul_op UnaryExpr    { if(strcmp($2, "REM") == 0)
                                    {
                                        if(strcmp($1, "i") != 0)
                                        {
                                            char *str;
                                            asprintf(&str, "invalid operation: (operator REM not defined on %s)", $1);
                                            yyerror(str);
                                            error_flag = 1;
                                            $$ = "ERROR";
                                        }
                                        else if(strcmp($3, "i") != 0)
                                        {
                                            char *str;
                                            asprintf(&str, "invalid operation: (operator REM not defined on %s)", $3);
                                            yyerror(str);
                                            error_flag = 1;
                                            $$ = "ERROR";
                                        }
                                    }
                                    else if( strcmp($1, $3) )
                                    {
                                        char *str;
                                        asprintf(&str, "invalid operation: %s (mismatched type %s and %s)", $2, $1, $3);
                                        yyerror(str);
                                        error_flag = 1;
                                        $$ = "ERROR";
                                    }
                                    else $$ = $1;
                                    fprintf(fp, "\t%s%s\n", $1, $2); }
    | UnaryExpr { $$ = $1;}
;

UnaryExpr
    : PrimaryExpr   { $$ = $1;}
    | unary_op UnaryExpr    { if(strcmp("NEG", $1) == 0)
                                {    
                                    fprintf(fp, "\t%sneg\n", $2);
                                } 
                                else if(strcmp("NOT", $1) == 0)
                                {
                                    if(strcmp("b", $2) == 0)
                                    {
                                        fprintf(fp, "\ticonst_1\n");
                                        fprintf(fp, "\tixor\n");
                                    }
                                }
                                $$ = $2; }
;

PrimaryExpr
    : Operand   { $$ = $1; }
    | ConversionExpr    { $$ = $1; }
;

Operand
    : Literal   { $$ = $1; }
    | IDENT { symbol * target = lookup_symbol($1);
                if(target==NULL)
                {
                    $$ = "ERROR";
                }
                else
                {
                    $$ = target->type;
                    if(strcmp(target->type, "b")==0)
                        fprintf(fp, "\tiload %d\n", target->addr);
                    else
                        fprintf(fp, "\t%sload %d\n", target-> type, target->addr);
                } }
    | IDENT '(' ')' { symbol * target = lookup_symbol($1);
                        fprintf(fp, "\tinvokestatic Main/%s%s\n", $1, target->sign);
                        switch((target->sign)[strlen(target->sign)-1])
                        {
                            case 'I': $$ = "i"; break;
                            case 'F': $$ = "f"; break;
                            case 'B': $$ = "b"; break;
                            case 'S': $$ = "s"; break;
                            default: $$ = "v"; break;
                        } }
    | IDENT '(' Argument ')'    { symbol * target = lookup_symbol($1);
                                    fprintf(fp, "\tinvokestatic Main/%s%s\n", $1, target->sign);
                                    switch((target->sign)[strlen(target->sign)-1])
                                    {
                                        case 'I': $$ = "i"; break;
                                        case 'F': $$ = "f"; break;
                                        case 'B': $$ = "b"; break;
                                        case 'S': $$ = "s"; break;
                                        default: $$ = "v"; break;
                                    } }
    | '(' Expression ')'    { $$ = $2;}
;

Argument
    : Expression 
    | Argument ',' Expression
;

ConversionExpr
    : Type '(' Expression ')'   { char *ans;
                                    asprintf(&ans, "%c2%c", $3[0], $1[0]);
                                    $$ = $1;
                                    fprintf(fp, "\t%s\n", ans); }
;

Literal
    : INT_LIT   { $$ = "i";
                    fprintf(fp, "\tldc %d\n", $1); }
    | FLOAT_LIT { $$ = "f";
                    fprintf(fp, "\tldc %f\n", $1); }
    | BOOL_LIT  { $$ = "b"; }
    | '"' STRING_LIT '"'    { $$ = "a";
                                fprintf(fp, "\tldc \"%s\"\n", $2); }
;

BOOL_LIT
    : TRUE  { fprintf(fp, "\ticonst_1\n"); }
    | FALSE { fprintf(fp, "\ticonst_0\n"); }
;

unary_op
    : '+'   { $$ = "POS"; }
    | '-'   { $$ = "NEG"; }
    | '!'   { $$ = "NOT"; }
;

com_op
    : '>'   { $$ = "ifgt"; }
    | '<'   { $$ = "iflt"; }
    | GEQ   { $$ = "ifge"; }
    | LEQ   { $$ = "ifle"; }
    | EQL   { $$ = "ifeq"; }
    | NEQ   { $$ = "ifne"; }
;

add_op
    : '+'   { $$ = "add"; }
    | '-'   { $$ = "sub"; }
;

mul_op
    : '*'   { $$ = "mul"; }
    | '/'   { $$ = "div"; }
    | '%'   { $$ = "rem"; }
;

Type
    : INT   { $$ = "i"; }
    | FLOAT { $$ = "f"; }
    | BOOL  { $$ = "b"; }
    | STRING    { $$ = "a"; }
;

LBRACE
    : '{'   { yyscope++;
                create_symbol();}
;

RBRACE
    : '}'   { dump_symbol();
                yyscope-=1; }
;
%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    //setup code
    fp = fopen("hw3.j", "w+");
    fprintf(fp, ".source hw3.j\n");
    fprintf(fp, ".class public Main\n");
    fprintf(fp, ".super java/lang/Object\n\n");

    yylineno = 0;
    labeltop = 0;
    globallabel = 0;
    caseend = 0;
    cased = 0;
    error_flag = 0;
    create_symbol();
    yyparse();
    dump_symbol();

    fclose(yyin);
    fclose(fp);
    if(error_flag)
        remove("hw3.j");
    return 0;
}

static void create_symbol() {
    symboltable[yyscope] = NULL;
}

static void insert_symbol(char *name, int addr, int scope, char *type, char *sign) {
    /* create new symbol */
    symbol *newtemp = malloc(sizeof(symbol));
    newtemp->name =  name;
    newtemp->type = type;
    newtemp->sign = sign;
    newtemp->addr = addr;
    newtemp->line = line_count+1;
    newtemp->next = NULL;

    /* enter to correct scope, ypu can't have teo same name in one scope*/
    symbol *temp = symboltable[scope];
    symbol *temp_ex = NULL;
    if(temp == NULL) symboltable[scope] = newtemp;
    else
    {
        while(temp != NULL)
        {
            if(strcmp(temp->name, newtemp->name) == 0)
            {
                char *str;
                asprintf(&str, "%s redeclared in this block. previous declaration at line %d", temp->name, temp->line);
                yyerror(str);
                error_flag = 1;
            }
            temp_ex = temp;
            temp = temp->next;
        }
        temp_ex->next = newtemp;
    }
}

static symbol * lookup_symbol(char* name) {
    int find = 0;
    symbol *target = NULL;
    for(int scope = yyscope; scope>=0; scope-=1)
    {
        symbol *temp = symboltable[scope];
        while(temp != NULL)
        {
            if( strcmp(temp->name, name) == 0 )
            {
                target = temp;
                find = 1;
                break;
            }
            temp = temp->next;
        }
        if(find) break;
    }
    
    /* if we can't find the variable */
    if(!find)
    {
        char *str;
        asprintf(&str, "undefined: %s", name);
        yyerror(str);
        error_flag = 1;
        return NULL;
    }
    return target;
}

static void dump_symbol() {
    symbol *temp = symboltable[yyscope];
    symbol *temp_ex = NULL;
    int count = 0;
    while(temp != NULL)
    {
        temp_ex = temp;
        temp = temp->next;
        free(temp_ex);
        count++;
    }
}