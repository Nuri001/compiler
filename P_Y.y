%union{
char *Val;
struct node* N;
};
%{
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<math.h>
//////////////////////////////////////////////////////////tree////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct node
{
int line;
 char *token;
char var[10];
 char* code;
 struct node *left;
 struct node *right;
} node;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////symbol table////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct Stable
{
/*types->> BOOL    :0         CHAR  :1            INT  :2             REAL  :3             STRING:4           INTs:5            CHARs:6       REALs:7 
                  BOOL[]  :10      CHAR[]:11          INT[]:12          REAL[]:13               
*/
 char *name;
 int type; 
struct Stable *next;
} Stable ;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////procedorTable////////////////////
typedef struct Ptable
{
	/*type--->proc:10; func:11 ; main:12 ; bluk:13*/
	int type; 
	char* name;
	char args_id[15][15];
	int args_type[15];
	int return_type;
   struct Ptable *next;
} Ptable ;
//////////////////////////////////////////////////////////////////////////////////////
typedef struct Scope
{
    char *name;
    struct Stable *Stop;///symantic table
    struct Ptable *MyP;///pointer to the own procedor
    struct Ptable *Ptop;///procedor table
    struct Scope *next;

} Scope;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
node *mknode(char *token, node *left, node *right);
void printtree(int n,node *tree);
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void symantictest(node *tree);
void Test(node *tree);
void Test2(node *tree);
void Test3(node *tree);
void Test4(node *tree);
int isitProcedor(node *tree);
void SetTest(node *tree);
int chekType(node *tree);
void chekReturnVal(int t,char *name,node *tree);
int chekMath(node *tree);
int chekBool(node *tree);
void procsesTest(node *tree);
int functionTest(node *tree);
int  chekS(node *tree);
///////////////////Stable functionf///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void displayStable(Stable *Stop);
void pushStable(Stable *new);
void popStable(Stable *Stop);
void makeStable(node *tree);
void Vars(int type,node *tree);
int scanTable(char *name);
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////Ptable functions///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void displayPtable(Ptable *Ptop);
void pushPtable(Ptable *new);
void popPtable(Ptable *Ptop);
void makePtable(node *tree);
int scanPtable(Ptable *new);
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////scope functions///////////////////////////////////////////////////////////////////////////////////////////////////
void pushScope(Scope *new);
void makeScope(char *N);
void popScope();
void displayScope();
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int flg=1;
int flg2=1;
int m=0;
Scope *top=NULL;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////translet language////////////////////////////////////////////////////////////////////////////////
void trans(node *tree);
void ScanTree(node *tree);
void SetScan(node* tree);
void ScanFunction(node *tree);
void ScanArgs(node *tree);
void ScanV(node *tree);
void scanBol(node *tree);
void scanBody(node* tree);
void scanIfelse(node* tree);
void scanwhile(node* tree);
void scanBluk(node* tree);
void scanProcses(node *tree);
void scanFunc(node* tree);
void scanProc(node* tree);
void printfCode(node *tree);
int var=0;
int lbl=1;
int b=0;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
%}
/*types */
%token BOOL,CHAR,INT,REAL,STRING,INTs,CHARs,REALs
/*reserved words */
%token IF,ELSE,WHILE,VAR,FUNC,PROC,RETURN,NON
/*operators */
%token AND,OR,NOT
%token EQ,NEQ,BIG,BEQ,LES,LEQ
/*ides numbers */
%token  ID;//words and letters 
%token DCM,HEX,REALNUM,REALENUM
%token TRUE,FALSE
/*other*/
%token STR,COMENT,CH
%left EQ NEQ BIG BEQ LES LEQ
%left '+' '-'
%left  '/' '*'
%%
START     :  A       {  $<N>$ = mknode("START",$<N>1,mknode("END START",NULL,NULL));
                       if(flg==1)
                               {
                                 
                               printf("sentx test passed\n");
                             //printtree(0,$<N>$);
                               Test($<N>$);
                             //  if(flg2==1) printtree(0,$<N>$);
                            
                          }                                 
}  ; 

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
A:B A2 {$<N>$= mknode("",$<N>1,$<N>2);};
A2:B A2 {$<N>$= mknode("",$<N>1,$<N>2);}|;
//A              : A2 MN A2   {$<N>$= mknode("",$<N>1,mknode("",$<N>2,$<N>3));}
  //                     | error  { yyerror("the progra must contain a single main");}; 
//A2:B1|;
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//B1: B B2  {$<N>$= mknode("",$<N>1,$<N>2);};
//B2: B B2   {$<N>$= mknode("",$<N>1,$<N>2);}|;
B:PROCSES|FUNCTION;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////main///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*MN:MAIN '('  ')'  '{' BODY '}'  {  $<N>$ = mknode("(MAIN", $<N>5  , mknode(")",NULL,NULL));}
      | MAIN error ')' { yyerror("main procedor dosnt get arguments");};*/
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////PROCSES///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
PROCSES: PROC ID '(' ARG ')'  '{' BODY '}'   {$<N>$ = mknode("(PROC" ,    mknode($<Val>2,$<N>4,$<N>7)       , mknode(")",NULL,NULL) );}
                 |  PROC error '(' { yyerror("required name");}
                 | PROC error ')' { yyerror("arguments not acceptable");};
///////////////////////////////////////FUNCTION/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION : FUNC ID  '(' ARG ')'   RETURN TYPE    '{' BODY  RETURN SET2 ';' '}' {  char c[10]="RETURN  " ; strcat(c,$<Val>7); 
                                                                                                                                     $<N>$ = mknode("(FUNC",                                                                                                    
                                                                                                                                              mknode($<Val>2, $<N>4,
                                                                                                                                                    mknode(c,$<N>11,$<N>9) )                                  
                                                                                                                                                 , mknode(")",NULL,NULL) );
                                                                                                                                       }
                    |  FUNC error '(' { yyerror("required name");}
                     | FUNC error ')' { yyerror("arguments not acceptable");};
////////////////////////////argument////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ARG: VARS  ':' TYPE   ARG2    {$<N>$=mknode("(ARGUMENTS: " ,mknode($<Val>3,$<N>1,NULL),$<N>4);}
         |   {$<N>$=mknode("(no arguments)",NULL,NULL);};
ARG2 : ';'  VARS  ':' TYPE   ARG2     {$<N>$=mknode("AND " ,mknode($<Val>4,$<N>2,NULL),$<N>5);}   
         |  {$<N>$=mknode(")",NULL,NULL);};

////////////////////////////////////////////////////////////////BODY///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
BODY:OPTIONS BODY2  {$<N>$=mknode("(BODY",$<N>1, $<N>2);}
          |  {$<N>$=mknode("(empty)",NULL,NULL);};
BODY2: OPTIONS BODY2  {$<N>$=mknode("", $<N>1, $<N>2);} |  {$<N>$=mknode("END BODY)",NULL, NULL);};
OPTIONS:VAR DECLAR {$<N>$= mknode("VAR",$<N>2,NULL);}
                |SET
                |'^' SET {$<N>$= mknode("^",$<N>2,NULL);}
                |'{' BODY '}'     {$<N>$= mknode("{BLUKE",$<N>2,mknode("}",NULL,NULL));}
                |IFELS 
                |WYL 
                |FUNCTION
                |PROCSES
                |ID '(' ARGS2 ')'  ';'  {$<N>$= mknode("procses:",mknode($<Val>1,mknode("(",NULL,NULL),$<N>3),mknode(")",NULL,NULL));} ;
/////////////////////////////////////////////////////ARGS2////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////                
ARGS2:SET2 ARGS22 {$<N>$= mknode("arguments: ",$<N>1,$<N>2);}| ;
ARGS22: ',' SET2 ARGS22  {$<N>$=mknode(",",$<N>2,$<N>3);}|    ;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// if else and while  /////////////////////////////////////////////////////////////////////////////////////////////////////////
IFELS     : IF '('EXP2 ')' '{' BODY '}' EL  {$<N>$ = mknode("IF(",$<N>3,mknode(")", $<N>6,$<N>8)); };
EL          :  ELSE  '{' BODY  '}'  {$<N>$ = mknode("ELSE",$<N>3,NULL);}|;
WYL       : WHILE '(' EXP2 ')' '{' BODY '}'   {$<N>$ = mknode("(WHILE",$<N>3,mknode(")",$<N>6,NULL));};
/////////////SET/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SET       : ID  SET1 '='    SET2 SET3 {$<N>$ = mknode($<Val>1,$<N>2,mknode("=",$<N>4,NULL));}  | error SET2 { yyerror("ther must be asingle '=' ");   };
SET1      :  ARR  |;
SET2      :EXP2
               | error {yyerror("illigal expession");};
SET3: ';'|error { yyerror("expected  ';' ");   } ;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////ARR//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ARR    : '[' INDEX ']'     {  $<N>$=mknode("[",$<N>2,mknode("]",NULL,NULL));};
 INDEX:ID   {  $<N>$=mknode($<Val>1,mknode("ID",NULL,NULL),NULL);}
           |DCM  {  $<N>$=mknode($<Val>1,mknode("INT",NULL,NULL),NULL);}
           |error { yyerror("incorrect index '[' INDEX ']' ");   };
///////////////////////////////////////////////////////////////BOOLEAN expression////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*BOLEXP:  TRUE   {$<N>$=mknode($<Val>1,NULL,NULL);}
                 |FALSE {$<N>$=mknode($<Val>1,NULL,NULL);};
                 |COND ;
                 |NOT '('BOLEXP ')'  {$<N>$=mknode("!(",$<N>3,mknode(")",NULL,NULL));};
                 |'('BOLEXP ')' OPERATOR BOLEXP2  {$<N>$=mknode($<Val>4,mknode("(",$<N>2,mknode(")",NULL,NULL)),$<N>5);};
BOLEXP2: '('BOLEXP ')' OPERATOR  BOLEXP2    {$<N>$=mknode($<Val>4,mknode("(",$<N>2,mknode(")",NULL,NULL)),$<N>5);} 
                 |'('BOLEXP ')'  {$<N>$=mknode("(",$<N>2,mknode(")",NULL,NULL));} 
                 |error {yyerror("boolean expretion shold be like:  (cond) &&/|| (cond)");};*/
OPERATOR  :  AND  
                     | OR  ;
////////////////condetion expression//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//COND: VARIBALE OPERAND  VARIBALE {$<N>$=mknode($<Val>2,$<N>1,$<N>3);};
OPERAND   : EQ
                    | LEQ
                    |BEQ 
                    |NEQ 
                    | BIG
                    | LES ;
EXP2: EXP EXP3  {$<N>$=mknode("exp",$<N>1,$<N>2);}| EXP4  ;
EXP3: OPERAND EXP  EXP32 {$<N>$=mknode($<Val>1,$<N>2,$<N>3);}| ;
EXP32:OPERATOR EXP2  {$<N>$=mknode($<Val>1,$<N>2,NULL);}|;
EXP4:'(' EXP OPERAND EXP ')' EXP32  {$<N>$=mknode("(",mknode($<Val>3,$<N>2,$<N>4),mknode(")",$<N>6,NULL));}
         |NOT '(' EXP OPERAND EXP ')' EXP32  {$<N>$=mknode("!(",mknode($<Val>4,$<N>3,$<N>5),mknode(")",$<N>7,NULL));};

//////////////math expression//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
EXP:  EXP '+' EXP {$<N>$=mknode("+",$<N>1,$<N>3);}
        | EXP '-' EXP  {$<N>$=mknode("-",$<N>1,$<N>3);}
        | EXP '/' EXP  {$<N>$=mknode("/",$<N>1,$<N>3);}
        | EXP '*' EXP {$<N>$=mknode("*",$<N>1,$<N>3);}
        |  '-' EXP {$<N>$=mknode("MINUS",$<N>2,NULL);}
       | '(' EXP ')' {$<N>$=mknode("(",$<N>2,mknode(")",NULL,NULL));}
         |NOT '(' EXP ')' {$<N>$=mknode("!(",$<N>2,mknode(")",NULL,NULL));}
        |VARIBALE ;
//////////////////////////////////////VARIBALES/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
VARIBALE:ID  {$<N>$=mknode($<Val>$,mknode("ID",NULL,NULL),NULL);}
                 |INTEGER  {$<N>$=mknode($<Val>1,mknode("INT",NULL,NULL),NULL);}
                 |REALN  {$<N>$=mknode($<Val>1,mknode("REAL",NULL,NULL),NULL);}
                 |STR  {$<N>$=mknode($<Val>1,mknode("string",NULL,NULL),NULL);}
                 |'^' ID       {  $<N>$= mknode("^",mknode($<Val>2,NULL,NULL),NULL);}
                 |'^' error  { yyerror("using '^' alowed to use only on pointer varibalse");   };
                | CH {$<N>$=mknode($<Val>1,mknode("char",NULL,NULL),NULL);}
                 |TRUE  {$<N>$= mknode("true",NULL,NULL);} 
                 |ID ARR  { $<N>$=mknode($<Val>1,$<N>2,NULL);}
                 | ID '(' ARGS2 ')' {$<N>$= mknode("function:",mknode($<Val>1,mknode("(",NULL,NULL),$<N>3),mknode(")",NULL,NULL));}
                 | '|' ID '|'  { $<N>$= mknode("|",mknode($<Val>2,NULL,NULL),mknode("|",NULL,NULL));} 
                |'&' ID  SET1    { $<N>$= mknode("&",mknode($<Val>2,NULL,NULL),$<N>3);}
                |FALSE {$<N>$= mknode("false",NULL,NULL);} ; 
INTEGER:DCM 
               |HEX  ;
REALN:REALNUM 
             |REALENUM;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////sequence of variables and numbers statment//////////////////////////////////////////////////////////////////////////////////////////////////////////////
VARS     :ID VARS1    {$<N>$ = mknode($<Val>1 ,$<N>2,NULL);};
VARS1   :','  ID  VARS1  {$<N>$ = mknode($<Val>2,$<N>3,NULL);}
               |  ID {yyerror("expected ',' ");}|;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////declarations//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
DECLAR : VARS  ':' TYPE   DEC { $<N>$ = mknode($<Val>3,$<N>4,$<N>1);} 
                | error TYPE { yyerror("expected ':' befor type  ");   };
DEC         : ARR ';' {$<N>$ =$<N>1;} |';' {$<N>$=NULL;} | error { yyerror("expected  ';' ");   };
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////TYPE///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
TYPE      : REAL                 
                | STRING             
                | INT                    
                | INTs                  
                |CHAR                
                | CHARs              
                | BOOL    
                |REALs       
                |ID  { yyerror("unknown type");}
;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
%% 
#include "lex.yy.c"
main( )
{
 return yyparse();
}

node *mknode(char *token,node *left,node *right)
{
 node *newnode = (node*)malloc(sizeof(node));
 char *newstr = (char*)malloc(sizeof(token) + 1);
 strcpy(newstr,token);
 newnode->left = left;
 newnode->right = right;
 newnode->token = newstr;
newnode->line=yylineno;
 return newnode;
}

void printtree(int n,node *tree)
{
int i=n;
while(i>0){printf(" "); i--;}
 printf("%s\n", tree->token);
 if(tree->left)
 printtree(n+1,tree->left);
 if(tree->right)
 printtree(n+1,tree->right);
} 

void yyerror(char *s) {
    fprintf(stderr, "line %d:  error msg--> %s \n", yylineno, s);
flg=0;
}
////////////*********************************************************************************************************/////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


void Test(node *tree){

makeScope("A");

Test2(tree);

if(m!=1) { printf("symantic error: the program  must contain a singl main procedore \n"); flg2=0;     }
Test3(tree);

if(flg2==1) printf("symantic Test passed Successfuly\n");


printfCode(tree);

}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Test2(node *tree){
int flag=0;
if((strcmp(tree->token,"")==0) && (tree->left)) 
                  if(isitProcedor(tree->left)==1)  {  makePtable(tree->left); flag=1;}


 if((flag==0) && (tree->left))
 Test2(tree->left);
 if(tree->right)
 Test2(tree->right);
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Test3(node *tree){
int flag=0;
if(strcmp(tree->token,"")==0) 
                  if((tree->left)&&(isitProcedor(tree->left)==1))
                                     { Test4(tree->left);   flag=1;        }
 

 if((flag==0) && (tree->left))
 Test3(tree->left);
if(tree->right)
 Test3(tree->right);
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int isitProcedor(node *tree){ 

   if((strcmp(tree->token,"(FUNC")==0)||(strcmp(tree->token,"(PROC")==0)||(strcmp(tree->token,"{BLUKE")==0))  return 1;  
   else return 0;                                            
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Test4(node *tree)
 {
//printf("TEST4 welcome %s \n", tree->token);
Ptable *temp;
node *n;
int flag=0;
int t;
 if(strcmp(tree->token,"(PROC")==0) {
         temp=top->Ptop;
         makeScope(tree->left->token);  
         while(temp!=NULL)
                    {
                     if((temp->name)&&strcmp(temp->name,tree->left->token)==0)    top->MyP=temp; 
                      temp=temp->next; 
                    }         
              n=tree->left->right;
        }
 else if(strcmp(tree->token,"(FUNC")==0){
         temp=top->Ptop;
         makeScope(tree->left->token); 
         while(temp!=NULL)
                    {
                     if((temp->name)&&strcmp(temp->name,tree->left->token)==0) {top->MyP=temp;} 
                         temp=temp->next;
                     }
            
           n=tree->left->right->right;
             flag=1;
            }
 else if(strcmp(tree->token,"{BLUKE")==0){ makeScope("A");  n=tree->left;}
 else if(strcmp(tree->token,"IF(")==0){ 
                         
                         n=tree->right->left;
                         t=chekType(tree->left);
                         if(t!=0)  { printf("line:%d symantic error: incorrect IF statment, the expression in if(expreesion) must be a boolean type  \n",tree->line-3); flg2=0;     }
                        makeScope("if"); 
                         if(tree->right->right){
                               symantictest(n);
                                popScope();
                               makeScope("else");
                               n=tree->right->right->left;

                            }
                 }
 else if(strcmp(tree->token,"(WHILE")==0){
                  n=tree->right->left;
                  t=chekType(tree->left);
                 makeScope("while");
                  if(t!=0)  { printf("line:%d symantic error: incorrect WHILE statment, the expression in whle(expreesion) must be a boolean type  \n",tree->line); flg2=0;     }
                
       }
 else { printf("symantic error: incorrect Scope %s type \n",tree->token); flg2=0;                 }
//printf("TEST4 welcome %s befor simanticCHek\n", tree->token);
symantictest(n);
if(flag==1) chekReturnVal(top->MyP->return_type,tree->left->token,tree->left->right->left);

if(strcmp(tree->token,"IF(")==0)scanIfelse(tree) ;
if(strcmp(tree->token,"(WHILE")==0) scanwhile(tree);
 if(strcmp(tree->token,"{BLUKE")==0) scanBluk( tree);
if(strcmp(tree->token,"(FUNC")==0) scanFunc(tree);
 if(strcmp(tree->token,"(PROC")==0) scanProc(tree);
popScope();
} 

////////////////////////////////////////////////////////////
void chekReturnVal(int t,char *name,node *tree)
{
int type;
if(t==4) {  printf("line:%d symantic error: illigal type return   \n",tree->line); flg2=0;}
type=chekType(tree);
if(type!=t) {  printf("line:%d symantic error: return value of function %s not match    \n",tree->line,name); flg2=0;}
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void symantictest(node *tree){
int flag=0;
//printf("simanticCHek welcome %s \n", tree->token);
 if(strcmp(tree->token,"^")==0) {SetTest(tree);flag=1;}
//printf("1 \n");
 if(strcmp(tree->token,"VAR")==0) {makeStable(tree->left); flag=1;}
//printf("2 \n");
 if(isitProcedor(tree)==1){   makePtable(tree);   Test4(tree);  flag=1;}
//printf("3 \n");
  if(strcmp(tree->token,"IF(")==0){    Test4(tree);  flag=1;}
 if(strcmp(tree->token,"(WHILE")==0){    Test4(tree);  flag=1;}
//printf("4 \n");
 if((tree->right) && (strcmp(tree->right->token,"=")==0)){ SetTest(tree); flag=1;}
//printf("5 \n");
 if(strcmp(tree->token,"procses:")==0){ procsesTest(tree); flag=1;}
 if((flag==0) && (tree->left)) symantictest(tree->left);
 if((flag==0) && (tree->right)) symantictest(tree->right);

}
///////////////////////////////////////////
void procsesTest(node *tree)
{
 Scope *tmp;
 Ptable *p;
 Ptable *MyP;
node *arg;
MyP=NULL;
 tmp=top;
int i=0;
int a1;

////////////////////is the procses exsists///////////////////////////////////////
 while((MyP==NULL)&&tmp)
   {
     p=tmp->Ptop;
     while(p)
       {
          if((p->name)&& (strcmp(p->name,tree->left->token)==0))MyP=p;
          p=p->next;
       } 
    tmp=tmp->next;
  }
/////////////////////////////////////////////////////////////////////////////////////////////////////
 if(MyP==NULL)  {  printf("line:%d symantic error: %s not exsists  \n",tree->line,tree->left->token); flg2=0;    }
//////////////////if arguments are matched////////////////////////////////
 if(tree->left->right)
   {
     arg=tree->left->right;
     while(arg){
       a1=chekType(arg->left);
       if(a1!=MyP->args_type[i]){  printf("line:%d symantic error: %s not match as argument to %s  \n",tree->line,arg->left->token,MyP->name); flg2=0;    } 
       arg=arg->right;      
      i++;
      }
     if(MyP->args_type[i]) {  printf("line:%d symantic error: too few arrguments to %s  \n",tree->line,MyP->name); flg2=0;    } 
   }
scanProcses(tree);
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void SetTest(node *tree){
int v=0;
//printf("setTest on %s\n",tree->token);
 if(strcmp(tree->token,"^")==0){v=1; tree=tree->left;}
int type=isExist(tree->token);
int type2;
if((tree->left)&& (type!=10) && (type!=11)&& (type!=12)&& (type!=13) && (type!=4))
 {printf("line:%d symantic error: using '[index]'  on varibal %s is illigal \n",tree->line,tree->token);
   flg2=0;}

if((type==10) || (type==11) || (type ==12) || (type==13))
   {
     if(tree->left && tree->left->left) { 
             type2=  chekType(tree->left->left);
             if(type2==2) type=type-10;
             else { printf("line:%d symantic error: the index of '[index]' must be an int type \n",tree->line,tree->token); flg2=0;             }
         }
     else {printf("line:%d symantic error: expected '[index]'  on varibal %s \n",tree->line,tree->token); flg2=0;}
   }


if(v==1) 
       {
            if(type==5) type=2;
            else if(type==6) type=1;
            else if(type==7) type=3;
            else{  printf("line:%d symantic error: illigal to use '^' on '%s'  \n",tree->line,tree->token); flg2=0;}
       } 



if( (type==4) && (tree->left) ){
                  type2=  chekType(tree->left->left);
                   if(type2==2) type=1;
                else{ printf("line:%d symantic error: the index of '[index]' must be an int type \n",tree->line,tree->line); flg2=0;             }
             }


if(type==-1){printf("line:%d symantic error: varibale named \" %s\" not exist\n",tree->line,tree->token); flg2=0;}
else {   
               
              type2= chekType(tree->right->left);    
   
             if(type!=type2) { printf("line:%d symantic error: incorrect placement to %s varibal \n",tree->line,tree->token); flg2=0;   }

    }

ScanV(tree);


  
}
/*types->> BOOL    :0         CHAR  :1            INT  :2             REAL  :3             STRING:4           INTs:5            CHARs:6       REALs:7 
                  BOOL[]  :10      CHAR[]:11          INT[]:12          REAL[]:13               
*/
//////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int chekType(node *tree){
int t,t2;
//printf("chektype %s\n",tree->token);
     if(strcmp(tree->token,"MINUS")==0) return chekType(tree->left);
      if(strcmp(tree->token,"exp")==0){   if(tree->right) return chekBool(tree);
                                                                  else return chekType(tree->left);
                                                            }
      if(strcmp(tree->token,"(")==0) return chekBool(tree) ;
      if((strcmp(tree->token,"+")==0)||(strcmp(tree->token,"-")==0)||(strcmp(tree->token,"*")==0)||(strcmp(tree->token,"/")==0)) return chekMath(tree);  

    if(tree->left){  
     if(strcmp(tree->left->token,"char")==0) return 1;
      if(strcmp(tree->left->token,"string")==0) return 4;    
      if(strcmp(tree->left->token,"INT")==0) return 2;
      if(strcmp(tree->left->token,"REAL")==0) return 3;
      if(strcmp(tree->left->token,"ID")==0) return isExist(tree->token);
        }
      if(strcmp(tree->token,"true")==0) return 0; 
      if(strcmp(tree->token,"false")==0) {printf("catch false\n"); return 0; }
      if((strcmp(tree->token,"&&")==0)||(strcmp(tree->token,"||")==0)||(strcmp(tree->token,"==")==0)||(strcmp(tree->token,"<=")==0)||(strcmp(tree->token,">=")==0)||(strcmp(tree->token,"!=")==0)||
      (strcmp(tree->token,">")==0)||(strcmp(tree->token,"<")==0)) { return chekBool(tree);}
      if(strcmp(tree->token,"function:")==0) return functionTest(tree); 
      if(strcmp(tree->token,"|")==0)    return chekS(tree->left); 
      if(strcmp(tree->token,"&")==0)  { 
                             t=isExist(tree->left->token);
                             if(t==1) return 6;
                             if(t==2) return 5; 
                             if(t==3) return 7; 
                             if(tree->right) { 
                                            t2=  chekType(tree->right->left);            
                                            if(t2==2){
                                               if((t==4 )||( t==11)) return 6;
                                               else if(t==12 ) return 5;
                                              else if(t==13 ) return 7;
                                               else {       printf("line:%d symantic error: %s illigal type \n",tree->line,tree->left->token);            return -1; }
                                             }
                                            else{ printf("line:%d symantic error: the index of '[index]' must be an int type \n",tree->line); flg2=0; return -1;  }           
                              }

                             else{       printf("line:%d symantic error: %s illigal type \n",tree->line,tree->left->token);                  return -1; }
                          }
     if(strcmp(tree->token,"^")==0)   { 
                        t=isExist(tree->left->token);
                        if(t==5) return 2;
                        if(t==6) return 1; 
                        if(t==7) return 3; 
                        else{       printf("line:%d symantic error: %s illigal type \n",tree->line,tree->left->token);                  return -1; }
                        } 
   if((tree->left) && (strcmp(tree->left->token,"[")==0)){
          t=isExist(tree->token);
          t2=chekType(tree->left->left);
          if((t==10) || (t==11) || (t==12) || (t==13)){ 
                  if(t2==2) return t-10;
                  else { printf("line:%d symantic error: the index of '[index]' must be an int type \n",tree->line); flg2=0;     return -1;        }                    
                  } 
          else if(t==4) { 
                  if(t2==2) return 1;
                  else { printf("line:%d symantic error: the index of '[index]' must be an int type \n",tree->line); flg2=0;     return -1;        }                    
                  } 
         else { printf("line:%d symantic error: using '[index]' on %s is illigal \n",tree->line,tree->token); flg2=0;     return -1;        }    
    }

return -1;
}
/*types->> BOOL    :0         CHAR  :1            INT  :2             REAL  :3             STRING:4           INTs:5            CHARs:6       REALs:7 
                  BOOL[]  :10      CHAR[]:11          INT[]:12          REAL[]:13               
ID ARR  { $<N>$=mknode($<Val>1,$<N>2,NULL);}
*/
/////////////////////////////////////////////////////////////////////////
int  chekS(node *tree)
{
int t=isExist(tree->token);
if(t==4) return 2;
else { printf("line:%d symantic error: expexted for  s to be a string type for the: \"|s|\" ,%s not  a string type \n",tree->line,tree->token); flg2=0;   return -1 ; }

}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int  functionTest(node *tree)
{
 Scope *tmp;
 Ptable *p;
 Ptable *MyP;
node *arg;
MyP=NULL;
 tmp=top;
int i=0;
int a1;

////////////////////is the function exsists//////////////////////////////////////////////////////
 while((MyP==NULL)&&tmp)
   {
     p=tmp->Ptop;
     while(p)
       {
          if((p->name)&& (strcmp(p->name,tree->left->token)==0)){ MyP=p;}
          p=p->next;
       } 
    tmp=tmp->next;
  }
////////////////////////////////////////////////////////////////////////////////////////////////////////////

 if(MyP==NULL) {  printf("symantic error: %s not exsists  \n",tree->left->token); flg2=0;    }
 
   /////////if arguments are matched///////////////////////////////////////////////
 if(MyP && tree->left->right)
   {
     arg=tree->left->right; 
     while(arg){
       a1=chekType(arg->left);
       if(a1!=MyP->args_type[i]){  printf("symantic error: %s not match as argument to %s  \n",arg->left->token,MyP->name); flg2=0;    } 
       arg=arg->right;      
      i++;
      }
     if(MyP->args_type[i]) {  printf("symantic error: too few arrguments to %s  \n",MyP->name); flg2=0;    } 
  }

if((MyP)&&(MyP->return_type)) return MyP->return_type;
else return -1;
}



int chekBool(node *tree){
  int t,l,r,flag=0;
  

    if((strcmp(tree->token,"&&")==0)|| (strcmp(tree->token,"||")==0)) return chekBool(tree->left);
                                                                                
                                                                                                           
       if(strcmp(tree->token,"exp")==0){
              l=chekType(tree->left);
              if(tree->right){
                     r=chekType(tree->right->left);
                     if((strcmp(tree->right->token,"==")==0) || (strcmp(tree->right->token,"!=")==0))
                                   if(r!=l) { printf("symantic error: illigal to use '==' or '!=' between different types \n"); flg2=0; return -1;   }
                     else if((strcmp(tree->right->token,">=")==0) || (strcmp(tree->right->token,"<=")==0)|| (strcmp(tree->right->token,"<")==0)|| (strcmp(tree->right->token,">")==0))
                                  if(r!=2 || l!=2) { printf("symantic error: using '<=', '>=' ,'>' or '<' are aloweed only on int/real types  \n"); flg2=0; return -1;   }
           
                  if(tree->right->right) return chekBool(tree->right->right); 
                 else return 0;                                             
                }
              else return l;
       }

     if((strcmp(tree->token,"(")==0)||(strcmp(tree->token,"!(")==0)){
          l=chekType(tree->left->left);
          r=chekType(tree->left->right);
          if((strcmp(tree->left->token,"==")==0) || (strcmp(tree->left->token,"!=")==0))
                  if(r!=l) { printf("symantic error: illigal to use '==' or '!=' between different types \n"); flg2=0; return -1;   }
          else if((strcmp(tree->left->token,">=")==0) || (strcmp(tree->left->token,"<=")==0)|| (strcmp(tree->left->token,"<")==0)|| (strcmp(tree->left->token,">")==0))
                 if(r!=2 || l!=2) { printf("symantic error: using '<=', '>=' ,'>' or '<' are aloweed only on int/real types  \n"); flg2=0; return -1;   }
           
          if(tree->right->left) return chekBool(tree->right->left); 
         else return 0;
       }



}



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int chekMath(node *tree){
  int t,r,l;

     l=chekType(tree->left);
     r=chekType(tree->right);
//printf("left:token %s type %d \nright:token %s type %d   \n",tree->left->token,l,tree->right->token,r);
     if(l==-1 || r==-1) return -1;   
    if(l==3 || r==3)  return 3;
    if((r==2 && l==2)|| (r==2 && l==100) || (r==100 && l==2) ) return 2;



}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int isExist(char *id){       
 // printf("isExist %s \n ",id); 
Scope *temp;
temp=top;
Stable *st;
Ptable *pt;
int i;
while(temp!=NULL)
   { 
      st=temp->Stop;
      while(st!=NULL)
        {
         // printf("try %s\n ",st->name); 
          if (strcmp(st->name,id)==0) {  return st->type; }
          st=st->next;             
         }          
         
       pt=temp->MyP;
      if(pt!=NULL){    for( i=0;strlen(pt->args_id[i])>0;i++) 
              { 
                  //printf("try arg %s\n ",pt->args_id[i]);
                 if(strcmp(pt->args_id[i],id)==0)
                              {   
                                   //printf("return type %d\n ",pt->args_type[i]);
                                  return pt->args_type[i];  
              
                               }
                }    
          }
         temp=temp->next;

        }  

return -1;
}
/////////symbol table functions///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void makeStable(node *tree)
{
/*types->> BOOL    :0         CHAR  :1            INT  :2             REAL  :3             STRING:4           INTs:5            CHARs:6       REALs:7 
                  BOOL[]  :10      CHAR[]:11          INT[]:12          REAL[]:13               
*/
int t;
char  type[10]="";
strcpy(type,tree->token);

 if(strcmp(type,"bool")==0) t=0;
if(strcmp(type,"char")==0) t=1;
if(strcmp(type,"int")==0) t=2;
if(strcmp(type,"real")==0) t=3;
if(strcmp(type,"string")==0) t=4;
if(strcmp(type,"int*")==0) t=5;
if(strcmp(type,"char*")==0) t=6;
if(strcmp(type,"real*")==0) t=7;

if(tree->left != NULL) t=t+10;
Vars(t,tree->right);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Vars(int type,node *tree){
Stable *ptr= (Stable*)malloc(sizeof(Stable));
char *newid = (char*)malloc(sizeof(tree->token) + 1);
strcpy(newid,tree->token);
ptr->type=type;
ptr->name=newid;
pushStable(ptr);

if(tree->left!=NULL)  Vars(type,tree->left);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int scanTable(char *name){
Stable  *temp;
Ptable *pt;
int i;
temp=top->Stop;
while(temp!=NULL)
{
 if(strcmp(temp->name,name)==0) return 1;
temp=temp->next;
}

if(top->MyP) { 
        pt=top->MyP;
        for( i=0;strlen(pt->args_id[i])>0;i++) 
              { 
                 if(strcmp(pt->args_id[i],name)==0)
                              {   
                                   
                                  return pt->args_type[i];  
              
                               }
                }    

     }
        

return 0;
}
////////////////////////
void displayStable(Stable  *Stop){
Stable  *temp;
temp=Stop;
printf("desplay stable\n");
while(temp!=NULL)
{
 printf("ID:%s  type:%d \n " ,temp->name,temp->type);
temp=temp->next;
}

printf("end dwsplay stable\n");
}
////////////////////////////////////////////////////////////////////////
void pushStable(Stable *new){
if (scanTable(new->name)==0)
{
    new->next=top->Stop;
    top->Stop=new;
}
else  {  printf("symantic error: varibale named \" %s\" alrady exists in this scope\n");   flg2=0;}
}
void popStable(Stable *Stop){

if(Stop==NULL) printf ("Stable is empty\n");
else {
  Stable *temp;
 temp=Stop;
 Stop=Stop->next;
 printf("%s deleted\n", temp->name);
free(temp); }

}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////Ptable functionf///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void displayPtable(Ptable *Ptop){
Ptable  *temp;
temp=Ptop;
int i;
printf("desplay Ptable\n");
while(temp!=NULL)
{
   if(temp->type==11)
        {
           printf("FUNCTION: %s   return type:%d   \n " ,temp->name,temp->return_type);
          printf("arguments: \n" );
          for( i=0;strlen(temp->args_id[i])>0;i++)  printf("id: %s  type: %d\n",temp-> args_id[i],temp->args_type[i]);
           }

   if(temp->type==10)
          {
           printf("PROC: %s \n" ,temp->name);
           printf("arguments: \n" );
         for( i=0;strlen(temp->args_id[i])>0;i++)  printf("id: %s  type: %d\n",temp-> args_id[i],temp->args_type[i]);
           }

   if(temp->type==12)     printf("MAIN procedor\n");
   if(temp->type==13) printf("BLUKE procedor\n");

temp=temp->next;
}

printf("end dwsplay Ptable\n");

}
////////////////////////////////////////////////////////////////////////
void makePtable(node *tree)
{
Ptable *ptr= (Ptable*)malloc(sizeof(Ptable));
char  type[10]="";
strcpy(type,tree->token);

node *args;
args=NULL;
node *args2=NULL;

        if(strcmp(type,"(PROC")==0)  ////args:tree->left->left 
          {      
                ptr->type=10;
               char *name = (char*)malloc(sizeof(tree->left->token) + 1);
                strcpy(name,tree->left->token);
                ptr->name=name;
               args=tree->left->left; 
          }
       if(strcmp(type,"(FUNC")==0) 
         {
            ptr->type=11;
           char *name = (char*)malloc(sizeof(tree->left->token) + 1);
            strcpy(name,tree->left->token);
           ptr->name=name;
          args=tree->left->left;

          char *rt= (char*)malloc(sizeof(tree->left->right->token) + 1);
          strcpy(rt,tree->left->right->token); 

         if(strcmp(tree->left->right->token,"RETURN  bool")==0) ptr->return_type=0;
        if(strcmp(tree->left->right->token,"RETURN  char")==0) ptr->return_type=1;
         if(strcmp(tree->left->right->token,"RETURN  int")==0)ptr->return_type=2;
        if(strcmp(tree->left->right->token,"RETURN  real")==0) ptr->return_type=3;
         if(strcmp(tree->left->right->token,"RETURN  string")==0)  ptr->return_type=4;
        if(strcmp(tree->left->right->token,"RETURN  int*")==0)  ptr->return_type=5;
        if(strcmp(tree->left->right->token,"RETURN  char*")==0)  ptr->return_type=6;
        if(strcmp(tree->left->right->token,"RETURN  real*")==0)  ptr->return_type=7;

        }

if(strcmp(type,"{BLUKE")==0)  ptr->type=13; 

if((args!=NULL) &&(strcmp(args->token,"(ARGUMENTS: ")==0) )
    { 
           int i=0;
          int t=-1;
  while (strcmp(args->token,")")!=0)
         {
                if(strcmp(args->left->token,"bool")==0) t=0;
               if(strcmp(args->left->token,"char")==0) t=1;
               if(strcmp(args->left->token,"int")==0) t=2;
               if(strcmp(args->left->token,"real")==0) t=3;
               if(strcmp(args->left->token,"string")==0) t=4;
               if(strcmp(args->left->token,"int*")==0) t=5;
               if(strcmp(args->left->token,"char*")==0) t=6;
               if(strcmp(args->left->token,"real*")==0) t=7;
               if(t==-1) {printf("illegal argument type\n ");   flg2=0;  }  

              args2=args->left->left;///vars node
   
                   while(args2 != NULL)
                   {                     
                               strcpy(ptr->args_id[i],args2->token);                                    
                               ptr->args_type[i]=t;
                               args2=args2->left;
                             i++;
                        }
            args=args->right;          
          }
} 

pushPtable(ptr); 

}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void pushPtable(Ptable *new){
if (scanPtable(new)==0)
{

     if((new->name) && ((strcmp(new->name,"main")==0) || (strcmp(new->name,"Main")==0))) m++;

    new->next=top->Ptop;
    top->Ptop=new;

}
else { printf("symantic error: procedor named \"%s\" alrady exists in this scope\n"); flg2=0;}
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void popPtable(Ptable *Ptop){
if(Ptop==NULL) printf ("Ptable is empty\n");
else {
  Ptable *temp;
 temp=Ptop;
 Ptop=Ptop->next;
if(temp->type==10) printf("procses name:%s deleted\n", temp->name);
if(temp->type==11) printf("function name:%s deleted\n", temp->name);
if(temp->type==12) printf("main deleted\n");
if(temp->type==13) printf("bluke deleted\n");
free(temp); }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int scanPtable(Ptable *new){
Ptable  *temp;
temp=top->Ptop;

if(new->type==10 || new->type==11)  {
         while(temp!=NULL){
                    if((temp->type==10 ||   temp->type==11) &&(strcmp(temp->name,new->name)==0) )return 1;
              temp=temp->next;}
}
return 0;
}
/////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////scope functions////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void pushScope(Scope *new)
{
//printf("push scope %s\n",new->name);
    new->next=top;
    top=new;
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void makeScope(char *N){
Scope *ptr= (Scope*)malloc(sizeof(Scope));
char *name = (char*)malloc(sizeof(N) + 1);
strcpy(name,N);
ptr->name=name;
ptr->Ptop=NULL;
ptr->Stop=NULL;

pushScope(ptr);
}
/////////////////////////////////////////////////////////////////////////
void popScope(){
if(top==NULL) printf ("Scope is empty\n");
else {
//printf("deleting Scop %s \n",top->name);

 Scope *temp;
 temp=top;
 top=top->next;

free(temp); }
}
////////////////////////////////////////////////////////////////
void displayScope(){
Scope *temp;
temp=top;

printf("Scope display\n");
while(temp!=NULL)
{
printf("Scope name: %s \n",temp->name);
if(temp->MyP!=NULL)printf("%s \n \n \n",temp->MyP->name);
displayPtable(temp->Ptop);
displayStable(temp->Stop);
temp=temp->next;
}
printf("end scope display\n");

}
//^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^
//^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^
//^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^/^

void  ScanTree(node *tree){
	char tmp[50]="     ";
	char tmpv[10];
	char s[2];
	int t,t1,t2;
int l=0;
int r=0;
	
	if(strcmp(tree->token,"MINUS")==0){
                ScanTree(tree->left);
		sprintf(tree->var,"t%d",var);
		var++;
                strcat(tmp,tree->var);
                strcat(tmp,"=-");     
                strcat(tmp,tree->left->var);     
                strcat(tmp,"\n    ");  
                if(tree->left->code) {
		       char *cod = (char*)malloc(sizeof(tmp) +sizeof(tree->left->code)+5 );
		        strcpy(cod,tree->left->code);
                       strcat(cod,tmp); 
                        }
               else {  char *cod = (char*)malloc(sizeof(tmp)+3 );
                                strcpy(cod,tmp);                              }
	          }	

 	if((strcmp(tree->token,"+")==0)||(strcmp(tree->token,"-")==0)||(strcmp(tree->token,"*")==0)||(strcmp(tree->token,"/")==0))
			{
	               ScanTree(tree->left);
	               	ScanTree(tree->right);
		        sprintf(tree->var,"t%d",var);
	       	        var++;                  
                       strcat(tmp,tree->var);
                       strcat(tmp,"=");
                      strcat(tmp,tree->left->var);
                      strcat(tmp,tree->token);
                      strcat(tmp,tree->right->var);
                       strcat(tmp,"\n    ");
		       if(tree->left->code) l=1;
                       if(tree->right->code) r=1;
//        printf("left:%s \n -------------------- right: %s \n",tree->left->code,tree->right->cod	        	e);
                    

                      if(l==1 && r==1){
		               char *cod = (char*)malloc(sizeof(tmp) +sizeof(tree->left->code)+sizeof(tree->right->code)+5);
                                strcpy(cod,tree->left->code);
                                 strcat(cod,tree->right->code);
                                  strcat(cod,tmp);
                                tree->code=cod;}
                  else if(l==1 && r==0){
			    char *cod = (char*)malloc(sizeof(tmp) +sizeof(tree->left->code)+5);
                                strcpy(cod,tree->left->code);
                                  strcat(cod,tmp);
                                tree->code=cod;}
                         
                  else if(l==0 && r==1){
			    char *cod = (char*)malloc(sizeof(tmp) +sizeof(tree->right->code)+5);
                                strcpy(cod,tree->right->code);
                                  strcat(cod,tmp);
                                tree->code=cod;}
                         
                 else {
			    char *cod = (char*)malloc(sizeof(tmp)+1);
                                strcpy(cod,tmp);
                                tree->code=cod;}
	          	  
                 }

                if(strcmp(tree->token,"(")==0){
                   ScanTree(tree->left);
                   strcpy(tree->var,tree->left->var);
                  if(tree->left->code)tree->code=tree->left->code;
                      
                  }

                          if(strcmp(tree->token,"^")==0){
		               strcpy(tree->var,"^");        
                               strcat(tree->var,tree->left->token); }
	  
			  if(strcmp(tree->token,"|")==0){
			     strcpy(tree->var,"|");        
                             strcat(tree->var,tree->left->token);
                               strcat(tree->var,"|"); }
				  
			 if(strcmp(tree->token,"true")==0) strcpy(tree->var,"1");
		         if(strcmp(tree->token,"false")==0) strcpy(tree->var,"0");


               if( tree->left){ 
                    if((strcmp(tree->left->token,"ID")==0)||(strcmp(tree->left->token,"INT")==0)||(strcmp(tree->left->token,"REAL")==0) )strcpy(tree->var,tree->token);
		    if((strcmp(tree->left->token,"string")==0)||(strcmp(tree->left->token,"char")==0)) strcpy(tree->var,tree->token);		              	
                    

                    if(strcmp(tree->left->token,"[")==0)
					  {
						 t= isExist(tree->token);
						  if(t==10)sprintf(s,"1");
						  else if(t==11)sprintf(s,"1");
						  else if(t==12)sprintf(s,"4");
						  else if(t==13)sprintf(s,"8");
						  else printf("line %d: error on scan\n",tree->line);
						  
						  t1=var;
						  sprintf(tmpv,"t%d",var);
	       	                                  var++; 						    
					      strcat(tmp,tmpv);
						  strcat(tmp,"=");
						  strcat(tmp,tree->left->left->token);
						  strcat(tmp,"\n     ");
						  t2=var;
						  sprintf(tmpv,"t%d",var);
	       	                                  var++; 
						  strcat(tmp,tmpv);
						  strcat(tmp,"=");
						  strcat(tmp,s);
						  strcat(tmp,"\n     ");
						  t=var;
						  sprintf(tmpv,"t%d",var);
	       	                                  var++; 
						  strcat(tmp,tmpv);
						  strcat(tmp,"=");
						  sprintf(tmpv,"t%d*t%d",t1,t2);
						  strcat(tmp,tmpv);
						  strcat(tmp,"\n     ");
						   sprintf(tree->var,"t%d",var);
						   var++;
						   strcat(tmp,tree->var);
						  strcat(tmp,"=");
						  strcat(tmp,tree->token);
						  sprintf(tmpv,"+t%d",t);
						   strcat(tmp,tmpv);
                                                   strcat(tmp,"\n    ");
						 char *cod = (char*)malloc(sizeof(tmp)+1);
                                                 strcpy(cod,tmp);
                                                 tree->code=cod;
					


                      }/////end of   if(strcmp(tree->left->token,"[")==0)                                      

             }///end of if(	 tree->left)

       if(strcmp(tree->token,"&")==0){
                   strcpy(tree->var,"&");  
                    if(tree->right){
                                tree->left->left=tree->right;
                                ScanTree(tree->left);
                                strcat(tree->var,tree->left->var);
                                tree->code=tree->left->code;

                       }
                  
                     else strcat(tree->var,tree->left->token);
           }


    if(strcmp(tree->token,"function:")==0)ScanFunction(tree);                  
                  
	
}///end of function


void ScanFunction(node *tree){	
//printf("ScanFunction token: %s\n",tree->token);
 Scope *tmp;
 Ptable *p;
 Ptable *MyP;
MyP=NULL;
 tmp=top;
int i;
int popSize=0;
char tmp2[20]="\n     ";
char s[2];


 while((MyP==NULL)&&tmp)
   {
     p=tmp->Ptop;
     while(p)
       {
          if((p->name)&& (strcmp(p->name,tree->left->token)==0)) MyP=p;
          p=p->next;
         } 
    tmp=tmp->next;
   }	
	
 if(MyP==NULL)  printf("error: %s not exsists  \n",tree->left->token);    


for(i=0;strlen(MyP->args_id[i])>0;i++)
{
      //printf("ScanFunction for i=%d \n type=%d\n",i,MyP->args_type[i]);
	if(MyP->args_type[i]==0) popSize+=1;
 	if(MyP->args_type[i]==1) popSize+=1;
	if(MyP->args_type[i]==2) popSize+=4;
	if(MyP->args_type[i]==3) popSize+=8;		
}

if(tree->left->right)ScanArgs(tree->left->right); 
  
  sprintf(tree->var,"t%d",var);var++; 		  
  strcat(tmp2,tree->var);
  strcat(tmp2,"=LCall ");   			  
  strcat(tmp2,tree->left->token); 
  strcat(tmp2,"\n     ");
  sprintf(s,"%d",popSize);
  strcat(tmp2,"PopParams "); 
  strcat(tmp2,s); 
strcat(tmp2,"\n    ");
 
if(tree->left->right){
         char *cod = (char*)malloc(sizeof(tmp2)+sizeof(tree->left->right->code)+4);
          strcpy(cod,tree->left->right->code);
		  strcat(cod,tmp2);
          tree->code=cod;

         }	
   else{
	    char *cod = (char*)malloc(sizeof(tmp2)+1);
          strcpy(cod,tmp2);
          tree->code=cod;	   
   }
   
   
   
}




void ScanV(node *tree){

    char tmp[50];     
char tmp2[200];
               if(tree->left)ScanTree(tree);                
               else strcat(tree->var,tree->token);
               SetScan(tree->right->left);  
 
               strcpy(tmp,tree->var);
               strcat(tmp,"=");
               strcat(tmp,tree->right->left->var);
              strcat(tmp,"\n");

               if(tree->code){
                      if(tree->right->left->code){
                                strcpy(tmp2,tree->right->left->code); 
                                char *cod = (char*)malloc(sizeof(tmp)+sizeof(tree->code)+sizeof(tmp2)+1);              
                              // strcpy(cod,"\n    ");  
                                strcpy(cod,tree->code);         
                                strcat(cod,tmp2);    
                                strcat(cod,tmp);                         
                                tree->code=cod; }
                  
                       else{
                                char *cod = (char*)malloc(sizeof(tmp)+sizeof(tree->code)+1);              
                                strcpy(cod,tree->code);         
                                strcat(cod,tmp);                         
                                tree->code=cod; }

                             }                             
            

           else {
                   if(tree->right->left->code){ 
                         strcpy(tmp2,tree->right->left->code);  
                         char *cod = (char*)malloc(sizeof(tmp)+sizeof(tmp2)+5);   


                          strcpy(cod,tmp2);
                          strcat(cod,tmp);           
                       //strcat(cod,"\n    ");                      
                          tree->code=cod;      } 
                  else {

                           char *cod = (char*)malloc(sizeof(tmp)+1);   
                         // strcpy(cod,"     ");  
                          strcpy(cod,tmp);                      
                          tree->code=cod;  }


                              
                                                              
                   }
  }                            


void SetScan(node* tree)
{
char tmp[300]="     ";
char tmpv[20];
       if(strcmp(tree->token,"exp")==0){
       if(tree->right)  {
                              
	                            scanBol(tree);  
				strcat(tmp,tree->code);
                               
                                sprintf(tmpv,"=%s",tree->var);
				sprintf(tree->var,"t%d",var);	 var++;
                                strcat(tmp,tree->var);
				 strcat(tmp,tmpv);
				strcat(tmp,"\n     ");
                                }
              else {
             
                         ScanTree(tree->left);		
                           						 
                         if(tree->left->code)strcat(tmp,tree->left->code);
			 sprintf(tree->var,"t%d",var);	 var++;
			 strcat(tmp,tree->var);
			 strcat(tmp,"=");
			 strcat(tmp,tree->left->var);
			 strcat(tmp,"\n     ");
                    }   //end else               
            } 



else if((strcmp(tree->token,"(")==0) ||(strcmp(tree->token,"!(")==0)){
	scanBol(tree);
   
	strcat(tmp,tree->code);    
   
    sprintf(tmpv,"=%s",tree->var);
	sprintf(tree->var,"t%d",var);	 var++;
     strcat(tmp,tree->var);

	strcat(tmp,tmpv);
	strcat(tmp,"\n      ");
 

}

  else printf("errorr for now 200\n");

  char *cod = (char*)malloc(sizeof(tmp)+1);
   strcpy(cod,tmp);
tree->code=cod;
   }


 ////////////////////////////////////////////////////////////////////////////////////


void scanBol(node *tree)
{
	node *tmpt;
        char *cd;
        char s[10];
	char c1[100];
	char c2[100];	
char tmp[100];



	 if(strcmp(tree->token,"exp")==0)
	 {
		  ScanTree(tree->left);
		   if(tree->left->code) strcpy(c1,tree->left->code);
		
		if(tree->right){
			    tmpt=tree->right;
			    ScanTree(tmpt->left);
			    if(tmpt->left->code) strcpy(c2,tmpt->left->code);
			  sprintf(tree->var,"t%d",var);	 
	                  var++;	   
                  sprintf(tmp,"if %s %s %s  goto lbl%d\n      %s=0\n      goto lbl%d\nlbl%d: %s=1\nlbl%d: "
				  ,tree->left->var,tmpt->token,tmpt->left->var,lbl,tree->var,lbl+1,lbl,tree->var,lbl+1);
                   lbl++;
                   lbl++;
				   
                                    
				   if((tree->left->code)&&(tmpt->left->code)) {
					    char *cod = (char*)malloc(sizeof(tmp)+sizeof(c1)+sizeof(c2)+1);
                                          strcpy(cod,c1);
				          strcat(cod,c2);
				         strcat(cod,tmp);
				        tree->code=cod; }	  
					   
				  
				   else if(tree->left->code){
                   	    char *cod = (char*)malloc(sizeof(tmp)+sizeof(c1)+1);
                        strcpy(cod,c1);
						strcat(cod,tmp);
				        tree->code=cod;	   }
		         else if(tmpt->left->code) {
                   	    char *cod = (char*)malloc(sizeof(tmp)+sizeof(c2)+1);
                        strcpy(cod,c2);
						strcat(cod,tmp);
				        tree->code=cod;	   }
				
				else{ 
				       char *cod = (char*)malloc(sizeof(tmp)+1);
                                        strcpy(cod,tmp);
				        tree->code=cod;	   }
                        if(tmpt->right) {
                               tmpt=tmpt->right;
                               scanBol(tmpt->left)  ;
                               sprintf(tmpt->var,"T%d",b); b++;
                               if(strcmp(tmpt->token,"&&")==0)
                                     sprintf (tmp,"%s=%s AND %s\n        ",tmpt->var,tree->var,tmpt->left->var);
                                else if(strcmp(tmpt->token,"||")==0);       
                                               sprintf (tmp,"%s=%s OR %s\n      ",tmpt->var,tree->var,tmpt->left->var);

                                 if(tmpt->left->code){
                                          char *newcode = (char*)malloc(sizeof(tree->code)+sizeof(tmp)+sizeof(tmpt->left->code)+1);
                                        strcpy(newcode,tree->code);
                                        strcat(newcode,tmpt->left->code);
                                        strcat(newcode,tmp);
                                         tree->code=newcode;
                                                     }        
                               strcpy(tree->var,tmpt->var);      

                           }/////end   if(tmpt->right)
		   }///end if tree->right

		 else         printf("eror to be here\n");
	 }////end if exp

if((strcmp(tree->token,"(")==0)||(strcmp(tree->token,"!(")==0)){
         

           ScanTree(tree->left->left);
           ScanTree(tree->left->right);
           sprintf(tree->var,"t%d",var); var++;
          if(tree->left->left->code)  { strcat(tmp,tree->left->left->code);     }
         if(tree->left->right->code)  { strcat(tmp,tree->left->right->code);  }
        sprintf(tmp,"if %s %s %s  goto lbl%d\n      %s=0\n      goto lbl%d\nlbl%d: %s=1\nlbl%d: "
				  ,tree->left->left->var,tree->left->token,tree->left->right->var,lbl,tree->var,lbl+1,lbl,tree->var,lbl+1);

                   lbl++;
                   lbl++;
       //  strcat(tmp,tree->left->left->var);
      // strcat(tmp,tree->left->token);
   //  strcat(tmp,tree->left->right->var);
     if(strcmp(tree->token,"!(")==0)
             {  
                strcat(tmp,"\n     "); 
                sprintf(s,"= NOT %s\n",tree->var);    
                sprintf(tree->var,"t%d",var); var++;     
               strcat(tmp,tree->var); 
               strcat(tmp,s);
                strcat(tmp,"\n     ")     ;                 

                                  }
      
           if(tree->right->left){

             tmpt=tree->right->left;
              scanBol(tmpt->left)  ;
                sprintf(tmpt->var,"T%d",b); b++;
                 if(tmpt->left->code)  { strcat(tmp,tmpt->left->code);  }
                 strcat(tmp,tmpt->var);
                 strcat(tmp,"=");
                 strcat(tmp,tree->var);                
                 if(strcmp(tmpt->token,"&&")==0) strcat(tmp," AND ");
                else if(strcmp(tmpt->token,"||")==0) strcat(tmp," OR ");
                else printf("error\n ");       
                 
               strcat(tmp,tmpt->left->var);  

               strcat(tmp,"\n      ")    ;
               strcpy(tree->var,tmpt->var);
                 }


            char *newcode = (char*)malloc(sizeof(tmp)+1);
               strcpy(newcode,tmp);
               tree->code=newcode;


}


 }////end of function

/////////////////////////////////////////////////////////////////////////////////////////

void scanIfelse(node* tree)
{



	char tmp[500]="";
	char l1[10];
	char l2[10];
	scanBol(tree->left);
	sprintf(l1,"lbl%d",lbl); lbl++;
	sprintf(l2,"lbl%d",lbl); lbl++;
	if(tree->left->code)strcat(tmp,tree->left->code);
	strcat(tmp,"if ");
	strcat(tmp,tree->left->var);
	strcat(tmp," goto ");
	strcat(tmp,l1);
	strcat(tmp,"\n      ");	
	if(tree->right->right){       
                    scanBody(tree->right->right->left);
                   if(tree->right->right->left->code)strcat(tmp,tree->right->right->left->code);

                     } 
	

	strcat(tmp,"goto ");
	strcat(tmp,l2);
	strcat(tmp,"\n");
	strcat(tmp,l1);
        strcat(tmp,": ");
////////////////////////////////////////////////////////////
	scanBody(tree->right->left);
	if(tree->right->left->code)strcat(tmp,tree->right->left->code);
/////////////////////////////////
	strcat(tmp,l2);
strcat(tmp,": ");

           char *newcode = (char*)malloc(sizeof(tmp)+1);
               strcpy(newcode,tmp);
               tree->code=newcode;



}
/////////////////////////////////////////////////////////////////////////////////////
void scanBody(node* tree)
{
	char tmp[400]="";
	node *tmpt;
	node *l;

	tmpt=tree;
if(strcmp(tmpt->token,"(empty)")!=0){
	while(strcmp(tmpt->token,"END BODY)")!=0){
	  l=tmpt->left;
          if(l->code){strcat(tmp,l->code);}
		
      
	tmpt=tmpt->right;
		
	}


	char *newcode = (char*)malloc(sizeof(tmp)+1);
               strcpy(newcode,tmp);
               tree->code=newcode;
     }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void scanwhile(node* tree)
{
	char tmp[500]="";
	char l1[10];
	char l2[10];
	scanBol(tree->left);
	sprintf(l1,"lbl%d",lbl); lbl++;
	sprintf(l2,"lbl%d",lbl); lbl++;
	strcat(tmp,l1);
	strcat(tmp,": ");
	if(tree->left->code)strcat(tmp,tree->left->code);
	strcat(tmp,"if Z ");
	strcat(tmp,tree->left->var);
	strcat(tmp," goto ");
	strcat(tmp,l2);
	strcat(tmp,"\n      ");	
	if(tree->right->left){scanBody(tree->right->left); 
           	if(tree->right->left->code)strcat(tmp,tree->right->left->code);}
	strcat(tmp,"      goto ");
	strcat(tmp,l1);
	strcat(tmp,"\n");
	strcat(tmp,l2);
        strcat(tmp,": ");

           char *newcode = (char*)malloc(sizeof(tmp)+1);
               strcpy(newcode,tmp);
               tree->code=newcode;
//printf("whilleee \n %s \n ******************* \n",tree->code);


	
}////end scanwhile
//////////////////////////////////////////////
void scanProcses(node *tree)
{
 Scope *tmp;
 Ptable *p;
 Ptable *MyP;
MyP=NULL;
 tmp=top;
int i;
int popSize=0;
char tmp2[200]="";
char s[2];


 while((MyP==NULL)&&tmp)
   {
     p=tmp->Ptop;
     while(p)
       {
          if((p->name)&& (strcmp(p->name,tree->left->token)==0)) MyP=p;
          p=p->next;
         } 
    tmp=tmp->next;
   }	
	
 if(MyP==NULL)  printf("error: %s not exsists  \n",tree->left->token);    


for(i=0;strlen(MyP->args_id[i])>0;i++)
{

	if(MyP->args_type[i]==0) popSize+=1;
 	if(MyP->args_type[i]==1) popSize+=1;
	if(MyP->args_type[i]==2) popSize+=4;
	if(MyP->args_type[i]==3) popSize+=8;		
}

if(tree->left->right)ScanArgs(tree->left->right); 
if(tree->left->right->code) strcat(tmp2,tree->left->right->code);  

  strcat(tmp2,"LCall ");   			  
  strcat(tmp2,tree->left->token); 
  strcat(tmp2,"\n     ");
  sprintf(s,"%d",popSize);
  strcat(tmp2,"PopParams "); 
  strcat(tmp2,s); 
strcat(tmp2,"\n    ");
 

	    char *cod = (char*)malloc(sizeof(tmp2)+1);
          strcpy(cod,tmp2);
          tree->code=cod;	   	



	
}
///////////////////////////////////////////////////////////////////////////////
void ScanArgs(node *tree)
{

	node *tmpt;
	tmpt=tree;
	char tmp[300]="     ";
	int t,t1,t2;
	
	while(tmpt)
	{
		SetScan(tmpt->left);
	    sprintf(tmpt->var,"t%d",var);
	    var++;       
        if(tmpt->left->code)		{
		  strcat(tmp,tmpt->left->code);
		   //strcat(tmp,"\n      ");
		 }
         strcat(tmp," ");
        strcat(tmp,tmpt->var);
        strcat(tmp,"=");
	strcat(tmp,tmpt->left->var);
	strcat(tmp,"\n      ");		
	strcat(tmp,"PushParm ");	
      	strcat(tmp,tmpt->var);
          if(tmpt->right) strcat(tmp,"\n      ");
		
	 tmpt=tmpt->right;	 
		
   	}///end while
	
    char *cod = (char*)malloc(sizeof(tmp)+1);
      strcpy(cod,tmp);                                           
       tree->code=cod;       

                           

}

//////////////////////////////////////
void scanBluk(node* tree)
{
	scanBody(tree->left);
	if(tree->left->code)tree->code=tree->left->code;

	
}
////////////////////////////////////////
void scanFunc(node* tree)
{
	char tmp[500];
	strcpy(tmp,tree->left->token);
	strcat(tmp,":\n     ");
     strcat(tmp,"BegimFunc");
	 strcat(tmp,"\n");
	scanBody(tree->left->right->right);
	if(tree->left->right->right->code)strcat(tmp,tree->left->right->right->code);
	SetScan(tree->left->right->left);
	if(tree->left->right->left->code)strcat(tmp,tree->left->right->left->code);
	strcat(tmp,"return ");
	strcat(tmp,tree->left->right->left->var);
	strcat(tmp,"\n     ");
        strcat(tmp,"EndFunc\n");

 char *cod = (char*)malloc(sizeof(tmp)+1);
      strcpy(cod,tmp);                                           
       tree->code=cod;    

//printf("function: \n%s \n",tree->code);

}

void scanProc(node* tree)
{
	char tmp[500];
	strcpy(tmp,tree->left->token);
	strcat(tmp,":\n     ");
     strcat(tmp,"BegimFunc");
	 strcat(tmp,"\n");
	scanBody(tree->left->right);
	if(tree->left->right->code)strcat(tmp,tree->left->right->code);
	strcat(tmp,"\n     ");
        strcat(tmp,"EndFunc\n");

 char *cod = (char*)malloc(sizeof(tmp)+1);
      strcpy(cod,tmp);                                           
       tree->code=cod;    

}

////////////////////////////////////////////////////
void printfCode(node *tree){
printf ("\nSTART\n");
tree=tree->left;
while(tree)
{
if(tree->left->code) printf("%s",tree->left->code);
tree=tree->right;

}

printf ("\nEND START\n");

}





