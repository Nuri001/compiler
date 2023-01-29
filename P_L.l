%{
#include <string.h>
%}
%%
"bool"                   { yylval.Val=strdup(yytext);   return BOOL;}
"char"            { yylval.Val=strdup(yytext);   return CHAR;}
"int"              { yylval.Val=strdup(yytext);  return INT;}
"real"           { yylval.Val=strdup(yytext);    return REAL;}
"string"         { yylval.Val=strdup(yytext);    return STRING;}
"int*"              { yylval.Val=strdup(yytext); return INTs;}
"char*"            { yylval.Val=strdup(yytext);  return CHARs;}
"real*"             { yylval.Val=strdup(yytext); return REALs;}
"if"                  { yylval.Val=strdup(yytext);  return IF;}
"else"            { yylval.Val=strdup(yytext);  return ELSE;}
"while"          { yylval.Val=strdup(yytext);   return WHILE;}
"var"             { yylval.Val=strdup(yytext);  return VAR;}
"func"           { yylval.Val=strdup(yytext);   return FUNC;}
"proc"           { yylval.Val=strdup(yytext);   return PROC;}
"return"        { yylval.Val=strdup(yytext);    return RETURN;}
"true"        { yylval.Val=strdup(yytext);    return TRUE;}
"false"        { yylval.Val=strdup(yytext);    return FALSE;}
"null"           { yylval.Val=strdup(yytext);   return NON;}
"&&"             { yylval.Val=strdup(yytext);   return AND;}
"||"               { yylval.Val=strdup(yytext); return OR;}
"!"               { yylval.Val=strdup(yytext);  return NOT;}
"=="             { yylval.Val=strdup(yytext);   return EQ;}
"!="              { yylval.Val=strdup(yytext);  return NEQ;}
">"               { yylval.Val=strdup(yytext);  return BIG;}
">="              { yylval.Val=strdup(yytext);  return BEQ;}
"<"                { yylval.Val=strdup(yytext); return LES;}
"<="              { yylval.Val=strdup(yytext);  return LEQ;}
[a-zA-Z][0-9"_"a-zA-Z]*                   { yylval.Val=strdup(yytext); return ID;}
[1-9][0-9]*|0                                       { yylval.Val=strdup(yytext);  return DCM;}
[0][x|X][0-9|A-F]+                                { yylval.Val=strdup(yytext);  return HEX;}
[\+|\-]?[0-9]*"."[0-9]+                                                {   yylval.Val=strdup(yytext);        return REALNUM;}
[\+|\-]?[0-9]*["."]?[0-9]*[e|E][\+|\-]?[0-9]+                                                         {yylval.Val=strdup(yytext); return REALENUM;}
["\""][a-z| \t|A-Z|0-9]*["\""]  { yylval.Val=strdup(yytext);  return STR;}
["\'"].["\'"]  { yylval.Val=strdup(yytext);  return CH;}
"/%"([^"%/"]|("%"|"/")[a-z0-9A-Z,.()&-+( \t)(\n)]+)*"%/"                ;
[ \t]                                       ;
\n                                                    yylineno++    ;
.                                                      return yytext[0];
%% 
