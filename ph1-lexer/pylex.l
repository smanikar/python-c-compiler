%{
/*************************************************
 * Srikanth Manikarnike
 * U0706564
 *************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define END 65535

unsigned int line_no = 1 ;
unsigned int col_no = 0 ;
unsigned int paran_cnt = 0;

unsigned int indent_stack[32] ; /* Ought to be enough, right? ;) */
unsigned int* stack = indent_stack ;
char str[256];

void change_indent(unsigned int width) {

  /* Unchanged: */
  if (width == *stack) 
    return ;

  if (width == END){
	  if(stack == indent_stack)
		  return;
  }
  
  /* New indent: */
  if (width > *stack) {
    stack++ ;
    *stack = width ;
    printf("(INDENT)\n") ;
    return ;
  } 

  /* Unindented: */
  while (width < *stack) {
    stack-- ;
    printf("(DEDENT)\n");
  }

  if(width != *stack){
	  printf("(ERROR \"mis-indented program\")\n");
	  exit(1);
  }

  if (width > *stack) {
    stack++ ;
    *stack = width ;
  }
}

void error_handle(char *s){
	switch(*s){
		case'?':
		case'\\':
		case'@':
		case '$':
		case '`':
		case '!': printf("(ERROR \"unknown char: '%c'\")\n",*s);
				  break;
		default:  printf("(ERROR \"unterminated string literal at EOF\")\n"); 
				  exit(1);
	}
}

void str_to_rkt(char *s){
	int i, slen=strlen(s),j,istriple=0;
	strcpy(str, s);
	char temp[4];
	/* raw string */
	if('r' == str[0] || 'R' == str[0]){
		strcpy(str,&str[1]);	
		slen = strlen(str);
	}
	/*byte literal */
	if(('b' == str[0] || 'B' == str[0]) && ('r' == str[0] || 'R' == str[0])){
		strcpy(str,&str[2]);	
		slen = strlen(str);
	}
	/*triple quoted*/
	if(((str[1] == '\'') &&( str[0] == '\'') && (str[2] == '\'')) ||
	    ((str[1] == '\"') &&( str[0] == '\"') && (str[2] == '\"'))){
		slen = strlen(str);
		if(slen < 6) return;
		istriple = 1;
		strncpy(&(str[1]), &(str[3]), slen-6); 
		strcpy(&(str[slen-6+1]),"\"");
		slen = strlen(str);
	}
	/* replacing quotes */
	if('\''==str[0] && '\''==str[slen-1])
		str[0] = str[slen-1] ='"';
	/* complex numbers */
	if('j'==str[slen-1]){
		/*seg faulting*/
		str[slen-1] = 'i';
		for(i=slen; i>=0; i--)
			str[i+1] = str[i];
		str[0] = '+';
	} 
	for(i=0; str[i]!='\0'; i++){
		/* escaped characters */
		if(str[i] == '\\' ){
			switch(str[i+1]){
				case '\n':
					strncpy(&(str[i]),&(str[i+2]), slen-i-1);
					slen = strlen(str);
					if(i)
						i -= 1;
					break;
				case '\r':
					strncpy(&(str[i]),&(str[i+3]), slen-i-2);
					slen = strlen(str);
					if(i)
					  	i -= 1;
					break;
				case '\\':
					i+=1;
					break;
				case '\'':
				case '\"':
				case 'a':
				case 'b':
				case 'f':
				case 'n':
				case 'r':
				case 't':
				case 'v':
					break;
				default: 
					for(j=slen; j >= i+1 ; j--)
						str[j+1] = str[j];
					str[i+1] = '\\';
					i+=1;
					break;
			}
		}else if (str[i] == '\'' || str[i] == '\"') {
			/*escaping quotes*/
			slen = strlen(str);
			if(i == 0 || i == slen-1)
				continue;
			if(str[i-1] == '\\' )
				continue;
			for(j=slen; j >= i ; j--)
				str[j+1] = str[j];
			str[i] = '\\';
			i+=1;
		} else if( istriple){
			slen = strlen(str);
			switch(str[i]){
				case '\n':
					for(j=slen; j >= i+1 ; j--)
						str[j+1] = str[j];
					str[i] = '\\';
					str[i+1] = 'n';
					i+=1;
					break;
					//strncpy(&(str[i]),&(str[i+2]), slen-i-1);
				case '\r':
					//strncpy(&(str[i]),&(str[i+3]), slen-i-2);
					for(j=slen; j >= i+1 ; j--)
						str[j+1] = str[j];
					str[i] = '\\';
					str[i+1] = 'r';
					i+=1;
					break;
				case '\\':
					i+=1;
					break;
				default:
					break;
			}
		}
		/* remove newlines */
		else if(str[i] == '\n'){
			printf("(ERROR \"newline in string\")\n");
			exit(1);
		}
	}
}

%}

nl             \r|\n|\r\n
nline          \r\n
ws             [ \t]
id 			   [A-Za-z_][A-Za-z_0-9]*

open 			 "("|"{"|"["
close	         ")"|"}"|"]"
lit 		     {floatnumber}|{integer}|{imagnumber}|{bytesliteral}|{stringliteral}

stringliteral    {stringprefix}?({shortstring}|{longstring})
stringprefix     [rR]
shortstring     ([']{shortstringitemq}*['])|(["]{shortstringitemqq}*["])
longstring       ([']['][']{longstringitem}*['][']['])|(["]["]["]{longstringitem}*["]["]["])
shortstringitemq  {shortstringcharq}|{stringescapeseq}
shortstringitemqq  {shortstringcharqq}|{stringescapeseq}
longstringitem     {longstringchar}|{stringescapeseq}
shortstringcharq   [^'\r\n\\]|[^\'\\r\\n\\]
shortstringcharqq  [^"\r\n\\]|[^\"\\r\\n\\]
longstringchar   [^\\]
stringescapeseq  "\\"[\x00-\x7F]

bytesliteral    {bytesprefix}({shortbytes}|{longbytes})
bytesprefix     ["b"]|["B"]|["br"]|["Br"]|["bR"]|["BR"]
shortbytes      ([']{shortbytesitemq}*['])|(["]{shortbytesitemqq}*["])
longbytes       ([']['][']{longbytesitem}*['][']['])|(["]["]["]{longbytesitem}*["]["]["])
shortbytesitemq  {shortbytescharq}|{bytesescapeseq}
shortbytesitemqq  {shortbytescharqq}|{bytesescapeseq}
longbytesitem   {longbyteschar}|{bytesescapeseq}
shortbytescharq  [^\'"]*
shortbytescharqq  [^\"]*
longbyteschar   [^\\]*
bytesescapeseq  ("\\")[\x00-\x7F]*

integer        {decimalinteger}|{octinteger}|{hexinteger}|{bininteger}
decimalinteger {nonzerodigit}{digit}*|[0]+
nonzerodigit   [1-9]
digit          [0-9]
octinteger     [0][("o"|"O")]{octdigit}+
hexinteger     [0][("x"|"X")]{hexdigit}+
bininteger     [0][("b"|"B")]{bindigit}+
octdigit       [0-7]
hexdigit       {digit}|[a-f]|[A-F]
bindigit       [0-1]

floatnumber   {pointfloat}|{exponentfloat}
pointfloat    {intpart}?{fraction}|{intpart}["."]
exponentfloat ({intpart}|{pointfloat}){exponent}
intpart       {digit}+
fraction      ["."]{digit}+
exponent      ("e"|"E")("+"|"-")?{digit}+

imagnumber 	  ({floatnumber}|{intpart})("j"|"J")

op 			  {arithops}|{logicalops}|{bitwiseops}
arithops	  "+"|"-"|"*"|"/"|"%"|"**"|"//"
logicalops    "<"|">"|"<="|">="|"=="|"!="
bitwiseops	  "<<"|">>"|"&"|"|"|"^"|"~"

delim 		  {braces}|{punc}|{assign}
braces 		  "("|")"|"["|"]"|"{"|"}"
punc		  ","|":"|"."|";"|"@"|"="
assign		  "+="|"-="|"*="|"/="|"//="|"%="|"&="|"|="|"^="|">>="|"<<="|"**="


punct 		  {op}|{delim}

keyword       {kwone}|{kwtwo}|{kwthree}|{kwfour}|{kwfive}|{kwsix}
kwone 	      "False"|"class"|"finally"|"is"|"return"
kwtwo	   	  "None"|"continue"|"for"|"lambda"|"try"
kwthree		  "True"|"def"|"from"|"nonlocal"|"while"
kwfour		  "and"|"del"|"global"|"not"|"with"
kwfive		  "as"|"elif"|"if"|"or"|"yield"|"assert"
kwsix		  "else"|"import"|"pass"|"break"|"except"|"in"|"raise"

%x INPARAN INSTRING

%%
<INITIAL>^({punct}|{keyword}|{lit}|{id}) {change_indent(0); REJECT;}	
<INITIAL>{open}                          {printf("(PUNCT \"%s\")\n",yytext);
                                          BEGIN(INPARAN); paran_cnt++;}
<INITIAL,INPARAN>^{ws}*{nl}              {}
<INITIAL>^{ws}* 	                     {change_indent(strlen(yytext));}
<INPARAN>{close}                         {printf("(PUNCT \"%s\")\n",yytext); 
                                          BEGIN(0); paran_cnt--;}
<INITIAL>{nl}		                     {printf("(NEWLINE)\n");}
<INITIAL>^{ws}+[#][^\r\n]*[\r\n]+        {}
<INITIAL>{ws}+[#][^\r\n]*[\r\n]+         {printf("(NEWLINE)\n");}
<INITIAL,INPARAN>("\\"){ws}*[\r\n]       {}
<INITIAL,INPARAN>{lit}                   {str_to_rkt(yytext);
                                          printf("(LIT %s)\n",str);} 
<INITIAL,INPARAN><<EOF>>                 {printf("(ENDMARKER)\n"); 
	                                      yyterminate();}
<INITIAL,INPARAN>{keyword}               {printf("(KEYWORD %s)\n",yytext);}
<INITIAL,INPARAN>{punct}                 {printf("(PUNCT \"%s\")\n",yytext);}
<INITIAL,INPARAN>{id}                    {printf("(ID \"%s\")\n",yytext);}
<INITIAL,INPARAN>['][^\']*([\r\n])       {printf("(ERROR \"newline in string\")\n");
	                                      yyterminate();}
<INITIAL,INPARAN>(["][^\"]*([\r\n]))     {printf("(ERROR \"newline in string\")\n"); yyterminate();}
<INITIAL,INPARAN>(["]["]["][^\"\r\n]*[\r\n]+) {printf("(ERROR \"unterminated string literal at EOF\")\n"); yyterminate();}
<INITIAL,INPARAN>(['][']['][^\'\r\n]*[\r\n]+) {printf("(ERROR \"unterminated string literal at EOF\")\n"); yyterminate();}
<INITIAL,INPARAN>{ws}                    {}
<INITIAL,INPARAN>^[#][^\r\n]*[\r\n]*     {}
<INITIAL,INPARAN>{ws}*[#][^\r\n]*[\r\n]* {}

<INPARAN>^{ws}*{nl}     {}
<INPARAN>^{ws}*         {}
<INPARAN>{nl}		    {}
<INITIAL,INPARAN>.   		 		    { error_handle(yytext); }
%%

int yywrap(void) {
  /* Called at EOF. */
  change_indent(0);
  return 1 ;
}


int main(int argc, char* argv[]) {

  (void)argc; (void)argv ;
  (void)yyunput;

  indent_stack[0] = 0 ;

  yylex() ; 

  return 0 ;
}

/*************************************************
 * Srikanth Manikarnike
 * U0706564
 *************************************************/
