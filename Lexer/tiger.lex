type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentInc = ref 0
val stringAcc = ref ""
val posOffset = ref 0
val inString = ref false
  
fun eof() =
  let val pos = hd(!linePos)
  in (
      if !commentInc <> 0
      then (ErrorMsg.error (!posOffset) "EOF inside of comment")
      else ();
      if !inString = true
      then (ErrorMsg.error (!posOffset) "EOF inside of string")
      else ();
      Tokens.EOF(pos,pos))
  end

fun processDigit(yytext) =
  let
     val charCode = valOf(Int.fromString(yytext))
  in
     if charCode > 255
     then (ErrorMsg.error (!posOffset) ("ASCII code:" ^ yytext ^ "incorrect"))
     else ();
     String.str(chr(charCode))
  end

%% 
 %s COMMENT STRING STRING_ESCAPE STRING_SEQ CONTROL;
 DIGITS = [0-9]+;
 ID = [a-zA-Z][a-zA-Z0-9_]*;
 WS = [\ \t\f];
 CONTROL_CHARS = [A-Z | \] | \[ | \^  | \\ | \_];
%%

<INITIAL>\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>{WS} => (continue());

<INITIAL>","	  => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"."    => (Tokens.DOT (yypos, yypos + 1));
<INITIAL>":"    => (Tokens.COLON (yypos, yypos + 1));
<INITIAL>";"    => (Tokens.SEMICOLON (yypos, yypos + 1));
<INITIAL>"+"    => (Tokens.PLUS (yypos, yypos + 1));
<INITIAL>"-"    => (Tokens.MINUS (yypos, yypos + 1));
<INITIAL>"*"    => (Tokens.TIMES (yypos, yypos + 1));
<INITIAL>"/"    => (Tokens.DIVIDE (yypos, yypos + 1));
<INITIAL>"&"    => (Tokens.AND (yypos, yypos + 1));
<INITIAL>"|"    => (Tokens.OR (yypos, yypos + 1));
<INITIAL>"="    => (Tokens.EQ (yypos, yypos + 1));
<INITIAL>"<>"   => (Tokens.NEQ (yypos, yypos + 2));
<INITIAL>"<"    => (Tokens.LT (yypos, yypos + 1));
<INITIAL>"<="   => (Tokens.LE (yypos, yypos + 2));
<INITIAL>">"    => (Tokens.GT (yypos, yypos + 1));
<INITIAL>">="   => (Tokens.GE (yypos, yypos + 2));
<INITIAL>":="   => (Tokens.ASSIGN (yypos, yypos + 2));
<INITIAL>"["    => (Tokens.LBRACK (yypos, yypos + 1));
<INITIAL>"]"    => (Tokens.RBRACK (yypos, yypos + 1));
<INITIAL>"{"    => (Tokens.LBRACE (yypos, yypos + 1));
<INITIAL>"}"    => (Tokens.RBRACE (yypos, yypos + 1));
<INITIAL>"("    => (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL>")"    => (Tokens.RPAREN (yypos, yypos + 1));

<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>type   => (Tokens.TYPE (yypos, yypos + 4));
<INITIAL>array  => (Tokens.ARRAY (yypos, yypos + 5));
<INITIAL>of     => (Tokens.OF   (yypos, yypos + 2));
<INITIAL>function   => (Tokens.FUNCTION (yypos, yypos + 8));
<INITIAL>for    => (Tokens.FOR  (yypos, yypos + 3));
<INITIAL>while  => (Tokens.WHILE (yypos, yypos + 5));
<INITIAL>do     => (Tokens.DO   (yypos, yypos + 2));
<INITIAL>break  => (Tokens.BREAK (yypos, yypos + 5));
<INITIAL>if     => (Tokens.IF   (yypos, yypos + 2));
<INITIAL>then   => (Tokens.THEN (yypos, yypos + 4));
<INITIAL>else   => (Tokens.ELSE (yypos, yypos + 4));
<INITIAL>let    => (Tokens.LET  (yypos, yypos + 3));
<INITIAL>in     => (Tokens.IN   (yypos, yypos + 2));
<INITIAL>end    => (Tokens.END  (yypos, yypos + 3));
<INITIAL>nil    => (Tokens.NIL  (yypos, yypos + 3));
<INITIAL>to     => (Tokens.TO   (yypos, yypos + 2));

<INITIAL>{ID} => (Tokens.ID (yytext, yypos, yypos + size yytext));
<INITIAL>{DIGITS} => (Tokens.INT (valOf(Int.fromString(yytext)), yypos, yypos + size yytext));

<INITIAL>"/*"   => (YYBEGIN COMMENT; commentInc := !commentInc + 1; posOffset:= yypos; continue ());
<COMMENT>"/*"   => (commentInc := !commentInc + 1; posOffset := yypos; continue ());
<COMMENT>"*/"   => (commentInc := !commentInc - 1;if !commentInc = 0 then (YYBEGIN INITIAL; posOffset := 0) else ();continue ());
<COMMENT>\n     => (lineNum := !lineNum+1;linePos := yypos :: !linePos; posOffset:= yypos; continue());
<COMMENT>.      => (posOffset := yypos ;continue ());

<INITIAL>"\""     => (YYBEGIN STRING; inString := true; stringAcc := ""; posOffset:= yypos; continue());
<STRING>"\""      => (YYBEGIN INITIAL; inString := false; posOffset:= 0 ;Tokens.STRING(!stringAcc, yypos, yypos + size (!stringAcc) ));
<STRING>"\\"      => (YYBEGIN STRING_ESCAPE; posOffset:= yypos; continue());
<STRING>\n        => (ErrorMsg.error yypos ("illegal character " ^ yytext);posOffset:= yypos; continue());
<STRING>.         => (stringAcc := !stringAcc ^ yytext; posOffset:= yypos; continue());
<STRING_ESCAPE>"n" => (YYBEGIN STRING; stringAcc := !stringAcc ^ "\n";posOffset:= yypos; continue());
<STRING_ESCAPE>"t" => (YYBEGIN STRING; stringAcc := !stringAcc ^ "\t";posOffset:= yypos; continue());
<STRING_ESCAPE>"\^" => (YYBEGIN CONTROL;posOffset:= yypos; continue());
<CONTROL>{CONTROL_CHARS} => (YYBEGIN STRING;stringAcc := !stringAcc ^ String.str(chr(ord(String.sub(yytext, 1)) - 64));posOffset:= yypos; continue());
<CONTROL>. => (ErrorMsg.error yypos ("illegal control character " ^ yytext); YYBEGIN STRING; continue());
<STRING_ESCAPE>{DIGITS}{DIGITS}{DIGITS} => (YYBEGIN STRING;stringAcc := !stringAcc ^ processDigit(yytext) ;posOffset:= yypos; continue());
<STRING_ESCAPE>"\"" => (YYBEGIN STRING; stringAcc := !stringAcc ^ "\"";posOffset:= yypos; continue());
<STRING_ESCAPE>"\\" => (YYBEGIN STRING; stringAcc := !stringAcc ^ "\\";posOffset:= yypos; continue());
<STRING_ESCAPE>{WS} => (YYBEGIN STRING_SEQ;posOffset:= yypos;  continue());
<STRING_ESCAPE>\n   => (YYBEGIN STRING_SEQ;lineNum := !lineNum+1; linePos := yypos :: !linePos;posOffset:= yypos;  continue());
<STRING_ESCAPE>.    => (ErrorMsg.error yypos ("illegal character " ^ yytext);posOffset:= yypos;  continue());
<STRING_SEQ>{WS}    => (posOffset:= yypos; continue());
<STRING_SEQ>\n      => (lineNum := !lineNum+1; linePos := yypos :: !linePos;posOffset:= yypos;  continue());
<STRING_SEQ>"\\"    => (YYBEGIN STRING;posOffset:= yypos;  continue());
<STRING_SEQ>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

