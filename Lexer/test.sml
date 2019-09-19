val linePos = [123];
fun eof() =
    let val pos = hd(!linePos)
    in
        if !commentInc <> 0
        then (ErrorMsg.error pos ("eof inside of comment"))
        else ();
        if !inString = true
        then (ErrorMsg.error pos ("eof inside of string"))
        else ();
        Tokens.EOF(pos,pos)
    end
