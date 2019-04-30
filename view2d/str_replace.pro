function str_replace, mystring, oldchar, newchar

lentot = STRLEN(mystring)
if (lentot eq 0) then return, ''
char = STRARR(lentot)   &   newstring = ''
For i = 0, lentot-1 Do Begin
    char(i) = STRMID(mystring, i, 1 )
    if (char(i) eq oldchar) then char(i) = newchar
    newstring = newstring + char(i)
EndFor

return, newstring
end

