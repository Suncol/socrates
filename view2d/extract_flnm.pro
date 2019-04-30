function extract_flnm, path_flnm

len = STRLEN(path_flnm)
For i = 0, len-1 Do Begin
   char = STRMID( path_flnm, len-1-i, 1 )
   if (char eq '/' or char eq '\') then GOTO, CONTINUE
EndFor

CONTINUE:
flnm = STRMID( path_flnm, len-i, i )

return, flnm
end
