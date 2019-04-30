PRO make_depthlist, depth, depthlist

;------ Convert the depth coordinates to mm/dd/yyyy julian dates -----------

common formats      ; in: Dfmt

depthlist = STRARR( depth.dim+1 )

If depth.type eq 'days0' then Begin
   For i = 0, depth.dim-1 Do Begin
      days0date, depth.coord(i), date
      depthlist(i) = STRING( date.month, date.day, date.year, FORMAT=Dfmt )
   EndFor
 EndIf Else Begin
   depthlist(0:depth.dim-1) = STRTRIM( depth.coord(0:depth.dim-1), 2 )
   If depth.coord(depth.dim) gt 1000.*365. then Begin
      days0date, depth.coord(depth.dim), date
      depthlist(depth.dim) = STRING( date.month, date.day, date.year, FORMAT=Dfmt )
   EndIf
EndElse

end
