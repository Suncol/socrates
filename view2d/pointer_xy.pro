PRO pointer_xy, i, j, xc, yc                     ; i, j: output
                                                 ; xc, yc : input and output

common axes            ; in : x, y, ygeo
common plots_info      ; in : show.ygeo

i = -1   &   dist = 1e10
For iprov = 0, x.dim-1 Do Begin 			; Look for closest x
   If (ABS(x.coord(iprov)-xc) lt dist) then Begin
      dist = ABS(x.coord(iprov)-xc)
      i = iprov
   EndIf
EndFor
if i eq -1 then return

j = -1   &   dist = 1e10
If show.ygeo then Begin
   For jprov = 0, y.dim-1 Do Begin 			; Look for closest ygeo
      If (ABS(ygeo.matrix(i,jprov)-yc) lt dist) then Begin
         dist = ABS(ygeo.matrix(i,jprov)-yc)
         j = jprov
      EndIf
   EndFor
 EndIf Else Begin
   For jprov = 0, y.dim-1 Do Begin 			; Look for closest y
      If (ABS(y.coord(jprov)-yc) lt dist) then Begin
         dist = ABS(y.coord(jprov)-yc)
         j = jprov
      EndIf
   EndFor
EndElse
if j eq -1 then return
   
xc = x.coord(i)
If show.ygeo then yc = ygeo.matrix(i,j) Else yc = y.coord(j)

END
