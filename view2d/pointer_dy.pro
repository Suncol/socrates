PRO pointer_dy, i, j, dc, yc, arch2=arch2

common axes
common axes2

If not KEYWORD_SET(arch2) then Begin
   depthdim = depth.dim   &   ydim = y.dim
   depthcoord = depth.coord   &   ycoord = y.coord
 EndIf Else Begin
   depthdim = depth2.dim   &   ydim = y2.dim
   depthcoord = depth2.coord   &   ycoord = y2.coord
EndElse

   
dist = 1e10
For iprov = 0, depthdim-1 Do Begin                      ; Look for closest day
   If (ABS(depthcoord(iprov)-dc) lt dist) then Begin
      dist = ABS(depthcoord(iprov)-dc)
      i = iprov
   EndIf
EndFor
dist = 1e10
For jprov = 0, ydim-1 Do Begin                  ; Look for closest y 
   If (ABS(ycoord(jprov)-yc) lt dist) then Begin
      dist = ABS(ycoord(jprov)-yc)
      j = jprov
   EndIf
EndFor

If not KEYWORD_SET(arch2) then Begin
   dc = depthcoord(i)    &    yc = ycoord(j)
EndIf

return
end
