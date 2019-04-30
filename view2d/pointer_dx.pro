PRO pointer_dx, i, j, dc, xc, arch2=arch2

common axes
common axes2

If not KEYWORD_SET(arch2) then Begin
   depthdim = depth.dim   &   xdim = x.dim
   depthcoord = depth.coord   &   xcoord = x.coord
 EndIf Else Begin
   depthdim = depth2.dim   &   xdim = x2.dim
   depthcoord = depth2.coord   &   xcoord = x2.coord
EndElse

; print, 'dc: ',dc,' ; xc: ',xc
i = -1   &   dist = 1e10
For iprov = 0, depthdim-1 Do Begin                      ; Look for closest day
   If (ABS(depthcoord(iprov)-dc) lt dist) then Begin
      dist = ABS(depthcoord(iprov)-dc)
      i = iprov
   EndIf
EndFor
j = -1   &   dist = 1e10
For jprov = 0, xdim-1 Do Begin                  ; Look for closest x
   If (ABS(xcoord(jprov)-xc) lt dist) then Begin
      dist = ABS(xcoord(jprov)-xc)
      j = jprov
   EndIf
EndFor

; print, 'i: ',i,' ; j: ',j
If not KEYWORD_SET(arch2) and (i ne -1) and (j ne -1) then Begin
   dc = depthcoord(i)    &    xc = xcoord(j)
EndIf

return
end
