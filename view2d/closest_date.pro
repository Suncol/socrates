function closest_date, did, ref=ref, lbound=lbound, ubound=ubound

common axes             ; in : depth.coord
common axes2            ; in : depth2.coord, depth2.dim
common axes3            ; in : depth3.coord, depth3.dim
common arch_info        ; in : arch, arch2, arch3

ref = KEYWORD_SET(ref)
If ref then Begin
   arch0 = arch3    &   depth0 = depth3
 EndIf Else Begin
   arch0 = arch2    &   depth0 = depth2 
EndElse

cdid = -1
idx = where( arch0.list.depth(0:depth0.dim-1) eq arch.list.depth(did), nidx )
If nidx gt 0 then Begin
   cdid = idx(0)   &   GOTO, FOUND
EndIf

lbound = KEYWORD_SET(lbound)
ubound = KEYWORD_SET(ubound)
If lbound then Begin
   For id = 0, depth0.dim-1 Do $
      If (depth0.coord(id) gt depth.coord(did)) and    $
            (depth0.coord(id) lt depth.coord(depth.dim-1) ) then Begin
         cdid = id   &   GOTO, FOUND
      EndIf
 EndIf Else If ubound then Begin
   For id = depth0.dim-1, 0, -1 Do $
      If (depth0.coord(id) lt depth.coord(did)) and    $
            (depth0.coord(id) gt depth.coord(0) ) then Begin
         cdid = id   &   GOTO, FOUND
      EndIf
 EndIf Else Begin
   For id = 0, depth0.dim-2 Do     $
      If ABS( depth0.coord(id) - depth.coord(did)) lt                    $
            ABS( depth0.coord(id+1) - depth.coord(did)) then Begin
         cdid = id   &   GOTO, FOUND
      EndIf
   cdid = depth0.dim-1
EndElse

FOUND:
If cdid eq -1 then Begin
   If lbound then cdid = 0 Else If ubound then cdid = depth0.dim-1
EndIf

return, cdid
end
