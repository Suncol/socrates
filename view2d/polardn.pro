pro polardn, depth, latpn

pi = 3.141592654
latpn = fltarr(depth.dim)
depth_coord = depth.coord

If depth.type eq 'days0' then nt = depth.dim-1    $
 Else If depth.coord(depth.dim) gt 1000.*365. then Begin
   nt = 0
   depth_coord(0) = depth.coord(depth.dim)
EndIf Else return

For id = 0, nt Do Begin
   daynum = ((depth_coord(id) - 1) MOD 365) + 1 - 172   ; Julian day - 172
   declind = 23.45 * COS( 2.*pi/365.*daynum )
   If( ABS(daynum) gt 91 ) then Begin                ; northern polar night
      latpn(id) = 90. + declind
    EndIf Else Begin                                 ; southern polar night
      latpn(id) = -90. + declind
   EndElse
EndFor

if depth.dim gt 1 and nt eq 0 then latpn(1:depth.dim-1) = latpn(0)
    
end
