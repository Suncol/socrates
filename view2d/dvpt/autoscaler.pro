pro autoscaler

common comscale
common axes
common plots_info
common dataranges
common select_id
common last_fields
common last_com

if (last.action ne 'form_titles') then reset_titles = 1

If (last.plot2d eq 'depth-x') then Begin
   If (zmolat.units eq 'Dobson Units') then Begin
      scale.mins = 200.   &   scale.maxs = 800.  &   scale.interv='20'
      scale.vector = scale.mins + (scale.maxs-scale.mins)*FINDGEN(30)/30.
      return
   EndIf
   i0 = idp0   &    i1 = idp1
   j0 = ip0    &    j1 = ip1
   z = zmolat
 EndIf Else If (last.plot2d eq 'depth-y') then Begin
   i0 = idp0   &    i1 = idp1
   j0 = jp0    &    j1 = jp1
   z = zmolev
 EndIf Else Begin
   i0 = ip0    &    i1 = ip1
   j0 = jp0    &    j1 = jp1
   z = zl
EndElse

scale.mins = MIN( z.matrix(i0:i1,j0:j1) )
scale.maxs = MAX( z.matrix(i0:i1,j0:j1) )
If z.badvals then Begin
   z.matrix( where ( z.valOK eq 0 ) ) = 1.1*ABS(scale.maxs)
   scale.mins = MIN( z.matrix(i0:i1,j0:j1) )
   z.matrix( where ( z.valOK eq 0 ) ) = -1.1*ABS(scale.mins)
   scale.maxs = MAX( z.matrix(i0:i1,j0:j1) )
EndIf
if (scale.mins eq 0.) then scale.mins=1.e-30
scale.vector(0) = scale.mins
If scale.mins gt scale.maxs then Begin
   scale.vector = scale.mins + FINDGEN(scale.n)
   scale.interv = 'INTERVAL ERROR'
   return
EndIf
If( ABS((scale.maxs-scale.mins)/scale.mins) lt 1.e-6 ) then Begin
   scale.maxs = 1.e30            ; constant values (cstval) flag
   scale.vector = FINDGEN(30)
   return
EndIf
if (scale.maxs eq 0.) then scale.maxs=2.e-30
inter = (scale.maxs - scale.mins) / (scale.n-1)
For i = 1, scale.n-1 Do scale.vector(i) = scale.vector(i-1) + inter
scale.interv = strtrim(inter,2)

return
end

