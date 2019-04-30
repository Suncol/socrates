pro get_minmax, minv, maxv

common dataranges
common select_id
common last_fields
common last_com

If (last.plot2d eq 'depth-x') then Begin
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

minv = MIN( z.matrix(i0:i1,j0:j1) )
maxv = MAX( z.matrix(i0:i1,j0:j1) )
If z.badvals then Begin
   z.matrix( where ( z.valOK eq 0 ) ) = 1.1*ABS(maxv)
   minv = MIN( z.matrix(i0:i1,j0:j1) )
   z.matrix( where ( z.valOK eq 0 ) ) = -1.1*ABS(minv)
   maxv = MAX( z.matrix(i0:i1,j0:j1) )
EndIf

end
