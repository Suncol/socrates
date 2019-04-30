pro autoscaler, force=force

common comscale
common axes
common plots_info
common dataranges
common select_id
common last_fields
common last_com

if (last.action ne 'form_titles') then reset_titles = 1

get_minmax, mins, maxs
supermins = mins - 0.1*ABS(mins)
scale.vector(0) = supermins < scale.vector(0)

if not autolevSW and not KEYWORD_SET(force) then GOTO, SET_STRMINMAX

if (last.plot2d eq 'depth-x') then If(zmolat.units eq 'Dobson Units') then Begin
      mins = 200.   &   inter=20.   &   GOTO, SET_VECTOR
EndIf

if (mins eq 0.) then mins = 1.e-30
if (maxs eq 0.) then maxs = 2.e-30

If mins gt maxs then Begin
   scale.interv = 'INTERVAL ERROR'
   return
 EndIf Else If mins eq maxs then Begin
   scale.vector = FINDGEN( scale.n )
   scale.interv = '??'   &   scale.mins = '??'   &   scale.maxs = '??'
   return
EndIf

inter = (maxs - mins) / ( scale.n - 1 )
SET_VECTOR:
scale.vector(1) = mins
For i = 2, scale.n-1 Do scale.vector(i) = scale.vector(i-1) + inter

SET_STRINGS:
scale.interv = STRTRIM( STRING(inter,FORMAT='(G10.3)'), 2 )

SET_STRMINMAX:
scale.mins = '??'   &   scale.maxs = '??'
For is = 0, scale.n-1 Do Begin
   If scale.vector(is) gt mins then Begin
      scale.mins = STRTRIM( STRING(scale.vector(is),FORMAT='(G10.3)'), 2 )
      GOTO, SET_STRMAX
   EndIf
EndFor
SET_STRMAX:
For is = scale.n-1, 1, -1 Do Begin
   If scale.vector(is) lt maxs then Begin
      scale.maxs = STRTRIM( STRING(scale.vector(is),FORMAT='(G10.3)'), 2 )
      return
   EndIf
EndFor

end

