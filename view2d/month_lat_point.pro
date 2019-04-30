PRO month_lat_point, id, il, dc, lc, printval=printval

common last_fields
common axes
common widgets_info

If (N_ELEMENTS(dc) eq 0) then cursor, dc, lc, /wait, /data

dist = 1e10
For idprov = 0, depth.dim-1 Do Begin 			; Look for closest days0
   If (ABS(depth.coord(idprov)-dc) lt dist) then Begin
      dist = ABS(depth.coord(idprov)-dc)
      id = idprov
   EndIf
EndFor
dist = 1e10
For ilprov = 0, x.dim-1 Do Begin 			; Look for closest lat
   If (ABS(x.coord(ilprov)-lc) lt dist) then Begin
      dist = ABS(x.coord(ilprov)-lc)
      il = ilprov
   EndIf
EndFor
   
dc = depth.coord(id)    &    lc = x.coord(il)

FmtStr = '(A3,"/",I2,"/",I4)'
If (KEYWORD_SET(printval)) then Begin
   idm1 = (id-1>0)   &    idp1 = (id+1<(depth.dim-1))
   ilm1 = (il-1>0)   &    ilp1 = (il+1<(x.dim-1))
   day_low = .5 * (depth.coord(idm1)+dc)  &  day_up = .5 * (dc+depth.coord(idp1))
   lat_low = .5 * (x.coord(ilm1)+lc)   &    lat_up = .5 * (lc+x.coord(ilp1))
   plots, [day_low,day_up], [lat_low,lat_low]
   plots, [day_low,day_up], [lat_up,lat_up]
   plots, [day_low,day_low], [lat_low,lat_up]
   plots, [day_up,day_up], [lat_low,lat_up]
   days0date, dc, date
   msg(0) = 'Date : '+string(date.month,date.day,date.year,FORMAT=FmtStr)+  $
	' ; latitude : '+STRING(lc,Format='(f4.0)')+' '+x.units
   msg(1) = zmolat.name+' = '+STRING(zmolat.matrix(id,il))+' '+zmolat.units
   WIDGET_CONTROL, msg_id, SET_VALUE=msg
EndIf

return
end
