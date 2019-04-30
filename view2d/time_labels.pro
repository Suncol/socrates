pro time_labels, depth

; NOTICE: this routine is very inefficient for long-time archives.
; It must be rewritten so that days0date is not called in the loops below

common dataranges
common label_widgets
common arch_info

if (not arch.OK.molat and not arch.OK.molev) then return

depth.label.nb = 0
depth.label.txt = strarr(30)   &   depth.label.coord = fltarr(30)

If depth.type ne 'days0' then Begin
   depth.label.name = depth.name + ' (' + depth.units + ')'
   If depth.coord(idp0) eq 0. and depth.coord(idp1) eq 24. then Begin
      depth.label.nb = 9
      depth.label.coord = [ 0., 3., 6., 9., 12., 15., 18., 21., 24. ]
      depth.label.txt = STRING( depth.label.coord, FORMAT='(i2)' )
   EndIf
   return
EndIf

; msg(0) = 'Recalculating the labels for time...'
; WIDGET_CONTROL, msg_id, SET_VALUE=msg

if (depth.dim gt 1) and (idp1 eq depth.dim-1) and               $
      ((depth.coord(idp1)-depth.coord(idp1-1) gt 3650.)) then   $
   idp1 = depth.dim-2

; Compute fullnb, fullcoord and fulltxt: one element per month

 &  fulltxt = strarr(12*depth.dim) 
   &  fullcoord = fltarr(12*depth.dim) 
days0date, depth.coord(idp0), date
fullcoord(0) = depth.coord(idp0)
fulltxt(0) = date.month
fullnb = 0
For days0 = depth.coord(idp0), depth.coord(idp1) do Begin
   days0date, days0, date
   If (fulltxt(fullnb) ne date.month) then Begin
      fullnb = fullnb+1
      fullcoord(fullnb) = days0
      fulltxt(fullnb) = date.month
   EndIf
EndFor

;print, 'idp0 : ',idp0,'  ;  idp1 : ',idp1,'  ;  fullnb : ',fullnb
;print, 'fullcoord(0) : ',fullcoord(0),'  ;  fullcoord(fullnb) : ',fullcoord(fullnb)
;print, 'fulltxt(0) : ',fulltxt(0),'  ;  fulltxt(fullnb) : ',fulltxt(fullnb)

If (fullnb eq 0) then Begin

  arch.OK.molat = 0B   &   arch.OK.molev = 0B   &   return
 
 EndIf Else If (fullnb lt 20) then Begin

   depth.label.name = 'MONTH'
   depth.label.nb = fullnb
   depth.label.coord(0:fullnb) = fullcoord(0:fullnb)
   depth.label.txt(0:fullnb) = fulltxt(0:fullnb)
   if depth.label.coord(1)-depth.label.coord(0) le 10. then   $
      depth.label.txt(0) = ' '
   if depth.label.coord(fullnb)-depth.label.coord(fullnb-1) le 10. then   $
      depth.label.txt(fullnb) = ' '

 EndIf Else If (fullnb lt 70) then Begin

; If simulation longer than 2.5 years (fullnb > 29) but shorter than 7.5yr
; create depth.label.coord and depth.label.txt to show only every 3 months (Jan, Apr, Jul, Oct)
   depth.label.name = 'MONTH'
   depth.label.nb = 0
   For n = 0, fullnb Do Begin
       If (fulltxt(n) eq 'Jan') then Begin
          days0date, fullcoord(n), date
          depth.label.coord(depth.label.nb) = fullcoord(n)
          depth.label.txt(depth.label.nb) = 'Jan'+strtrim(date.year,2)
          depth.label.nb = depth.label.nb + 1
       EndIf
       If (fulltxt(n) eq 'Apr') or (fulltxt(n) eq 'Jul') 	$
             or (fulltxt(n) eq 'Oct') then Begin
          depth.label.coord(depth.label.nb) = fullcoord(n)
          depth.label.txt(depth.label.nb) = fulltxt(n)
          depth.label.nb = depth.label.nb + 1
       EndIf
   EndFor
   depth.label.nb = depth.label.nb - 1

EndIf Else If (fullnb lt 200) then Begin

; If simulation longer than ?? years (fullnb > 70) create depth.label.coord
; and depth.label.txt to show only year numbers
   depth.label.name = 'YEAR'
   depth.label.nb = 0
   For n = 0, fullnb Do Begin
       If (fulltxt(n) eq 'Jan') then Begin
          days0date, fullcoord(n), date
          depth.label.coord(depth.label.nb) = fullcoord(n)
          depth.label.txt(depth.label.nb) = strtrim(date.year,2)
          depth.label.nb = depth.label.nb + 1
       EndIf
   EndFor
   depth.label.nb = depth.label.nb - 1

EndIf Else Begin

; If simulation longer than ?? years (fullnb > 200) create depth.label.coord
; and depth.label.txt to show only the year every 5 years
   depth.label.name = 'YEAR'
   depth.label.coord(0) = depth.coord(idp0)
   days0date,depth.coord(idp0), date
   depth.label.txt(0) = strtrim(date.year,2)
   depth.label.nb = 1
   For n = 0, fullnb Do Begin
       If (fulltxt(n) eq 'Jan' and $
           fullcoord(n)-depth.label.coord(depth.label.nb-1) gt 5.*364.) then Begin
          days0date, fullcoord(n), date
          depth.label.coord(depth.label.nb) = fullcoord(n)
          depth.label.txt(depth.label.nb) = strtrim(date.year,2)
          depth.label.nb = depth.label.nb + 1
       EndIf
   EndFor
   depth.label.nb = depth.label.nb - 1
EndElse

return
end
