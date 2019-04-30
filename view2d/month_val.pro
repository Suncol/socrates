pro month_val, _EXTRA=e

common axes             ; in : x, y, depth
common axes2            ; in : depth2
common axes3            ; in : depth3
common last_fields      ; in : zmolat, zmolat2, zmolat3
common select_id        ; in : ip, jp
common plots_info       ; in : logsw, reset_titles, show screen, valsrange, valSW
                        ; inout : titles
common dataranges       ; in : idp0, idp1, idp2_0, idp2_1, idp3_0, idp3_1
common formats          ; in : Xfmt, Yfmt, Dfmt
common hardware         ; in : colorSW, l2color, l3color

WIDGET_CONTROL, /HOURGLASS
my_thick = 3
If (!D.NAME eq screen) then Begin
   window, 1, xsize=1000, ysize=600, title='Fixed '+x.name+' plot'
   my_thick = 2
EndIf

If reset_titles and show.title then Begin
   set_titles, zmolat, /plot1d, /molat   &   titles(1) = ''
   xc = x.coord(ip)
   if (zmolat.loc(1) ne '') then titles(1) = titles(1) +  zmolat.loc(1) + ' ; '
   titles(1) = titles(1) + x.name+' : '+string(xc,FORMAT=Xfmt)+ ' '+x.units
EndIf
set_xymargins, junk, my_ymargin, /plot1d
if show.linfit then my_ymargin(0) = 13

minmissval = -1.e20   &   maxmissval = 1.e22   &   my_yrange = fltarr(2)

value = zmolat.matrix(0:depth.dim-1,ip)   &   nidx = 0
If zmolat.badvals then Begin
   idx = where ( zmolat.valOK(0:depth.dim-1,ip) eq 0B, nidx )
   if nidx gt 0 then value( idx ) = maxmissval
EndIf
my_yrange(0) = MIN(value)
if nidx gt 0 then value( idx ) = minmissval
my_yrange(1) = MAX(value)

zm2OK = 0B
if z2.compOK then zm2OK = (depth2.coord(idp2_0) lt depth.coord(idp1)) and  $
                          (depth2.coord(idp2_1) gt depth.coord(idp0))
If zm2OK then Begin
   value2 = zmolat2.matrix(0:depth2.dim-1,ip)   &   nidx = 0
   if zmolat2.badvals then Begin
      idx = where ( zmolat2.valOK(0:depth2.dim-1,ip) eq 0, nidx )
      if nidx gt 0 then value2( idx ) = maxmissval
   EndIf
   my_yrange(0) = MIN(value2) < my_yrange(0)
   if nidx gt 0 then value2( idx ) = minmissval
   my_yrange(1) = MAX(value2) > my_yrange(1)
EndIf

zm3OK = 0B
if z3.compOK then zm3OK = (depth3.coord(idp3_0) lt depth.coord(idp1)) and  $
                      (depth3.coord(idp3_1) gt depth.coord(idp0))
If zm3OK then Begin
   value3 = zmolat3.matrix(0:depth3.dim-1,ip)   &   nidx = 0
   If zmolat3.badvals then Begin
      idx = where ( zmolat3.valOK(0:depth3.dim-1,ip) eq 0, nidx )
      if nidx gt 0 then value3( idx ) = maxmissval
   EndIf
   my_yrange(0) = MIN(value3) < my_yrange(0)
   if nidx gt 0 then value3( idx ) = minmissval
   my_yrange(1) = MAX(value3) > my_yrange(1)
EndIf

If valSW then my_yrange = valsrange 

mylogsw = logsw   &   If my_yrange(0) le 0 then mylogsw = 0

my_xrange = [depth.coord(idp0), depth.coord(idp1)]
thistring = up_subscript( zmolat )

index = indgen(idp1-idp0+1)
if zmolat.badvals then index = where(zmolat.valOK(idp0:idp1,ip) eq 1B)
If index(0) eq -1 then Begin
   junk = DIALOG_MESSAGE( 'All values in the time series are missing', /ERROR )
   return
EndIf

plot, depth.coord(0:depth.dim-1), value, title=titles(0)+'!C'+titles(1), $
         xrange=my_xrange, xstyle=1, xmargin=[16,4],                     $
         ylog=mylogsw, yrange=my_yrange, ymargin=my_ymargin, ystyle=1,   $
         thick=2,                                                        $
         ytitle=thistring(0)+' ('+thistring(1)+')',                      $
         xtitle=depth.label.name, xticks=depth.label.nb,                 $
         xtickname=depth.label.txt(0:depth.label.nb),                    $
         xtickv=depth.label.coord(0:depth.label.nb),                     $
         ticklen=.02+.98*show.grid,                                      $
         xgridstyle=show.grid, ygridstyle=show.grid, _EXTRA = e

If zm2OK  then Begin
   index = indgen(idp2_1-idp2_0+1)
   if zmolat2.badvals then index = where(zmolat2.valOK(idp2_0:idp2_1,ip) eq 1B)
   If index(0) ne -1 and depth2.dim gt 1 then Begin
      If colorsw then oplot, depth2.coord(0:depth2.dim-1), value2,       $
                                   color=l2color, thick=my_thick         $
                  Else  oplot, depth2.coord(0:depth2.dim-1), value2,     $
                                   linestyle=2, thick=my_thick
   EndIf
EndIf
If zm3OK  then Begin
   index = indgen(idp3_1-idp3_0+1)
   if zmolat3.badvals then index = where(zmolat3.valOK(idp3_0:idp3_1,ip) eq 1B)
   If index(0) ne -1 and depth3.dim gt 1 then Begin
      If colorsw then oplot, depth3.coord(0:depth3.dim-1), value3,       $
                                      color=l3color,  thick=my_thick     $
                  Else  oplot, depth3.coord(0:depth3.dim-1), value3,     $
                                    linestyle=3, thick=my_thick
   EndIf
EndIf

If show.linfit then Begin
;   linpar = LINFIT( depth.coord(0:depth.dim-1), value )
;   linvals = linpar(1) * depth.coord + linpar(0)
;   oplot, depth.coord, linvals, thick = 4
; Code below to linfit only winter and summer solstices when *only* the 
;     2 solstices and 2 equinoxes were archived, beginning at 28 Sep.
   month = ['Sep','Dec','Mar','Jun']
   For i = 0, 3 do Begin           ; i=1->Dec, i=3->June
      didx = 4*INDGEN(depth.dim/4)+i
      linpar = LINFIT( depth.coord(didx), value(didx) )
      linvals = linpar(1) * depth.coord + linpar(0)
      oplot, depth.coord(0:depth.dim-1), linvals(0:depth.dim-1), thick=4, $
             color=25+25*i
      trend = STRTRIM( STRING( linpar(1)*365,FORMAT='(G10.3)'), 2 )
      plots, [0.7,0.75],[.15-i*0.03,.15-i*0.03],thick=4,       $
                 color=25+25*i,/normal
      xyouts, 0.755, .145-i*0.03, month(i)+' linfit trend= '    $
                  +trend+' ('+zmolat.units+'/year)', /normal
   EndFor
EndIf

If show.legends then print_legends, titles

end
