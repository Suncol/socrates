pro horslicer, _EXTRA=e

common axes             ; in : x, y, depth, yp
common last_fields      ; in : zl, z2, z3
common dataranges       ; in : ip0, ip1, jp0, jp1
common select_id        ; in : jp
common plots_info       ; in : screen, logsw, show, reset_titles ; inout : titles
common arch_info        ; in : arch2, arch3, arch.latpn
common axes2            ; in : x2, depth2
common axes3            ; in : x3, depth3
common hardware         ; in : colorSW, l2color, l3color
common formats          ; in : Yfmt

WIDGET_CONTROL, /HOURGLASS
my_thick = 3
If (!D.NAME eq screen) then Begin
   window, 1, xsize=800, ysize=600, title='Horizontal plot'
   my_thick = 2
EndIf

If reset_titles and show.title then Begin
   set_titles, zl, /plot1d
   yc = y.coord(jp)
   titles(0) = titles(0) + ' ; '+ y.name+' : '+string(yc,FORMAT=Yfmt)+' '+y.units
   If (N_ELEMENTS(yp) ne 0 and yp.dim gt 0) then $
      titles(1) = titles(1) + yp.name+' : '+string(yp.coord(jp))+' '+yp.units
EndIf
set_xymargins, junk, my_ymargin, /plot1d

value = zl.matrix(*,jp)
all_lat = 0B   &   my_xrange = [x.coord(ip0),x.coord(ip1)]
If (ip0 eq 0) and (ip1 eq x.dim-1) and                     $
     ((x.name eq 'latitudes') or (x.name eq 'latitude')) then Begin
   all_lat = 1B
   my_xrange = [ -90., 90. ]
   my_tick = ['90S','60S','30S','Eq','30N','60N','90N']
EndIf

minmissval = -1.e20   &   maxmissval = 1.e22
my_yrange = fltarr(2)

nidx = 0
If zl.badvals then Begin
   idx = where ( zl.valOK(*,jp) eq 0, nidx )
   if nidx gt 0 then value( idx ) = maxmissval
EndIf
my_yrange(0) = MIN(value(ip0:ip1))
if nidx gt 0 then value( idx ) = minmissval
my_yrange(1) = MAX(value(ip0:ip1))
junk = WHERE( value(ip0:ip1) le 0. and value(ip0:ip1) ne minmissval, count)
mylogsw = logsw   &   If logsw and (count gt 0) then mylogsw = 0

If z2.compOK then Begin
   z2_vals = z2.matrix(*,jp)
   if z2.badvals then z2_vals( where ( z2.valOK(*,jp) eq 0 ) ) = maxmissval
   my_yrange(0) = MIN(z2_vals(ip0:ip1)) < my_yrange(0)
   if z2.badvals then z2_vals( where ( z2.valOK(*,jp) eq 0 ) ) = minmissval
   my_yrange(1) = MAX(z2_vals(ip0:ip1)) > my_yrange(1)
EndIf
If z3.compOK then Begin
   z3_vals = z3.matrix(*,jp)
   if z3.badvals then z3_vals( where ( z3.valOK(*,jp) eq 0 ) ) = maxmissval
   my_yrange(0) = MIN(z3_vals(ip0:ip1)) < my_yrange(0)
   if z3.badvals then z3_vals( where ( z3.valOK(*,jp) eq 0 ) ) = minmissval
   my_yrange(1) = MAX(z3_vals(ip0:ip1)) > my_yrange(1)
EndIf
If valSW then my_yrange = valsrange

If (ip1-ip0 lt 30) then my_psym = -1 Else my_psym = 0
thistring = up_subscript( zl )

index = where(zl.valOK(0:x.dim-1,jp) eq 1)

If all_lat then Begin
   plot, x.coord(index), value(index), title=titles(0)+'!C'+titles(1),  $
     xrange=my_xrange, xstyle=1, xmargin=[16,4],                        $
     xtitle=x.name+' ('+x.units+')', ylog=mylogsw,                      $
     yrange=my_yrange, ytitle=thistring(0)+' ('+thistring(1)+')',       $
     ymargin=my_ymargin, ystyle=valSW, psym=my_psym, thick=2,           $
     ticklen=.02+.98*show.grid,                                         $
     xgridstyle=show.grid, ygridstyle=show.grid,                        $
     xticks=6, xtickname=my_tick, xminor=3, _EXTRA = e                 
 EndIf Else Begin
   plot, x.coord(index), value(index), title=titles(0)+'!C'+titles(1),  $
     xrange=my_xrange, xstyle=1, xmargin=[16,4],                        $
     xtitle=x.name+' ('+x.units+')', ylog=mylogsw,                      $
     yrange=my_yrange, ytitle=thistring(0)+' ('+thistring(1)+')',       $
     ymargin=my_ymargin, ystyle=valSW, psym=my_psym, thick=2,           $
     ticklen=.02+.98*show.grid,                                         $
     xgridstyle=show.grid, ygridstyle=show.grid, _EXTRA = e
EndElse

If z2.compOK  then Begin
   index = where(z2.valOK(0:x2.dim-1,jp) eq 1)
   If index(0) ne -1 then Begin
      If colorsw then oplot, x2.coord(index), z2_vals(index), color=l2color,   $
                                         thick=my_thick                        $
                 Else oplot, x2.coord(index), z2_vals(index), linestyle=2,     $
                                         thick=my_thick
   EndIf
EndIf
If z3.compOK  then Begin
   index = where(z3.valOK(0:x3.dim-1,jp) eq 1)
   If index(0) ne -1 then Begin
      If colorsw then oplot, x3.coord(index), z3_vals(index), color=l3color,   $
                                         thick=my_thick                        $
                  Else oplot, x3.coord(index), z3_vals(index), linestyle=3,    $
                                         thick=my_thick
   EndIf
EndIf

If show.legends then print_legends, titles

lpn = arch.latpn(zl.did)
If show.pdn and not show.grid then Begin
   if (x.coord(ip0) lt lpn) and (x.coord(ip1) gt lpn) then      $
      plots, [lpn,lpn], my_yrange, linestyle=2 ; polar night limit
   if (x.coord(ip0) lt -lpn) and (x.coord(ip1) gt -lpn) then    $
      plots, [-lpn,-lpn], my_yrange, linestyle=1 ; polar day limit
EndIf

return
end
