pro MESOTVALS, _EXTRA=e

common axes             ; in : x, y, depth, yp
common last_fields      ; in : zl, z2, Z3
common dataranges       ; in : ip0, ip1, jp0, jp1
common select_id        ; in : jp
common plots_info       ; in : screen, logsw, show, reset_titles ; inout : titles
common arch_info        ; in : arch2, arch3, arch.latpn
common axes2            ; in : x2, depth2
common axes3            ; in : x2, depth3
common hardware         ; in : colorSW, l2color, l3color
common formats          ; in : Yfmt

WIDGET_CONTROL, /HOURGLASS
my_thick = 3
If (!D.NAME eq screen) then Begin
   window, 1, xsize=800, ysize=600, title='Temperature minimum plot'
   my_thick = 2
EndIf

If reset_titles and show.title then Begin
   set_titles, zl, /plot1d
   if jp0 gt 22 then titles(0) = titles(0) + ' ; Mesopause values'     $
    else titles(0) = titles(0) + ' ; Tropo/Mesopause values'
EndIf
set_xymargins, junk, my_ymargin, /plot1d

value = fltarr( x.dim )
for l = 0, x.dim-1 do value(l) = MIN( zl.matrix(l,jp0:jp1) )

x_vals = x.coord(*)
all_lat = 0B   &   my_xrange = [x.coord(ip0),x.coord(ip1)]
If (ip0 eq 0) and (ip1 eq x.dim-1) and                     $
     ((x.name eq 'latitudes') or (x.name eq 'latitude')) then Begin
   all_lat = 1B
   my_xrange = [ -90., 90. ]
   my_tick = ['90S','60S','30S','Eq','30N','60N','90N']
EndIf
my_yrange = [ MIN(value), MAX(value) ]

If z2.compOK then Begin
   z2_vals = fltarr( x2.dim )
   for l = 0, x2.dim-1 do z2_vals(l) = MIN( z2.matrix(l,jp0:jp1) )
   my_yrange = [ MIN(z2_vals) < my_yrange(0), MAX(z2_vals) > my_yrange(1) ]
EndIf
If z3.compOK then Begin
   z3_vals = fltarr( x3.dim )
   for l = 0, x3.dim-1 do z3_vals(l) = MIN( z3.matrix(l,jp0:jp1) )
   my_yrange = [ MIN(z3_vals) < my_yrange(0), MAX(z3_vals) > my_yrange(1) ]
EndIf

If valSW then my_yrange = valsrange

If (ip1-ip0 lt 30) then my_psym = -1 Else my_psym = 0
thistring = up_subscript( zl )
if jp0 gt 22 then my_ytitle = 'Mesopause ' else my_ytitle = 'Tropo/Mesopause '
my_ytitle = my_ytitle + thistring(0)+' ('+thistring(1)+')'

If all_lat then Begin
   plot, x_vals, value, title=titles(0)+'!C'+titles(1),                 $
     xrange=my_xrange, xstyle=1, xmargin=[16,4],                        $
     xtitle=x.name+' ('+x.units+')', ylog=logsw,                        $
     yrange=my_yrange, ystyle=valSW, ytitle=my_ytitle,                  $
     ymargin=my_ymargin, psym=my_psym, thick=2,                         $
     ticklen=.02+.98*show.grid,                                         $
     xgridstyle=show.grid, ygridstyle=show.grid,                        $
     xticks=6, xtickname=my_tick, xminor=3
 EndIf Else Begin
   plot, x_vals, value, title=titles(0)+'!C'+titles(1),                 $
     xrange=my_xrange, xstyle=1, xmargin=[16,4],                        $
     xtitle=x.name+' ('+x.units+')', ylog=logsw,                        $
     yrange=my_yrange, ystyle=valSW, ytitle=my_ytitle,                  $
     ymargin=my_ymargin, psym=my_psym, thick=2,                         $
     ticklen=.02+.98*show.grid,                                         $
     xgridstyle=show.grid, ygridstyle=show.grid
EndElse

If z2.compOK  then Begin
   If colorsw then oplot, x2.coord, z2_vals, color=l2color, thick=my_thick  $
    Else oplot, x2.coord, z2_vals, linestyle=2, thick=my_thick
EndIf
If z3.compOK  then Begin
   If colorsw then oplot, x3.coord, z3_vals, color=l3color, thick=my_thick     $
    Else oplot, x3.coord, z3_vals, linestyle=3, thick=my_thick
EndIf

If show.legends then print_legends, titles

lpn = arch.latpn(zl.did)
If show.pdn and not show.grid then Begin
   if (x.coord(ip0) lt lpn) and (x.coord(ip1) gt lpn) then      $
      plots, [lpn,lpn], my_yrange, linestyle=2 ; polar night limit
   if (x.coord(ip0) lt -lpn) and (x.coord(ip1) gt -lpn) then    $
      plots, [-lpn,-lpn], my_yrange, linestyle=1 ; polar day limit
EndIf

end
