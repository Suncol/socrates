pro month_val2, _EXTRA=e

common axes		; in : x, y, depth
common axes2            ; in : depth2
common axes3            ; in : depth3
common last_fields	; in : zmolev, zmolev2, zmolev3
common select_id	; in : ip, jp
common plots_info	; in : logsw, reset_titles, show, valSW, valsrange
                        ; inout : titles
common dataranges	; in : idp0, idp1, idp2_0, idp2_1, idp3_0, idp3_1
common formats		; in : Xfmt, Yfmt, Dfmt
common hardware		; in : colorSW, l2color, l3color

WIDGET_CONTROL, /HOURGLASS
if (!D.NAME eq screen) then window, 1, title='Fixed '+x.name+' plot'

If reset_titles and show.title then Begin
   set_titles, zmolev, /plot1d, /molev
   yc = y.coord(jp) 
   titles(0) = titles(0) + ' ; '+ y.name+' : '+string(yc,FORMAT=Xfmt)+' '+y.units
EndIf
set_xymargins, junk, my_ymargin, /plot1d

value = zmolev.matrix(idp0:idp1,jp)
if valsw then my_yrange = valsrange else my_yrange = [ MIN(value), MAX(value) ]
junk = WHERE(value le 0., count)
mylogsw = logsw
If logsw and (count gt 0) then mylogsw = 0

If z2.compOK then Begin
   value2 = zmolev2.matrix(idp2_0:idp2_1,jp)
   if not valSW then   $
      my_yrange = [ MIN(value2) < my_yrange(0), MAX(value2) > my_yrange(1) ]
EndIf
If z3.compOK then Begin
   value3 = zmolev3.matrix(idp3_0:idp3_1,jp)
   if not valSW then   $
      my_yrange = [ MIN(value3) < my_yrange(0), MAX(value3) > my_yrange(1) ]
EndIf

depth_vals = depth.coord(idp0:idp1)
my_xrange = [depth.coord(idp0), depth.coord(idp1)]
thistring = up_subscript( zmolev )

plot, depth_vals, value, title=titles(0)+'!C'+titles(1),		$
         xrange=my_xrange, xstyle=1, xmargin=[16,4], 			$
         ylog=mylogsw, yrange=my_yrange, ymargin=my_ymargin, 	 	$
         ytitle=thistring(0)+' ('+thistring(1)+')', 			$
         xtitle=depth.label.name, 					$
         xticks=depth.label.nb, 					$
         xtickname=depth.label.txt(0:depth.label.nb), 			$
	 xtickv=depth.label.coord(0:depth.label.nb), 			$
         ticklen=.02+.98*show.grid, 					$
         xgridstyle=show.grid, ygridstyle=show.grid, _EXTRA = e
         
If z2.compOK  then Begin
   depth_vals2 = depth2.coord(idp2_0:idp2_1)
   If colorsw then oplot, depth_vals2, value2, color=l2color 		$
    Else  oplot, depth_vals2, value2, linestyle=2
EndIf
If z3.compOK  then Begin
   depth_vals3 = depth3.coord(idp3_0:idp3_1)
   If colorsw then oplot, depth_vals3, value3, color=l3color 		$
    Else  oplot, depth_vals3, value3, linestyle=3
EndIf

If show.legends then print_legends, titles

return
end
