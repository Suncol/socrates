pro month_lat_view, zmonthlat, _EXTRA=e

common axes		; in : depth, yp
common comscale		; in : scale
common select_id	; in : jp
common dataranges	; in : idp0, idp1, ip0, ip1
common plots_info	; in : show, reset_titles ; inout : titles
common formats		; in : Dfmt
common last_com		; in : last

WIDGET_CONTROL, /HOURGLASS

If reset_titles then Begin
   set_titles, zmonthlat, /molat
   If (N_ELEMENTS(yp) ne 0) and (zmonthlat.units ne 'molec/cm2')   	$
          and (zmonthlat.units ne 'Dobson Units') and show.title then 	$
      titles(1) = titles(1) + ' ; '+yp.name+' : '+string(yp.coord(jp))+' '+yp.units
EndIf
set_xymargins, junk, my_ymargin

values = zmonthlat.matrix(idp0:idp1,ip0:ip1)
               
If (ip0 eq 0) and (ip1 eq x.dim-1) then Begin
   my_ytick = ['90S','60S','30S','Eq','30N','60N','90N']
   contour, values, depth.coord(idp0:idp1), x.coord(ip0:ip1),		$
         level=scale.vector, ymargin=my_ymargin,			$
         title=titles(0)+'!C'+titles(1), 				$
         subtitle=titles(2)+'!C'+titles(3), /follow, 			$
         xrange=[depth.coord(idp0),depth.coord(idp1)], xstyle=1,	$
         yrange=[-90,90], ystyle=1, yticks=6, ytickname=my_ytick,	$
         ytitle=x.name, yminor=3,   					$
         xtitle=depth.label.name, 					$
         xticks=depth.label.nb, 					$
         xtickname=depth.label.txt(0:depth.label.nb), 			$
	 xtickv=depth.label.coord(0:depth.label.nb), _EXTRA=e
 EndIf Else Begin
   contour, values, depth.coord(idp0:idp1), x.coord(ip0:ip1),		$
         level=scale.vector, ymargin=my_ymargin,			$
         title=titles(0)+'!C'+titles(1), 				$
         subtitle=titles(2)+'!C'+titles(3), /follow, 			$
         xrange=[depth.coord(idp0),depth.coord(idp1)], xstyle=1,	$
         ytitle=x.name, yrange=[x.coord(ip0),x.coord(ip1)], ystyle=1,   $
         xtitle=depth.label.name, 					$
         xticks=depth.label.nb, 					$
         xtickname=depth.label.txt(0:depth.label.nb), 			$
	 xtickv=depth.label.coord(0:depth.label.nb), _EXTRA=e
EndElse
         
if (MIN(values) eq MAX(values)) then $
   xyouts, .2, .5, /normal, CHARSIZE=3,			 		   $
       'CONSTANT VALUE IS '+STRTRIM(MIN(values),2)

return
end
