pro month_lev_view, zmonthlev, _EXTRA=e

common axes		; in : depth, x 
common comscale		; in : scale
common select_id	; in : jp
common dataranges	; in : idp0, idp1, jp0, jp1
common plots_info	; in : show, reset_titles ; inout : titles
common formats		; in : Dfmt

WIDGET_CONTROL, /HOURGLASS

If reset_titles then Begin
   set_titles, zmonthlev, /molev
;   If (N_ELEMENTS(x) ne 0) and (zmonthlev.units ne 'molec/cm2')   	$
;          and (zmonthlev.units ne 'Dobson Units') and show.title then 	$
;   titles(1) = titles(1)
EndIf
set_xymargins, junk, my_ymargin

values = zmonthlev.matrix(idp0:idp1,jp0:jp1)
               
If (jp0 eq 0) and (jp1 eq y.dim-1) then Begin
   my_ytick = [' 0 ',' 20',' 40',' 60',' 80','100','120']
   contour, values, depth.coord(idp0:idp1), y.coord(jp0:jp1),		$
         level=scale.vector, ymargin=my_ymargin,			$
         title=titles(0)+'!C'+titles(1), 				$
         subtitle=titles(2)+'!C'+titles(3), /follow, 			$
         xrange=[depth.coord(idp0),depth.coord(idp1)], xstyle=1,	$
         yrange=[0,120], ystyle=1, yticks=6, ytickname=my_ytick,	$
         ytitle=y.name, yminor=3,   					$
         xtitle=depth.label.name, 					$
         xticks=depth.label.nb, 					$
         xtickname=depth.label.txt(0:depth.label.nb), 			$
	 xtickv=depth.label.coord(0:depth.label.nb), _EXTRA=e
 EndIf Else Begin
   contour, values, depth.coord(idp0:idp1), y.coord(jp0:jp1),		$
         level=scale.vector, ymargin=my_ymargin,			$
         title=titles(0)+'!C'+titles(1), 				$
         subtitle=titles(2)+'!C'+titles(3), /follow, 			$
         xrange=[depth.coord(idp0),depth.coord(idp1)], xstyle=1,	$
         ytitle=y.name, yrange=[y.coord(jp0),y.coord(jp1)], ystyle=1,   $
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
