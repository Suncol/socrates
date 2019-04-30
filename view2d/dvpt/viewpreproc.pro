pro viewpreproc

common hardware		; in : colorSW
common last_com		; in : last.plot2d, last.action
common last_fields	; in : zl, zmolat
common arch_info	; in : arch.latpn
common plots_info	; in : ncols_avail, screen, show.pdn
common select_id	; in : depth_id, pointed
common dataranges	; in : ip0, ip1, jp0, jp1
common axes		; in : x.coord, y.coord
common widgets_info	; in : draw_id
common comscale		; in : scale

If colorSW then Begin
   mycolors = indgen(30) * (ncols_avail+100) / 30 + 10
   If (!D.NAME eq 'PS') then Begin				; for ps files
      mycolors(0:17) = indgen(18) * ncols_avail / 20 + 15
      mycolors(18:29) = mycolors(17)
   EndIf
EndIf

If (!D.NAME eq screen) then Begin
   WIDGET_CONTROL, draw_id, GET_VALUE=draw_win
   wset, draw_win ; draw_win should be 0
EndIf

CASE last.plot2d OF

   'depth-x' : BEGIN
                 if (zmolat.units eq 'Dobson Units' and       $
                              last.action ne 'manscaler') then autoscaler
		 If colorSW then Begin
		    month_lat_view, zmolat, /fill, c_color=mycolors
		    month_lat_view, zmolat, /overplot, c_color=1
		 EndIf else month_lat_view, zmolat
	       END
	       
   'x-y'     : BEGIN
		 If colorSW then Begin				; pour faire une echelle
		    viewer, zl, /fill, c_color=mycolors		; de couleurs : TVSCL, A, ..
		    viewer, zl, /overplot, c_color=1		;ou A(color_nb,data_nb)  
		 EndIf else viewer, zl
		 lpn = arch.latpn(depth_id)
		 if show.pdn and (x.coord(ip0) lt lpn) and				$
                                		 (x.coord(ip1) gt lpn) then	$
		    plots, [lpn,lpn], [y.coord(jp0),y.coord(jp1)], linestyle=2 ; polar night limit
		 if show.pdn and (x.coord(ip0) lt -lpn) and 			$
		                                 (x.coord(ip1) gt -lpn) then	$
		    plots, [-lpn,-lpn], [y.coord(jp0),y.coord(jp1)], linestyle=1 ; polar day limit
	       END
	       
 ELSE : MESSAGE, 'Last 2d plot unknown', /TRACEBACK

ENDCASE

if pointed and (!D.NAME eq screen) then draw_point

return
end
   
