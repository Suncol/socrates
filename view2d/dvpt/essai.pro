; This is a trial widget to test a zoom box operated by mouse
; left-button click on first corner of window, and drag opposite corner


PRO essai_ev, event

common wid_info, draw_id, col, old_graph_fct
common moving_box, xPress, yPress, px, py
common dataranges, pointed, ip0, jp0, ip1, jp1

WIDGET_CONTROL, event.id, GET_UVALUE = action		;find the user value
							;of the widget where
CASE action OF
     'done'   :  BEGIN
                    WIDGET_CONTROL, event.top, /DESTROY
                 END
     'unzoom' :  BEGIN
		    ip0 = 0   &   ip1 = 19   &   jp0 = 0   &   jp1 = 19
                    viewer, /fill, c_color=10*findgen(20)
		    viewer, /overplot, c_color=1
                 END
     'pointer' : BEGIN 
     		    If event.press then Begin
     		       xPress = event.x   &   yPress = event.y   &   nx=0   &   ny=0
		       px = [xPress, xPress + nx, xPress + nx, xPress, xPress]
		       py = [yPress, yPress, yPress + ny, yPress + ny, yPress]
		       device, GET_GRAPHICS_FUNCTION=old_graph_fct, $   ;Set xor graphics fct so that
		               SET_GRAPHICS_FUNCTION=6			;line overplotting returns
		               						;to previous pixel value
     		       WIDGET_CONTROL, draw_id, /DRAW_MOTION_EVENTS
     		    EndIf
     		    If (event.type eq 2) then Begin			;cursor is moving...
		       plots, px, py, col=col, /dev, thick=1, lines=0	;UnDraw previous box
     		       nx = event.x - xPress   &   ny = event.y - yPress
		       px = [xPress, xPress + nx, xPress + nx, xPress, xPress]
		       py = [yPress, yPress, yPress + ny, yPress + ny, yPress]
		       plots, px, py, col=col, /dev, thick=1, lines=0	;Re-Draw box
     		    EndIf
     		    If event.release then Begin
		       plots, px, py, col=col, /dev, thick=1, lines=0	;UnDraw previous box
     		       xRelease = event.x   &   yRelease = event.y
     		       point_or_zoom, xPress, yPress, xRelease, yRelease
     		       WIDGET_CONTROL, draw_id, DRAW_MOTION_EVENTS=0
     		    EndIf
     		 END
     		 
ENDCASE
CONTINUE :

END ;============= end of selecter event handling routine task =============

pro viewer, _EXTRA=e

common math_info, matrix, x, y
common dataranges, pointed, ip0, jp0, ip1, jp1

contour, matrix(ip0:ip1,jp0:jp1), x(ip0:ip1), y(jp0:jp1), 	$
   xrange=[x(ip0),x(ip1)], yrange=[y(jp0),y(jp1)],		$
   xstyle=1, ystyle=1,						$
   levels=findgen(20), /follow, _EXTRA=e

return
end ;============= end of viewing routine ==================================

PRO pointer, i, j, xc, yc

common math_info, matrix, x, y

dist = 1e10
For iprov = 0, 19 Do Begin 			; Look for closest x
   If (ABS(x(iprov)-xc) lt dist) then Begin
      dist = ABS(x(iprov)-xc)
      i = iprov
   EndIf
EndFor
dist = 1e10
For jprov = 0, 19 Do Begin 			; Look for closest y
   If (ABS(y(jprov)-yc) lt dist) then Begin
      dist = ABS(y(jprov)-yc)
      j = jprov
   EndIf
EndFor
   
xc = x(i)    &    yc = y(j)

return
end;============= end of pointing routine ==================================

pro point_or_zoom, xPress, yPress, xRelease, yRelease

common dataranges, pointed, ip0, jp0, ip1, jp1
common math_info, matrix, x, y
common wid_info, draw_id, col, old_graph_fct

if (xPress gt xRelease) then Begin
 prov = xPress   &   xPress = xRelease   &   xRelease = prov
endif
if (yPress gt yRelease) then Begin
 prov = yPress   &   yPress = yRelease   &   yRelease = prov
endif
corner0 = CONVERT_COORD(xPress, yPress, /DEVICE, /TO_DATA)
corner1 = CONVERT_COORD(xRelease, yRelease, /DEVICE, /TO_DATA)
x0 = corner0(0)   &   y0 = corner0(1)
x1 = corner1(0)   &   y1 = corner1(1)
pointer, ip0, jp0, x0, y0
pointer, ip1, jp1, x1, y1

If ((ip1-ip0) le 2) and ((jp1-jp0) le 2) then Begin
   pointed = 1
   i_low = (ip0-1>0)   &    i_up = (ip0+1<(20-1))
   j_low = (jp0-1>0)   &    j_up = (jp0+1<(20-1))
   x_low = .5 * (x(i_low)+x0)   &   x_up = .5 * (x0+x(i_up))
   y_low = .5 * (y(j_low)+y0)   &   y_up = .5 * (y0+y(j_up))
   px = [ x_low, x_up, x_up, x_low, x_low ]
   py = [ y_low, y_low, y_up, y_up, y_low ]
   plots, px, py, col=col, /data, thick=1, lines=0	; Draw box
   device, SET_GRAPHICS_FUNCTION=old_graph_fct		; turn off line overplotting
 EndIf Else Begin
   pointed = 0
   device, SET_GRAPHICS_FUNCTION=old_graph_fct		; turn off line overplotting
   viewer, /fill, c_color=10*findgen(20)
   viewer, /overplot, c_color=1
EndElse

return
end;============= end of point_or_zoom routine =============================

pro essai

common wid_info, draw_id, col, old_graph_fct
common math_info, matrix, x, y
common dataranges, pointed, ip0, jp0, ip1, jp1

base = WIDGET_BASE(TITLE = "zoom essai", /COLUMN)

done_id = WIDGET_BUTTON(base, Value='Done', Uvalue='done')
unzoom_id = WIDGET_BUTTON(base, Value='UnZoom', Uvalue='unzoom')
draw_id = WIDGET_DRAW(base, Uvalue='pointer', xsize=600, ysize=500,$
				/BUTTON_EVENTS)

WIDGET_CONTROL, base, /REALIZE

loadct,39

matrix = dist(20)
x = findgen(20)   &   y = findgen(20)
ip0 = 0   &   ip1 = 19   &   jp0 = 0   &   jp1 = 19
viewer, /fill, c_color=10*findgen(20)
viewer, /overplot, c_color=1

col = !d.n_colors -1

XManager, "essai", base, EVENT_HANDLER = "essai_ev"

end ;============= end of main routine ====================================
