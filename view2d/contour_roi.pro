PRO contour_roi, event, action      ; Define Region of Interest in contour plot

common moving_box            ; to save internal variables
common plots_info            ; in : draw_id, show.ygeo, ncols_avail
common last_com              ; out : last.action
common select_id             ; out : pointed, ip, jp

If show.ygeo then Begin        ; only find data pixel ; must be in 'x-y' contour
   if event.release or (event.type eq 2) then return
   coords = CONVERT_COORD(event.x, event.y, /DEVICE, /TO_DATA)
   pointer_xy, ip, jp, coords(0), coords(1)
   If (ip eq -1) or (jp eq -1) then Begin
      msg = DIALOG_MESSAGE( 'Point selection on geo alt 2D-plot failed',/ERROR )
      pointed = 0   &   return
   EndIf
   pointed = 1   &   device, window_state = win_exist
   if win_exist(1) then redo_last else draw_point
   return
EndIf

If event.press then Begin
   pressed = 1   &   xPress = event.x   &   yPress = event.y
   nx=0   &   ny=0
   px = [xPress, xPress + nx, xPress + nx, xPress, xPress]
   py = [yPress, yPress, yPress + ny, yPress + ny, yPress]
   device, GET_GRAPHICS_FUNCTION=old_graph_fct, $   ;Set xor graphics fct so that
           SET_GRAPHICS_FUNCTION=6                  ;line overplotting returns
                                                    ;to previous pixel value
   WIDGET_CONTROL, draw_id, /DRAW_MOTION_EVENTS
   last.action = action
EndIf
If (event.type eq 2) then Begin                     ;cursor is moving...
   plots, px, py, col=ncols_avail-1, /dev, thick=1, lines=0 ;UnDraw previous box
   nx = event.x - xPress   &   ny = event.y - yPress
   px = [xPress, xPress + nx, xPress + nx, xPress, xPress]
   py = [yPress, yPress, yPress + ny, yPress + ny, yPress]
   plots, px, py, col=ncols_avail-1, /dev, thick=1, lines=0 ;Re-Draw box
   last.action = action
EndIf
If event.release then Begin
   if not pressed then return
   plots, px, py, col=ncols_avail-1, /dev, thick=1, lines=0 ;UnDraw previous box
   xRelease = event.x   &   yRelease = event.y
   device, SET_GRAPHICS_FUNCTION=old_graph_fct              ; turn off line overplotting
   WIDGET_CONTROL, draw_id, DRAW_MOTION_EVENTS=0
   point_or_zoom, xPress, yPress, xRelease, yRelease
   pressed = 0
EndIf

END
