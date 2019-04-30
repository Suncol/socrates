pro point_or_zoom, xPress, yPress, xRelease, yRelease

common arch_info        ; in : arch.list.depth, diuvar, arch_to_compOK
common axes             ; in : x.coord, y.coord, depth
common select_id        ; out : pointed, ip, jp, idp
common dataranges       ; out : ip0, jp0, ip1, jp1
common label_widgets    ; out : msg
common formats          ; in : Xfmt, Yfmt, Dfmt
common last_com         ; in : last.plot2d ; out : last.action
common last_fields      ; in : zl
common plots_info       ; in : draw_id
common widgets_info     ; in : depthlistwidid

WIDGET_CONTROL, draw_id, GET_VALUE=draw_win
wset, draw_win

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
If (last.plot2d eq 'x-y') then Begin
   pointer_xy, ip00, jp00, x0, y0
   pointer_xy, ip01, jp01, x1, y1
 EndIf Else If (last.plot2d eq 'depth-x') then Begin
   pointer_dx, ip00, jp00, x0, y0
   pointer_dx, ip01, jp01, x1, y1
 EndIf Else If (last.plot2d eq 'depth-y') then Begin
   pointer_dy, ip00, jp00, x0, y0
   pointer_dy, ip01, jp01, x1, y1
EndIf

If (ip00 eq -1) or (ip01 eq -1) or (jp00 eq -1) or (jp01 eq -1) then Begin
   msg = DIALOG_MESSAGE( 'Point selection on 2D-plot failed', /ERROR )
   pointed = 0
   return
EndIf

If ((ip01-ip00) le 2) or ((jp01-jp00) le 2) then Begin
   last.action = 'point'   &   pointed = 1B
   If (last.plot2d eq 'x-y') then Begin
      ip = ip01   &   jp = jp01   &   idp = zl.did
    EndIf Else If (last.plot2d eq 'depth-x') then Begin
      zl.did= ip01   &   idp = ip01   &   ip = jp01   &   jp = 0
      WIDGET_CONTROL, depthlistwidid, SET_LIST_SELECT=zl.did
      find_ids   &   find_ids, /ref
    EndIf Else If (last.plot2d eq 'depth-y') then Begin
      zl.did= ip01   &   idp = ip01   &   ip = 0   &   jp = jp01
      WIDGET_CONTROL, depthlistwidid, SET_LIST_SELECT=zl.did
      archread, arch.list.vars(zl.vid), zl.did, zl
      find_ids, /read   &   find_ids, /ref, /read
   EndIf
   device, window_state = win_exist
   if win_exist(1) then redo_last else draw_point
 EndIf Else Begin
   last.action = 'zoom'
   If (last.plot2d eq 'x-y') then Begin
      y.i0 = jp00   &   y.i1 = jp01   &   x.i0 = ip00   &   x.i1 = ip01
      jp0 = y.i0   &   jp1 = y.i1   &   ip0 = x.i0   &   ip1 = x.i1  ; to remove ASAP
      redo_last
    EndIf Else If (last.plot2d eq 'depth-x') then Begin
      depth.i0 = ip00   &   depth.i1 = ip01   &   x.i0 = jp00   &   x.i1 = jp01
      idp0 = depth.i0   &   idp1 = depth.i1   ; to remove ASAP
      ip0 = x.i0   &   ip1 = x.i1             ; to remove ASAP
      time_labels, depth
      If z2.compOK then Begin
         idp2_0 = closest_date( depth.i0, /lbound )
         idp2_1 = closest_date( depth.i1, /ubound )
         print, 'idp2_0= ',idp2_0,' ; idp2_1= ',idp2_1,' i.e. from ',   $
              arch2.list.depth(idp2_0),' to ',arch2.list.depth(idp2_1)
      EndIf
      If z3.compOK then Begin
         idp3_0 = closest_date( idp0, /ref, /lbound )
         idp3_1 = closest_date( idp1, /ref, /ubound )
;         print, 'idp3_0= ',idp3_0,' ; idp3_1= ',idp3_1,' i.e. from ',   $
;              arch2.list.depth(idp3_0),' to ',arch2.list.depth(idp3_1)
      EndIf
      redo_last
    EndIf Else If (last.plot2d eq 'depth-y') then Begin
      depth.i0 = ip00   &   depth.i1 = ip01   &   y.i0 = jp00   &   y.i1 = jp01
      jp0 = y.i0   &   jp1 = y.i1               ; to remove ASAP
      idp0 = depth.i0   &   idp1 = depth.i1     ; to remove ASAP
      time_labels, depth
      redo_last
   EndIf
EndElse

end
