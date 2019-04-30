PRO calculator_ev, event

common last_fields
common plots_info
common widgets_info


WIDGET_CONTROL, event.id, GET_UVALUE = type		;find the user value
							;of the widget where
							;the event occured

CASE type OF

     'quit'      : BEGIN
     		     calcSW = 0
     		     WIDGET_CONTROL, calcbttn_id, /SENSITIVE
                     WIDGET_CONTROL, event.top, /DESTROY
     		   END
     'addstack'  : BEGIN
     		     If (N_ELEMENTS(zstack1) ne 0) then zstack2 = zstack1
     		     zstack1 = zl
     		   END
     'viewzst1'  : If (N_ELEMENTS(zstack1) ne 0) then Begin
     		      zl = zstack1
     		      viewpreproc
     		   EndIf else print,'The stack is empty !!'
     'viewzst2'  : If (N_ELEMENTS(zstack2) ne 0) then Begin
     		      zl = zstack2
     		      viewpreproc
     		   EndIf else error = WIDGET_MESSAGE('There is no second field in the stack !!', /ERROR)
     'absdiff'   : If (N_ELEMENTS(zstack2) le 0) then 	$
                     error = WIDGET_MESSAGE(		$
                       'I need two fields in the stack to compute their absolute difference !', /ERROR)  $
                    Else Begin
                     absdiff, zstack1, zstack2, zl
                     autoscaler
                     viewpreproc
                   EndElse
     'reldiff'   : If (N_ELEMENTS(zstack2) le 0) then 	$
                     error = WIDGET_MESSAGE(		$
                       'I need two fields in the stack to compute their relative difference !', /ERROR) $
                    Else Begin
                     reldiff, zstack1, zstack2, zl
                     autoscaler
                     viewpreproc
                   EndElse
     'vertcomp' : BEGIN
     		     If (N_ELEMENTS(zstack2) le 0) then  $
                         error = WIDGET_MESSAGE('I need two fields in the stack to compare !', /ERROR)  $
                      Else If (zstack1.units ne zstack2.units) then  $
                        error = WIDGET_MESSAGE('I will not compare two fields with different units !', /ERROR)  $
   		      Else Begin
     		        vertslicer, zstack1, /get_ip
     		        vertslicer, zstack2, /overp
                        wset,0   &   viewpreproc
      		     EndElse
     		   END
  
  ELSE: MESSAGE, 'Event User Value "'+type+'" Not Found', /TRACEBACK
ENDCASE

CONTINUE :

END ;============= end of selecter event handling routine task =============

;==============================================================================math_bttn3 = WIDGET_BUTTON(math_menu, Value='Add to stack', Uvalue='addstack')

pro calculator

common widgets_info

calcbase_id = WIDGET_BASE(TITLE = "Stack Calculator",  $
                            /COLUMN, TLB_FRAME_ATTR=1) ; main base has a fixed size

calc_bttn3 = WIDGET_BUTTON(calcbase_id, Value='Add current 2D-plot to stack', Uvalue='addstack')
calc_bttn4 = WIDGET_BUTTON(calcbase_id, Value='View zstack1', Uvalue='viewzst1')
calc_bttn5 = WIDGET_BUTTON(calcbase_id, Value='View zstack2', Uvalue='viewzst2')
calc_bttn6 = WIDGET_BUTTON(calcbase_id, Value='Absolute Difference', Uvalue='absdiff')
calc_bttn7 = WIDGET_BUTTON(calcbase_id, Value='Relative Difference', Uvalue='reldiff')
calc_bttn8 = WIDGET_BUTTON(calcbase_id, Value='2 vert. slices', UVALUE='vertcomp')
calc_done = WIDGET_BUTTON(calcbase_id, Value='Done', UVALUE='quit')

WIDGET_CONTROL, calcbase_id, XOFFSET=0, 	$
    YOFFSET=(selgeo.ysize+selgeo.yoffset+14*selgeo.ypad), /REALIZE

XManager, "calculator", calcbase_id, $			;register the widgets
		EVENT_HANDLER = "calculator_ev", $	;with the XManager
		GROUP_LEADER = selbase_id, _EXTRA=e

return
end
