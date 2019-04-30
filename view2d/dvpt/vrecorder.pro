pro vrecorder_ev, event

common axes
common last_fields
common select_id

WIDGET_CONTROL, event.id, GET_UVALUE = bttn

DateFmt = '(A3,"/",I2,"/",I4)'

CASE bttn OF

	'fixlat': BEGIN
		    print,'Click along the desired vertical line on the contour plot'
		    pointer, ip, jdumb
		    print, 'Next vertical slices will be recorded at'
		    print, x.name+' : ',x.coord(ip),' '+x.units
                  END
	'add':	BEGIN
	          If (N_ELEMENTS(ip) eq 0) then Begin
	             print, 'Define a latitude first !'
	             GOTO, CONTINUE
	          EndIf
	          printf, 11   &   printf, 11, zl.name   &   printf, 11, zl.units
	          prov = x.name   &    if (prov eq 'latitudes') then prov='lat'
	          printf, 11, prov+' '+string(x.coord(ip),FORMAT='(i3)')
	          printf, 11, zl.loc(0)+' ; '+zl.loc(1)+' ; '+zl.loc(2)
	          printf, 11, string(zl.date.month,zl.date.day,zl.date.year,  $
	                                  FORMAT=DateFmt)
	          printf, 11, zl.matrix(ip,*)
	        END
	'done': BEGIN
	          close, 11
		  WIDGET_CONTROL, event.top, /DESTROY
		END

  ELSE: MESSAGE, 'vrecorder_ev : Event User Value "'+type+'" Not Found', /TRACEBACK
ENDCASE

CONTINUE :

end

;==============================================================================

pro vrecorder, _EXTRA=e

common widget_ids
common axes
common filenames

vrecbase = WIDGET_BASE(GROUP_LEADER=selecterbase, $
                      TITLE = "Vert slices recorder", /COLUMN)

label1 = WIDGET_LABEL(vrecbase, VALUE='Recording file '+vrec_flnm)
fixlatbttn = WIDGET_BUTTON(vrecbase, VALUE='(Re)Define a '+x.name, UVALUE='fixlat')
addbttn = WIDGET_BUTTON(vrecbase, VALUE='Add', UVALUE='add')
donebttn = WIDGET_BUTTON(vrecbase, VALUE='Done', UVALUE='done')

WIDGET_CONTROL, vrecbase, /REALIZE			;create the widgets
							;that are defined

XManager, 'vrecorder', vrecbase, $			;register the widgets
		EVENT_HANDLER = "vrecorder_ev", $	;with the XManager
		GROUP_LEADER = selecterbase, _EXTRA=e	;and pass through the
							;group leader

return
end

;==== Lignes dans selecter_ev qui declenchaient ce pgm : 
;     'vertslrec' : BEGIN
;		     desc = strarr(4)
;		     desc(0) = '0, TEXT, data/?.dat, LABEL_LEFT=Enter File name:, WIDTH=20, TAG=fname'
;		     desc(1) = '1, BASE,, ROW, CENTER'
;		     desc(2) = '0, BUTTON, OK, QUIT, TAG=OK'
;		     desc(3) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
;                     form = CW_FORM(desc, /COLUMN, title='Write vertical profiles to ASCII')
;                     if form.cancel or (form.fname eq '') then GOTO, CONTINUE
;                     WIDGET_CONTROL, /HOURGLASS
;                     openw, 11, form.fname
;                     printf, 11, y.name   &   printf, 11, y.units
;                     printf, 11, y.dim    &   printf, 11, y.coord
;                     vrecorder
;      		   END
