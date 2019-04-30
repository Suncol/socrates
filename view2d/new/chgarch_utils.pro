PRO chgaxes                                    ; OOP: could become a method for "axis" class

common i_com         ; in : ifore
common arch_info     ; inout: arch(ifore).axis
common last_com      ; inout : last.plot

desc = strarr(9)
desc(0) = '0, TEXT,'+arch(ifore).x.name+',  LABEL_LEFT=Axis x  - name :, WIDTH=20, TAG=xname'
desc(1) = '0, TEXT,'+arch(ifore).x.units+', LABEL_LEFT=Axis x  - units:, WIDTH=20, TAG=xunits'
desc(2) = '0, TEXT,'+arch(ifore).y.name+',  LABEL_LEFT=Axis y  - name :, WIDTH=20, TAG=yname'
desc(3) = '0, TEXT,'+arch(ifore).y.units+', LABEL_LEFT=Axis y  - units:, WIDTH=20, TAG=yunits'
desc(4) = '0, TEXT,'+arch(ifore).y2.name+', LABEL_LEFT=Axis y2 - name :, WIDTH=20, TAG=ypname'
desc(5) = '0, TEXT,'+arch(ifore).y2.units+',LABEL_LEFT=Axis y2 - units:, WIDTH=20, TAG=ypunits'
desc(6) = '1, BASE,, ROW, CENTER'
desc(7) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(8) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
form = CW_FORM(desc, /COLUMN, title='Change axes labels')
If form.cancel then Begin
   action = 'cancelled'   &   return
EndIf

arch(ifore).x.name = form.xname   &   arch(ifore).x.units = form.xunits
arch(ifore).y.name = form.yname   &   arch(ifore).y.units = form.yunits
arch(ifore).y2.name = form.ypname   &   arch(ifore).y2.units = form.ypunits

last.plot.type = 'd2'                 &   last.plot.d2.x.axis  = arch(ifore).x
last.plot.d2.y.axis = arch(ifore).y   &   last.plot.d2.y2.axis = arch(ifore).y2
redo_plot

END

;======================================================================

PRO chgarch_utils
print, 'Chgarch_utils routines compiled'
END