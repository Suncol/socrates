pro test_form

pointed = 0
desc = strarr(7)
desc(0) = '1, BASE,, COLUMN, FRAME'
desc(1) = '2, BUTTON, This variable - this date (default)|ALL variables - this date|this variable - ALL dates, EXCLUSIVE, TAG=bg'
If pointed then Begin
   desc(2) = '1, BASE,, COLUMN, FRAME'
   desc(3) = '2, BUTTON, Altitude Profile(s) at pointed Latitude (default)|Latitude profile(s) at pointed altitude|2D data, EXCLUSIVE, TAG=kd'
   iprov = 4
EndIf Else Begin
   iprov = 2
   desc = desc(0:4)
EndElse
desc(iprov) = '1, BASE,, ROW, CENTER'
desc(iprov+1) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(iprov+2) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
form = CW_FORM(desc, /COLUMN, title='Write what ?')
if form.cancel then return
help, form, /struc

end
