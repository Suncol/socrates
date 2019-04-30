pro valsetter

common plots_info     ; out: valSW, valsrange

desc = strarr(6)
desc(0) = '0, TEXT, '+strtrim(valsrange(0),2)+', LABEL_LEFT=min: , WIDTH=15, TAG=newmin'
desc(1) = '0, TEXT, '+strtrim(valsrange(1),2)+', LABEL_LEFT=max: , WIDTH=15, TAG=newmax'
desc(2) = '0, BUTTON, Manual setting|Automatic setting:, EXCLUSIVE, TAG=bg'
desc(3) = '1, BASE,, ROW, CENTER'
desc(4) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(5) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'

form = CW_FORM(desc, /COLUMN, title='Values range for 1D-plots')

if form.cancel then return

valSW = 1
If (form.bg eq 1) then Begin
   valSW = 0 
 EndIf Else Begin
   valsrange(0) = form.newmin
   valsrange(1) = form.newmax
EndElse

return
end
      
