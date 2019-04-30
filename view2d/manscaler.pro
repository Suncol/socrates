pro manscaler

common comscale
common plots_info
common last_fields
common last_com

desc = strarr(8)
desc(0) = '0, FLOAT, '+scale.mins+', LABEL_LEFT=New minimum of the levels:, WIDTH=10, TAG=newmin'
desc(1) = '1, BASE,, ROW, FRAME'
desc(2) = '1, BASE,, COLUMN'
desc(3) = '2, BUTTON, Linear interval between levels|'+    $
          'Factor of 1.4143 between levels|Factor of 2 between levels|Factor of 10 between levels, EXCLUSIVE, TAG=bg'
desc(4) = '2, TEXT, '+strtrim(scale.interv,2)+', LABEL_LEFT=- value:, WIDTH=10, TAG=linint'
desc(5) = '1, BASE,, ROW, CENTER'
desc(6) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(7) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
GIVE_FORM :
form = CW_FORM(desc, /COLUMN, title='Levels manual setting')
if form.cancel then return

scale.vector(0) = form.newmin

If (form.bg eq 0) then Begin
   If (FLOAT(form.linint) le 0.) then Begin
      msg = ['Linear intervals between levels have been selected',	$
	     'Please give a value greater than zero for these intervals !']
      error = WIDGET_MESSAGE(msg, /ERROR)
      GOTO, GIVE_FORM
   EndIf
   For i = 1, scale.n-1 Do scale.vector(i) = scale.vector(i-1) + FLOAT(form.linint)
   scale.interv = strtrim(form.linint,2)
EndIf Else If (form.bg eq 1) then Begin
   For i = 1, scale.n-1 Do   $
      If (scale.vector(i-1) ge 1e38) then  $
        scale.vector(i) = scale.vector(i-1) + 1.e37    $
       Else scale.vector(i) = 1.4143 * scale.vector(i-1)
   scale.interv = 'factor of 1.4143'
EndIf Else If (form.bg eq 2) then Begin
   For i = 1, scale.n-1 Do   $
      If (scale.vector(i-1) ge 1e38) then  $
        scale.vector(i) = scale.vector(i-1) + 1.e37    $
       Else scale.vector(i) = 2. * scale.vector(i-1)
   scale.interv = 'factor of 2'
EndIf Else If (form.bg eq 3) then Begin
   For i = 1, scale.n-1 Do   $
      If (scale.vector(i-1) ge 1e38) then  $
        scale.vector(i) = scale.vector(i-1) + 1.e37    $
       Else scale.vector(i) = 10. * scale.vector(i-1)
   scale.interv = 'factor of 10'
EndIf

autolevSW = 0B   &   autoscaler   &   viewpreproc

end
