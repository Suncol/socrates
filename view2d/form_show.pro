pro form_show

common plots_info	; inout : show ; out : titles

desc = strarr(11)

desc(0) = '0, LABEL, Text elements, CENTER'
desc(1) = '1, BASE,, COLUMN, FRAME'

desc(2) = '2, BUTTON, '
If show.title then desc(2) = desc(2) + 'turn Title OFF (now it is ON)' $
 Else desc(2) = desc(2) + 'turn Title ON (now it is OFF)'
If show.subtitle then desc(2) = desc(2) + '|turn Subtitle OFF (now it is ON) (2D-plots only)' $
 Else desc(2) = desc(2) + '|turn Subtitle ON (now it is OFF) (2D-plots only)'
If show.legends then desc(2) = desc(2) + '|turn Legends OFF (now it is ON) (1D-plots only)' $
 Else desc(2) = desc(2) + '|turn Legends ON (now it is OFF) (1D-plots only)'
desc(2) = desc(2) + ', TAG=bt'

desc(3) = '0, LABEL,, LEFT=1'
desc(4) = '0, LABEL, Graphics elements, CENTER'
desc(5) = '1, BASE,, COLUMN, FRAME'

desc(6) = '2, BUTTON, '
If show.pdn then desc(6) = desc(6) + 'turn Polar Night/Day limits OFF (now it is ON)' $
 Else desc(6) = desc(6) + 'turn Polar Night/Day limits ON (now it is OFF)'
If show.grid then desc(6) = desc(6) + '|turn Axes Grid OFF (now it is ON) (1D-plots only)' $
 Else desc(6) = desc(6) + '|turn Axes Grid ON (now it is OFF) (1D-plots only)'
If show.linfit then desc(6) = desc(6) + '|turn LINFIT OFF (now it is ON) (time 1D-plots only)' $
 Else desc(6) = desc(6) + '|turn LINFIT ON (now it is OFF) (time 1D-plots only)'
If yp.dim gt 0 then Begin
   If show.yp then desc(6) = desc(6) + '|turn Pressure Axis OFF (now it is ON)' $
    Else desc(6) = desc(6) + '|turn Pressure Axis ON (now it is OFF)'
EndIf
If show.mesolev then desc(6) = desc(6) + '|turn Temperature minimum level OFF (now it is ON)' $
 Else desc(6) = desc(6) + '|turn Temperature minimum level ON (now it is OFF)'
desc(6) = desc(6) + ', TAG=bg'

desc(7) = '0, LABEL,, LEFT=1'
desc(8) = '1, BASE,, ROW, CENTER'
desc(9) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(10) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'

form = CW_FORM(desc, /COLUMN, title='Plot switches')
if form.cancel then return

if form.bt(0) then if show.title then show.title = 0B else show.title = 1B
if form.bt(1) then if show.subtitle then show.subtitle = 0B else show.subtitle = 1B 
if form.bt(2) then if show.legends then show.legends = 0B else show.legends  = 1B
if form.bg(0) then if show.pdn then show.pdn = 0B else show.pdn  = 1B
if form.bg(1) then if show.grid then show.grid = 0B else show.grid  = 1B
if form.bg(2) then if show.linfit then show.linfit = 0B else show.linfit  = 1B
if form.bg(3) then if show.yp then show.yp = 0B else show.yp = 1B
if form.bg(4) then if show.mesolev then show.mesolev = 0B else show.mesolev = 1B

if not show.title then titles(0:1) = [ '', '' ]
if not show.subtitle then titles(2:3) = [ '', '' ]
if not show.legends then titles(4:5) = [ '', '' ]

redo_last

return
end
