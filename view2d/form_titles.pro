pro form_titles

common plots_info	; inout : titles ; out : reset_titles, show

for i = 0, 5 do titles(i) = str_replace( titles(i), ',' , ';' )

desc = strarr(14)
desc(0) = '0, LABEL, Title:, LEFT=1'
desc(1) = '0, TEXT, '+titles(0)+', WIDTH=80, TAG=t0'
desc(2) = '0, TEXT, '+titles(1)+', WIDTH=80, TAG=t1'
desc(3) = '0, LABEL,, LEFT=1'
desc(4) = '0, LABEL, Subtitle (2d-plots only):, LEFT=1'
desc(5) = '0, TEXT, '+titles(2)+', WIDTH=80, TAG=t2'
desc(6) = '0, TEXT, '+titles(3)+', WIDTH=80, TAG=t3'
desc(7) = '0, LABEL,, LEFT=1'
desc(8) = '0, LABEL, Legend(s) (1d-plots only):, LEFT=1'
desc(9) = '0, TEXT, '+titles(4)+', WIDTH=80, TAG=t4'
desc(10) = '0, TEXT, '+titles(5)+', WIDTH=80, TAG=t5'
desc(11) = '1, BASE,, ROW, CENTER'
desc(12) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(13) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
form = CW_FORM(desc, /COLUMN, title='Change texts for last plot displayed')
if form.cancel then return

titles = [ form.t0, form.t1, form.t2, form.t3, form.t4, form.t5 ]
if (titles(0) ne '') or (titles(1) ne '') then show.title = 1
if (titles(2) ne '') or (titles(3) ne '') then show.subtitle = 1
if (titles(4) ne '') or (titles(5) ne '') then show.legends = 1
reset_titles = 0
redo_last

return
end
