pro oned_ic

common arch_info
common axes
common select_id

latlist = STRING(FIX(x.coord(0)))
For ilat = 1, x.dim-1 Do latlist = latlist + '|' + STRING(FIX(x.coord(ilat)))

desc = strarr(7)
desc(0) = '0, TEXT,, LABEL_LEFT=Enter Directory name:, WIDTH=20, TAG=dname'
desc(1) = '1, BASE,, COLUMN, FRAME'
desc(2) = '2, BUTTON, First remove all files from this directory|This is a new directory - create it, EXCLUSIVE, TAG=bg'
desc(3) = '0, DROPLIST, '+latlist+', LABEL_LEFT=Choose a latitude:, TAG=ilat'
desc(4) = '1, BASE,, ROW, CENTER'
desc(5) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(6) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
form = CW_FORM(desc, /COLUMN, title='Write ONED initial conditions')

if form.cancel or (form.dname eq '') then return
WIDGET_CONTROL, /HOURGLASS
CASE form.bg OF
     0 : spawn, 'rm '+form.dname+'/*'
     1 : spawn, 'mkdir '+form.dname
ENDCASE
spawn, 'date', cur_date
For var_id_prov = 0, nbfields - 1  Do Begin
    archread, varlist(var_id_prov), depth_id, zprov
    If (zprov.units eq 'vmr') then Begin
       openw, 1, form.dname+'/'+zprov.name+'.dat'
       printf, 1, 'ASCII file written by the IDL/NetCDF Viewer for NCAR/ACD 2D model NetCDF output files'
       printf, 1, 'This data was written the '+cur_date+' from NetCDF file '+arch_flnm
       asciiwrite, zprov, fixed_ip=form.ilat
       close, 1
    EndIf
EndFor

return
end
