pro write_ascii, z, xaxis, yaxis, plot1d=plot1d

common axes         ; in : yp
common plots_info   ; in : screen, titles ; out : my_device
common arch_info    ; in : arch.flnm
common dataranges   ; in : ip0, ip1, jp0, jp1, idp0, idp1
common path_com     ; in : write_ascii_path
common last_com     ; in : last.action

; Ask for the file name

def_fname = write_ascii_path+z.name+'.dat'

If last.action ne 'asciiwrite_allchem' then Begin
   fname = DIALOG_PICKFILE( file=def_fname, filter='*.dat',  $
        title='Provide name for ASCII file to write' )
   if (fname eq '') then return
   junk = FINDFILE( fname, COUNT=c )
   If (c ne 0) then Begin
      junk = DIALOG_MESSAGE('This file already exists. OK to overwrite ?',/QUESTION)
      if (junk eq 'No') then return
   EndIf
EndIf Else fname = def_fname

WIDGET_CONTROL, /HOURGLASS

openw, 1, fname
printf, 1, '! ASCII file written by view2d, an IDL viewer for 2D model NetCDF output files'
printf, 1, '! This data was written the '+SYSTIME()+' from NetCDF file '+arch.flnm
printf, 1, '!========================================================================================='
line = string( z.name+' ('+z.units+')', FORMAT='(a16)')
printf, 1, line+'  <- name (units) of the variable'
line = string( yaxis.i1-yaxis.i0+1, FORMAT='(i16)')
printf, 1, line+'  <- number of data lines for the field below'
printf, 1, titles(0)
printf, 1, titles(1)
printf, 1, '!========================================================================================='
line0 = string( '! ', yaxis.name,  FORMAT='(A2,A10)' )
line1 = string( '! ', yaxis.units, FORMAT='(A2,A10)' )
line2 = '!-----------'
For i = xaxis.i0, xaxis.i1 Do Begin
   line0 = line0 + string( xaxis.name, FORMAT='(A12)' )
   if xaxis.name eq 'time' then                              $
      strcoord = string(arch.list.depth(i), FORMAT='(A12)' ) $
    else strcoord = string(xaxis.coord(i), FORMAT='(F12.0)')
   line1 = line1 + strcoord
   line2 = line2 + '|-----------'
EndFor
If yaxis.name eq 'log-p altitude' then Begin
   line0 = line0 + '    pressure'
   line1 = line1 + '        (mb)'
   line2 = line2 + '|-----------'
EndIf
printf, 1, line0
printf, 1, line1
printf, 1, line2
   
For j = yaxis.i0, yaxis.i1 Do Begin
    If yaxis.name eq 'time' and yaxis.units eq 'days' then $
       line = string(arch.list.depth(j), FORMAT='(A12)' )  $
     Else line = string(yaxis.coord(j),FORMAT='(f12.0)')   
    For i = xaxis.i0, xaxis.i1 Do Begin
       If yaxis.name eq 'time' and yaxis.units eq 'days' then  $
          line = line + string(z.matrix(j,i),FORMAT='(e12.4)') $
        Else line = line + string(z.matrix(i,j),FORMAT='(e12.4)')
    EndFor
    if yaxis.name eq 'log-p altitude' then line = line + string(yp.coord(j),FORMAT='(e12.4)') 
    printf, 1, line
EndFor

close, 1

end                   
