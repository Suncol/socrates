pro MESOTWRITE, _EXTRA=e

common axes             ; in : x, y, depth, yp
common last_fields      ; in : zl
common dataranges       ; in : ip0, ip1, jp0, jp1
common arch_info        ; in : arch
common path_com		; in : write_ascii_path
common formats          ; in : Dfmt

If zl.name ne 'temperature' then Begin
   junk = DIALOG_MESSAGE('First, select the temperature field', /ERROR)
   return
EndIf

; Ask for the file name

junk = STR_SEP( arch.attr(1,1), ' ', /trim )
def_fname = write_ascii_path + 'mesopause_' + junk(0) + '.dat'
fname = DIALOG_PICKFILE( file=def_fname, filter='*.dat',  $
                         title='Provide name for ASCII file to write' )
if (fname eq '') then return
junk = FINDFILE( fname, COUNT=c )
If (c ne 0) then Begin
   junk = DIALOG_MESSAGE('This file already exists. OK to overwrite ?',/QUESTION)
   if (junk eq 'No') then return
EndIf

WIDGET_CONTROL, /HOURGLASS

separator = '!---------------!---------------!---------------!---------------!---------------!---------------'
w = ncdf_valsget( 'w', ok=wok )
ygeo.matrix = calc_ygeo( )

; Open file and write header

openw, 1, fname
printf, 1, '! ASCII file written by view2d on '+SYSTIME()+' from NetCDF file '
printf, 1, '! ' + arch.flnm
line = string( 'mesotval', FORMAT='(a16)')
printf, 1, line+'  <- name (units) of the field below'
line = string( x.dim, FORMAT='(i16)')
printf, 1, line+'  <- number of latitudes (lines) for the field below'
printf, 1, separator
line = '!      latitude !   mesopause T !  mesopause z* !   mesopause z !   mesopause p '
if wok then line = line + '!   mesopause w'
printf, 1, line
line = '!     (degrees) !           (K) !          (km) !          (km) !          (mb) '
if wok then line = line + '!         (m/s)'
printf, 1, line
printf, 1, separator

; Find and print the values of temperature at mesopause and its altitude

For l = 0, x.dim-1 do Begin
   val = MIN( zl.matrix(l,jp0:y.dim-1), mlev )
   line = string( x.coord(l), val, y.coord(jp0+mlev),        $
                  ygeo.matrix(l,jp0+mlev), yp.coord(jp0+mlev),  $
                  format='(4f16.2,e16.5)' )
   if wok then line = line + string( w(l,jp0+mlev), format='(f16.8)' )
   printf, 1, line
EndFor

; Write additional info and close the file

printf, 1, separator
sim_date = string( zl.date.month, zl.date.day, zl.date.year, FORMAT=Dfmt )
printf, 1, sim_date
printf, 1, arch.attr(1,1)
close,1

return
end
