pro write_coldat, z, fname=fname

common arch_info	; in : arch
common axes		; in: x, y, depth
common dataranges	; in: jp0, jp1
common select_id	; in: ip, depth_id
common formats		; in: Xfmt, Yfmt, Dfmt
common path_com		; in: write_ascii_path

; Ask for the file name

If not KEYWORD_SET(fname) then Begin
   fname = DIALOG_PICKFILE( filter='*.dat', path=write_ascii_path, $
                         title='Provide name for ASCII file to write' )
   if (fname eq '') then return
EndIf
junk = FINDFILE( fname, COUNT=c )
If (c ne 0) then Begin
   junk = DIALOG_MESSAGE('This file already exists. OK to overwrite ?',/QUESTION)
   if (junk eq 'No') then return
EndIf

calc_zgeo, zgeo

fmt_data = '(f16.3,e16.5)'
separator = '!-------------------------------------------------------------------------'

openw, 1, fname
printf, 1, '! iday = ', depth.coord(depth_id) MOD 365., FORMAT='(a,i4)'
printf, 1, '! '+x.name+' = ', x.coord(ip), FORMAT='(a,f5.1)'
printf, 1, '! sza = ... to fix later'
printf, 1, jp1-jp0+1, '...is the number of altitude levels (data lines)', FORMAT='(i16,4x,a)'
; printf, 1, '! ','exact_alt', z.name, FORMAT='(a,a13,a16)'
printf, 1, '! ','log-p alt', z.name, FORMAT='(a,a13,a16)'
printf, 1, '! ','(km)', '('+z.units+')', FORMAT='(a,a13,a16)'
printf, 1, separator
;For j = jp0, jp1 Do printf, 1, zgeo(j), z.matrix(ip,j), FORMAT=fmt_data
For j = jp0, jp1 Do printf, 1, y.coord(j), z.matrix(ip,j), FORMAT=fmt_data

printf, 1, '!'
printf, 1, separator

nbfixcoo = 3   &   str_nbfixcoo = STRCOMPRESS(nbfixcoo,/REMOVE_ALL)
printf, 1, '! number of fixed coordinates at 2 lines each:'
printf, 1, str_nbfixcoo
printf, 1, '! Fixed coordinates... (1st line is format, 2d is name+value+units)'
Xlen = STRLEN(Xfmt)-2   &   fmt_X = '(a16,12x,'+STRMID(Xfmt,1,Xlen)+',a16)'
printf, 1, fmt_X
printf, 1, x.name, x.coord(ip), x.units, FORMAT = fmt_X
If arch.ok.date then Begin
   dname = 'date'   &   dunits = 'calendar'   &   dval=z.date
 EndIf Else Begin
   dname = depth.name   &   dunits = depth.units
   dval = arch.list.depth(depth_id)
EndElse
Dlen = STRLEN(Dfmt)-2   &   fmt_D = '(a16,5x,'+STRMID(Dfmt,1,Dlen)+',a16)'
printf, 1, fmt_D
printf, 1, dname, dval, dunits, FORMAT = fmt_D
If arch.ok.diuvar then Begin
   tname = 'local time'   &   tunits = 'hours'
   fmt_T = '(a16,11x,f5.2,a16)'   &   tval = z.slt(ip)
 EndIf Else Begin
   tname = 'local time'   & tunits = 'no_units'
   fmt_T = '(3a16)'   &   tval = arch.attr(2,1)
EndElse   
printf, 1, fmt_T
printf, 1, tname, tval, tunits, FORMAT = fmt_T

printf, 1, separator
printf, 1, '! main variable (units) to find in this data file:'
printf, 1, z.name+' ('+z.units+')'
printf, 1, '! as a function of:'
printf, 1, 'geometric altitude (km)'
printf, 1, '! Data source:'
printf, 1, arch.attr(0,1)+' '+arch.attr(1,1)
printf, 1, '! Data method:'
printf, 1, 'model'
printf, 1, '! data science field (for keyword searches):'
If (STRPOS(z.units,'K') ge 0) then kword = 'thermics' 			$
 Else If (STRPOS(z.units,'/s') ge 0) then kword = 'dynamics'		$
 Else If (z.units eq 'vmr' or z.units eq 'molec/cm3') then kword = 'chemistry' $
 Else kword = 'unknown' 
printf, 1, kword

printf, 1, separator
printf, 1, '! Original path, name and creation date for this data file (2 lines):'
printf, 1, fname
printf, 1, systime()
printf, 1, '! Path and name of originating program/data file:'
printf, 1, arch.flnm
printf, 1, separator

close, 1
return
end                   
