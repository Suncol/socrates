function nicer_name, name
;  This is an abbreviated version of up_subscript.pro
thistring = ''
len = STRLEN(name)   &   char = strarr(len)
if ((len eq 7) and (STRMID(name,0,6) eq 'tracer')) or (name eq 'c2h6') or      $
   or (name eq 'c2h4') or (name eq 'c3h6') or (name eq 'pan')                  $
   or (name eq 'ch3ccl3') or (name eq 'ccl2o') or (name eq 'ch3ccl3')          $
   then return, 'wrong'
If (name eq 'o1d') then thistring = 'O(1D)' Else     $
 If (name eq 'o3p') then thistring = 'O(3P)' Else Begin
   For i = 0, len-1 Do char(i) = STRMID( name, i, 1 )
   For i = 0, len-1 Do Begin
      mybyte = BYTE(char(i))
      If (name ne 'totdens') and ((char(i) eq 'a') or (char(i) eq 'b')  $
             or (char(i) eq 'c') or (char(i) eq 'f') or (char(i) eq 'h')  $
             or (char(i) eq 'i') or (char(i) eq 'n') or (char(i) eq 'o')  $
             or (char(i) eq 'p')) then char(i) = STRUPCASE(char(i))       $
       Else If ((char(i) eq 'x') or (char(i) eq 'y')) then return, 'wrong'
   EndFor
EndElse

For i = 0, len-1 Do thistring(0) = thistring(0) + char(i)
return, thistring
end
  
;=============================================================================

pro write_dvpt_ic

common arch_info	; in: arch
common axes		; in: x, y, yp, depth
common select_id	; in: ip, depth_id
common formats		; in: Xfmt, Yfmt, Dfmt
common path_com		; in: write_ascii_path
common plots_info	; inout : densSW

WIDGET_CONTROL, /HOURGLASS
junk = DIALOG_PICKFILE( filter='*.dat', path=write_ascii_path, $
                         title='Provide directory for ASCII files to write', $
                         get_path = out_path )
archread, 'temperature', depth_id, z
write_coldat, z, fname=out_path+z.name+'.dat'
old_densSW = densSW
densSW = 1
For i = 2, arch.list.nbvars-1 Do Begin
    If NCDF_UNITSGET(arch.cdfid,arch.list.vars(i)) eq 'vmr' then Begin
       archread, arch.list.vars(i), depth_id, z
       len = STRLEN(z.name)   &   char = strarr(len)
       nice_name = nicer_name(z.name)
       if nice_name ne 'wrong' then    $
          write_coldat, z, fname=out_path+nice_name+'.dat'
    EndIf
EndFor
densSW = old_densSW

spawn, 'cp '+out_path+'CH2O.dat '+out_path+'CHO.dat'
spawn, 'cp '+out_path+'OClO.dat '+out_path+'ClOO.dat'
end                   
