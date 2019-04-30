PRO printsetup

common hardware               ; inout : print_command

If !D.name ne 'X' then junk = DIALOG_PRINTERSETUP()                               $
 Else Begin
   desc = [ 'O, TEXT, '+print_command+', WIDTH=38, TAG=command',               $
            '1, BASE,, ROW, CENTER',                                           $
            '0, BUTTON, OK, QUIT, TAG=OK',                                     $
            '2, BUTTON, Cancel, QUIT, TAG=CANCEL'       ]
   form = CW_FORM(desc, /COLUMN, title='Shell command to print PostScript file')
   If form.cancel then action = 'cancelled' Else print_command = form.command
EndElse

END

;======================================================================

FUNCTION nf_unitsget, cdfid, varname

units = ''
info = ncdf_varinq(cdfid, varname)
For j=0, (info.natts-1 < 2) do begin           ; Get attributes assoc w/ variable
    attname = ncdf_attname(cdfid, varname, j)
    If (attname eq 'units') then Begin	       ; all other attr are discarded
       ncdf_attget, cdfid, varname, attname, attvalue
       units = string(attvalue)
    EndIf
EndFor

; if (z.units eq 'vmr') or (z.units eq 'molec/cm3') then z.name=STRUPCASE(z.name)

return, units
END

;======================================================================

PRO file_utils
END
