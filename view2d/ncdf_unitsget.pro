function ncdf_unitsget, cdfid, varname

units = 'not found'

info = NCDF_VARINQ( cdfid, varname )
For j=0, (info.natts-1 < 2) do begin		; Get attributes assoc w/ variable
    attname = ncdf_attname(cdfid, varname, j)
    If (attname eq 'units') then Begin		; all other attr are discarded
       ncdf_attget, cdfid, varname, attname, attvalue
       units = string(attvalue)
    EndIf
EndFor

return, units
end
