PRO make_dstring, arch0
;------ Convert the depth coordinates to mm/dd/yyyy julian dates -----------

common formats      ; in: Dfmt

If (arch0.d.name eq 'time') and (arch0.d.units eq 'days') and                  $
             ( arch0.ok.socrates or (arch0.attr(0,1) eq 'GS-2D') ) then Begin
    arch0.ok.d_is_date = 1
    For i = 0, arch0.d.dim-1 Do Begin
       days0date, arch0.d.coord(i), date
       arch0.dstring(i) = STRING( date.month, date.day, date.year, FORMAT=Dfmt )
    EndFor
 EndIf Else Begin
    arch0.ok.d_is_date = 0
    arch0.dstring = STRING( arch0.d.coord )
EndElse

END

;======================================================================

PRO string_utils
END
