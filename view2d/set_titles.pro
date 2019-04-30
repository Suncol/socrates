pro print_legends, titles

common last_fields       ; in : z2.compOK, z3.compOK
common hardware         ; in : colorSW, l2color, l3color

plots, [.05,.1], [.09,.09], thick=2, /normal
xyouts, .12, .08, titles(4), /normal

If z2.compOK then Begin
   If colorsw then plots, [.05,.1], [.05,.05], color=l2color, thick=2, /normal      $
    Else plots, [.05,.1], [.05,.05], linestyle=2, thick=2, /normal
   xyouts, .12, .04, titles(5), /normal
EndIf
If z3.compOK then Begin
   If colorsw then plots, [.05,.1], [.13,.13], color=l3color, thick=2, /normal      $
    Else plots, [.05,.1], [.13,.13], linestyle=3, thick=2, /normal
   xyouts, .12, .12, titles(6), /normal
EndIf

return
end

;==========================================================================

pro set_titles, z, molat=molat, molev=molev, plot1d=plot1d, time_avg=time_avg

; titles(0) and titles(1) : titles of the plot (max 2 lines)
; titles(2) and titles(3): subtitle(s) of the plot
; titles(4:6) : legend(s)

common plots_info       ; out : titles ; in : show
common arch_info        ; in : arch, arch2.flnm, ...
common comscale         ; in : scale
common select_id        ; in : ip
common last_fields      ; in : z2, zmolat2, z3, zmolat3
common dataranges       ; in : ip0, ip1, jp0, jp1, idp0, idp1
common axes             ; in : depth.coord, yp
common formats          ; in : Dfmt

molat = KEYWORD_SET(molat)   &   molev = KEYWORD_SET(molev)
plot1d = KEYWORD_SET(plot1d)   &   time_avg = KEYWORD_SET(time_avg)
vardiff = 0B   &   if (z2.fix eq 1) or (z3.fix eq 1) then vardiff = 1B

If show.title and not (plot1d and vardiff) then Begin
   thistring = up_subscript( z )
   titles(0) = thistring(0) + ' (' + thistring(1) + ')'
EndIf Else titles(0:1) = [ '', '' ]

z_type = ''   &   thislen = STRLEN(z.name)
if (thislen gt 10) then z_type = STRUPCASE(STRMID( z.name, thislen-10, 10 ))

if plot1d and not molat and not molev then GOTO, SET_LEGENDS

If show.title then Begin
   If molat or molev then Begin
      If depth.type eq 'days0' then Begin
         days0date, depth.coord(idp0), date
         titles(0) = titles(0) + ' ;  from  '                              $
                   + string(date.month,date.day,date.year,FORMAT=Dfmt)
         days0date, depth.coord(idp1), date
         titles(0) = titles(0) + '  to  '                                  $
                   + string(date.month,date.day,date.year,FORMAT=Dfmt)
      EndIf
      if plot1d then GOTO, SET_LEGENDS
      if depth.type eq 'slt' then titles(0) = titles(0) + '; ' + z.loc(0)
    EndIf Else Begin
      titles(0) = titles(0) + ' ; '                                     $
                + string(z.date.month,z.date.day,z.date.year,FORMAT=Dfmt) + ' ; '
   EndElse
   if molat or molev then it = 1 else it = 0
   titles(1) = ''
   titles(it) = titles(it) + arch.attr(1,1)
   if arch.attr(2,0) eq 'label_long' then titles(it) = titles(it)       $
                                                + ' ; ' + arch.attr(2,1)
   For i = it, 2 Do if (z.loc(i) ne '') then titles(it) = titles(it)     $
                                                       + ' ; ' + z.loc(i)
EndIf Else titles(0:1) = [ '', '' ]

If show.subtitle then Begin
   get_minmax, minv, maxv
   titles(2) = '!C!Cmin = '+strtrim(minv,2)+' ; max = '+strtrim(maxv,2)
   titles(2) = titles(2) + '  ;   Contour from ' + scale.mins + ' to ' +   $
               scale.maxs + ' by ' + scale.interv
   titles(3) = arch.attr(0,1) + ' output'
   If arch_to_compOK and (z_type eq 'DIFFERENCE') then Begin
      titles(3) = titles(3) + '  from files  ' + extract_flnm(arch.flnm)
      titles(3) = titles(3) + '  and  ' + extract_flnm(arch2.flnm)
    EndIf Else titles(3) = titles(3) + '  from file ' + extract_flnm(arch.flnm)
EndIf Else titles(2:3) = [ '', '' ]

return

SET_LEGENDS:
If show.legends then Begin
   if vardiff then titles(4) = z.name+' ; ' else titles(4) = ''
   If not molat and not molev then Begin
      If not time_avg then titles(4) = titles(4) +                         $
          string(z.date.month,z.date.day,z.date.year,FORMAT=Dfmt) + ' ; '  $
       Else Begin
         days0date, depth.coord(idp0), date
         titles(4) = 'from  '                                              $
                   + string(date.month,date.day,date.year,FORMAT=Dfmt)
         days0date, depth.coord(idp1), date
         titles(4) = titles(4) + '  to  '                                  $
                   + string(date.month,date.day,date.year,FORMAT=Dfmt) + ' ; '
      EndElse
   EndIf Else titles(4) = titles(4) + zmolat.loc(0) + ' ; '
   titles(4) = titles(4) + arch.attr(1,1) + ' ; ' + arch.attr(2,1)
   titles(4) = titles(4) + ' ; ' + arch.attr(0,1) + ' output'
   If arch_to_compOK and (z_type eq 'DIFFERENCE') then Begin
      titles(4) = titles(4) + ' from files ' + extract_flnm(arch.flnm)
      titles(4) = titles(4) + ' and ' + extract_flnm(arch2.flnm)
    EndIf Else titles(4) = titles(4) + ' from file ' + extract_flnm(arch.flnm)

   If z2.compOK then Begin
      if vardiff then titles(5) = z2.name+' ; ' else titles(5) = ''
      If not molat and not molev then Begin
         If not time_avg then titles(5) = titles(5) +                      $
                string(z2.date.month,z2.date.day,z2.date.year,FORMAT=Dfmt) $
          Else Begin
            days0date, depth.coord(idp2_0), date
            titles(5) = 'from  '                                              $
                      + string(date.month,date.day,date.year,FORMAT=Dfmt)
            days0date, depth.coord(idp2_1), date
            titles(5) = titles(5) + '  to  '                                  $
                   + string(date.month,date.day,date.year,FORMAT=Dfmt) + ' ; '
         EndElse
      EndIf Else titles(5) = titles(5) + zmolat2.loc(0) + ' ; '
      titles(5) = titles(5) + ' ; ' + arch2.attr(1,1) + ' ; ' + arch2.attr(2,1)
      titles(5) = titles(5) + ' ; ' + arch2.attr(0,1) + ' output'
      titles(5) = titles(5) + ' from file ' + extract_flnm(arch2.flnm)
   EndIf
   If z3.compOK then Begin
      if vardiff then titles(6) = z3.name+' ; ' else titles(6) = ''
      If not molat and not molev then Begin
         If not time_avg then titles(6) = titles(6) +                      $
                string(z3.date.month,z3.date.day,z3.date.year,FORMAT=Dfmt) $
          Else Begin
            days0date, depth.coord(idp3_0), date
            titles(6) = 'from  '                                              $
                      + string(date.month,date.day,date.year,FORMAT=Dfmt)
            days0date, depth.coord(idp3_1), date
            titles(6) = titles(6) + '  to  '                                  $
                   + string(date.month,date.day,date.year,FORMAT=Dfmt) + ' ; '
         EndElse
      EndIf Else titles(6) = titles(6) + zmolat3.loc(0) + ' ; ' 
      titles(6) = titles(6) + ' ; ' + arch3.attr(1,1) + ' ; ' + arch3.attr(2,1)
      titles(6) = titles(6) + ' ; ' + arch3.attr(0,1) + ' output'
      titles(6) = titles(6) + ' from file ' + extract_flnm(arch3.flnm)
   EndIf

EndIf Else titles(4:6) = [ '', '', '' ]

end
