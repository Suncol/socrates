pro set_xymargins, my_xmargin, my_ymargin, plot1d=plot1d

common plots_info	  ; in : show
common last_fields        ; in : z3.compOK

my_xmargin = [10,3]   &   my_ymargin = [5,3]

If show.yp then Begin
   my_xmargin(1) = 16
   if !D.NAME eq 'WIN' then my_xmargin(1) = 14
   if (!p.background eq 255) then my_xmargin = [8,12] ; PS output
EndIf

if show.title then my_ymargin(1) = 6

If NOT KEYWORD_SET(plot1d) then Begin
   if show.subtitle then my_ymargin(0) = 10
   if !D.NAME eq 'WIN' then my_ymargin = [8,4]
 EndIf Else Begin
   If show.legends then Begin
      my_ymargin(0) = 10
      If !D.NAME eq 'WIN' then my_ymargin = [7,4]
      if z3.compOK then my_ymargin(0) = my_ymargin(0) + 3
   EndIf
EndElse

return
end
