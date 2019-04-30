pro tot_molec, z, bg_arch=bg_arch, ref=ref

common arch_info        ; in : arch, arch2, arch3
common axes             ; in : depth
common axes2            ; in : depth2
common axes3            ; in : depth3
common select_id        ; in : jp, depth_id
common dataranges       ; in : jp0, jp1
common plots_info       ; in : show, densSW
common formats          ; in : Xfmt, Yfmt, Dfmt

If KEYWORD_SET(bg_arch) then Begin
   arch0 = arch2   &   depth0 = depth2
 EndIf Else If KEYWORD_SET(ref) then Begin
   arch0 = arch3   &   depth0 = depth3
 EndIf Else Begin
   arch0 = arch   &   depth0 = depth
EndElse

WIDGET_CONTROL, /HOURGLASS

vals = fltarr(depth0.dim)
mymatrix = fltarr(x.dim,y.dim)
col_vect = fltarr(x.dim)

R0 = 6356.7   &   convrd = !pi/180.   &   dphir=convrd*(x.coord(1)-x.coord(0))
cosp = COS(convrd*x.coord(*))

; About vmr -> nbdens conversion : we suppose that if z.units is 'vmr' or
; 'molec/cm3', it means that the data written on file is in 'vmr'
; About bad/missing values: they are not implemented for column (altfix=0)
; calculations

ncdf_varget, arch0.cdfid, z.name, mymatrix,               $       ; Get values
        OFFSET=[0,0,depth_id], COUNT=[x.dim,y.dim,1]
ni = mymatrix * arch0.totdens(*,*,depth_id) * 1.e-12         ; see NOTICE below
nicol = FLTARR(x.dim)
nitot = DOUBLE( 0. )

; NOTICE: due to overflow problems (nitot getting too large) we calculate
; 1.e-22 * nitot, multiplying ni by 1e-12 above then "forgetting" the 
; 1.e5 * 1.e5 of the formula for nitot below

For l = 0, x.dim-1 Do Begin
   For iz = y.dim-1, 1, -1 do     $    ; notice we approx ygeo by y
      nicol(l) = nicol(l) + 5.e4 * ( y.coord(iz) - y.coord(iz-1) )         $
                                 * ( ni(l,iz) + ni(l,iz-1) )
   nitot = nitot + nicol(l) * R0*dphir * 2.*!pi*R0 * cosp(l)
EndFor

msg = 'Total number of molecules of '+z.name+' = '+STRTRIM(nitot,2)+' *1.e22'
junk = DIALOG_MESSAGE( msg, /INFO )

return

For date_id = 0, depth0.dim-1 Do Begin
   ncdf_varget, arch0.cdfid, z.name, mymatrix,               $       ; Get values
        OFFSET=[0,0,date_id], COUNT=[x.dim,y.dim,1]
   mat2 = mymatrix * arch0.totdens(*,*,date_id)
   mat3 = .5 * ( mat2(*,1:y.dim-1) + mat2(*,0:y.dim-2) )
   For l = 0, x.dim -1 Do                                    $         ; to molec/cm2
      col_vect(l) = TOTAL( 1.e5 * mat3(l,0:y.dim-2)        $           ; 1e5 converts from
                           * (y.coord(1:y.dim-1)-y.coord(0:y.dim-2)) ) ; y.units=km to cm
   vals(date_id) = TOTAL( col_vect * dphir * 1.e5*R0 * 2.*!pi*R0 * cos2 )
EndFor

thistring = up_subscript( z )   &   titles = strarr(2)
titles(0) = thistring(0) + ' (total molecules)'
days0date, depth.coord(idp0), date
titles(0) = titles(0) + ' ;  from  '                              $
         + string(date.month,date.day,date.year,FORMAT=Dfmt)
days0date, depth.coord(idp1), date
titles(0) = titles(0) + '  to  '                                  $
         + string(date.month,date.day,date.year,FORMAT=Dfmt)
set_xymargins, junk, my_ymargin, /plot1d

plot, depth.coord(idp0:idp1), vals(idp0:idp1),                          $
         title=titles(0)+'!C'+titles(1),                                $
         xmargin=[16,4], ymargin=my_ymargin,                            $
         ytitle=thistring(0)+' (total molecules)',                      $
         xtitle=depth.label.name,                                       $
         xticks=depth.label.nb,                                         $
         xtickname=depth.label.txt(0:depth.label.nb),                   $
         xtickv=depth.label.coord(0:depth.label.nb),                    $
         ticklen=.02+.98*show.grid,                                     $
         xgridstyle=show.grid, ygridstyle=show.grid, _EXTRA = e


return
end
