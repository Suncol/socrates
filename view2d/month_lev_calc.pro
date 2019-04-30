pro month_lev_calc, latfix=latfix, bg_arch=bg_arch, ref=ref

common arch_info        ; in : arch, arch2, arch3
common axes             ; in : x, y, depth
common axes2            ; in : x2, y2, depth2
common axes3            ; in : x3, y3, depth3
common select_id        ; in : jp, depth_id
common dataranges       ; in : jp0, jp1
common plots_info       ; in : densSW
common formats          ; in : Xfmt
common last_fields      ; out : zmolev, zmolev2, zmolev3

WIDGET_CONTROL, /HOURGLASS

false = 0    &    true = 1
if (NOT KEYWORD_SET(latfix)) then latfix=false

If KEYWORD_SET(bg_arch) then Begin
   x0 = x2   &   y0 = y2   &   z0 = z2   &   arch0 = arch2   &   depth0 = depth2
 EndIf Else If KEYWORD_SET(ref) then Begin
   x0 = x3   &   y0 = y3   &   z0 = z3   &   arch0 = arch3   &   depth0 = depth3
 EndIf Else Begin
   x0 = x   &   y0 = y   &   z0 = zl   &   arch0 = arch   &   depth0 = depth
EndElse

zmolev0 = zmolev
zmolev0.name = z0.name   &   zmolev0.units = z0.units   &   zmolev0.loc = z0.loc
mymatrix = fltarr(x0.dim,y0.dim)   &   zmomatrix = fltarr(depth0.dim,y0.dim)
col_vect = fltarr(y0.dim)   &   m = y0.dim - 1   &   n = depth0.dim - 1

If depth0.type ne 'days0' and depth0.coord(depth0.dim) gt 1000.*365. then Begin
   days0date, depth0.coord(depth.dim), date
   zmolev0.loc(0) = date.month+'/'+STRTRIM(date.day,2)+'/'+STRTRIM(date.year,2)
EndIf

If latfix then Begin
   zmolev0.loc(1) = x0.name+' : '+STRING(x0.coord(ip),FORMAT=Xfmt)+x0.units
 EndIf Else Begin
   cosphir = COS(!pi*x0.coord(0:x0.dim-1)/180.)
   zmolev0.loc(1) = 'global average'
EndElse

; About vmr -> nbdens conversion : we suppose that if z.units is 'vmr' or
; 'molec/cm3', it means that the data written on file is in 'vmr'
; About bad/missing values: they are not implemented for column (latfix=0)
; calculations
For date_id = 0, n Do Begin
   If latfix then Begin
      NCDF_VARGET, arch0.cdfid, z0.name, col_vect,                           $ ; Get values
           OFFSET=[ip,0,date_id], COUNT=[1,y0.dim,1]
      zmolev0.valOK(date_id,0:m) = FINITE( col_vect )                   ; NaNQ and INF
      zmolev0.valOK(date_id,0:m) = zmolev0.valOK(date_id,0:m)  $
                                              and ABS(col_vect le 1.e28)       ; out of range
      If arch0.ok.missval then             $             ; explicit missing vals
         zmolev0.valOK(date_id,0:m) =                                 $
          zmolev0.valOK(date_id,0:m) and ( col_vect ne arch0.missval )
      If densSW and ((z0.units eq 'vmr') or (z0.units eq 'molec/cm3')) then  $
         col_vect = col_vect * arch0.totdens(ip,0:m,date_id)
      zmomatrix(date_id,0:m) = col_vect
    EndIf Else Begin
      NCDF_VARGET, arch0.cdfid, z0.name, mymatrix,                           $ ; Get values
           OFFSET=[0,0,date_id], COUNT=[x0.dim,y0.dim,1]
      count = 0
      If arch0.ok.missval then index = where( mymatrix eq arch0.missval, count )
      If count gt 0 then Begin
         zmolev0.valOK(date_id,*) = 0B
       EndIf Else Begin
         If densSW and ((z0.units eq 'vmr') or (z0.units eq 'molec/cm3')) then  $
            mymatrix = mymatrix * arch0.totdens(0:x0.dim-1,0:m, date_id)
         For j = 0, m Do zmolev0.matrix(date_id,j) =                      $
                 TOTAL( cosphir(*) * mymatrix(*,j) ) / TOTAL( cosphir(*) )
         zmolev0.valOK(date_id,0:m) = 1B
      EndElse
   EndElse
EndFor

zmolev0.badvals =                                                               $
      TOTAL( zmolev0.valOK(0:n,0:m) ) ne depth0.dim*y0.dim
If zmolev0.badvals then $
   zmomatrix( where ( zmolev0.valOK(0:n,0:m) eq 0B ) ) = arch0.missval
zmolev0.matrix(0:n,0:m) = zmomatrix

If KEYWORD_SET(bg_arch) then Begin
   zmolev2 = zmolev0
 EndIf Else If KEYWORD_SET(ref) then Begin
   zmolev3 = zmolev0
 EndIf Else Begin
   zmolev = zmolev0
EndElse

end
