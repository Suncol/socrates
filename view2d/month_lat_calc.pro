pro month_lat_calc, altfix=altfix, varname=varname, bg_arch=bg_arch, ref=ref

common arch_info        ; in : arch, arch2, arch3
common axes             ; in : x, y, depth
common axes2            ; in : x2, y2, depth2
common axes3            ; in : x3, y3, depth3
common select_id        ; in : jp, depth_id
common dataranges       ; in : jp0, jp1
common plots_info       ; in : densSW
common formats          ; in : Yfmt
common last_fields      ; out : zmolat, zmolat2, zmolat3

false = 0    &    true = 1
if (NOT KEYWORD_SET(altfix)) then altfix=false

If KEYWORD_SET(bg_arch) then Begin
   x0 = x2   &   y0 = y2   &   z0 = z2   &   arch0 = arch2   &   depth0 = depth2
 EndIf Else If KEYWORD_SET(ref) then Begin
   x0 = x3   &   y0 = y3   &   z0 = z3   &   arch0 = arch3   &   depth0 = depth3
 EndIf Else Begin
   x0 = x   &   y0 = y   &   z0 = zl   &   arch0 = arch   &   depth0 = depth
EndElse
If KEYWORD_SET(varname) then z0.name = varname

zmolat0 = zmolat
zmolat0.name = z0.name   &   zmolat0.units = z0.units   &   zmolat0.loc = z0.loc
mymatrix = fltarr(x0.dim,y0.dim)   &   zmomatrix = fltarr(depth0.dim,x0.dim)
col_vect = fltarr(x0.dim)   &   m = x0.dim - 1   &   n = depth0.dim - 1

WIDGET_CONTROL, /HOURGLASS

; About vmr -> nbdens conversion : we suppose that if z.units is 'vmr' or
; 'molec/cm3', it means that the data written on file is in 'vmr'
; About bad/missing values: they are not implemented for column (altfix=0)
; calculations
For date_id = 0, n Do Begin
   If altfix then Begin
      NCDF_VARGET, arch0.cdfid, z0.name, col_vect,                           $ ; Get values
           OFFSET=[0,jp,date_id], COUNT=[x0.dim,1,1]
      zmolat0.valOK(date_id,0:m) = FINITE( col_vect )                        $ ; NaNQ and INF
                                              and ABS(col_vect le 1.e30)       ; out of range
      If arch0.ok.missval then                                               $ ; explicit ...
         zmolat0.valOK(date_id,0:m) = zmolat0.valOK(date_id,0:m) and         $ ; ... missing vals
                                             ( col_vect ne arch0.missval )
      If densSW and ((z0.units eq 'vmr') or (z0.units eq 'molec/cm3')) then  $
         col_vect = col_vect * arch0.totdens(0:m,jp,date_id)
    EndIf Else Begin
      NCDF_VARGET, arch0.cdfid, z0.name, mymatrix,                           $ ; Get values
           OFFSET=[0,0,date_id], COUNT=[x0.dim,y0.dim,1]
      mat2 = mymatrix * arch0.totdens(*,*,date_id)
      mat3 = .5 * ( mat2(*,jp0+1:jp1) + mat2(*,jp0:jp1-1) )
      For l = 0, x0.dim -1 Do                                                 $
         col_vect(l) = TOTAL( 1.e5 * mat3(l,0:jp1-jp0-1)                     $ ; 1e5 converts from
                              * (y0.coord(jp0+1:jp1)-y0.coord(jp0:jp1-1)) )    ; y.units=km to cm
      if (z0.name eq 'o3') or (z0.name eq 'O3') or (z0.name eq 'ozone') then $
         col_vect = col_vect * 1000./2.687e19                                  ; convert from molec/cm2 to Dobson units
   EndElse
   zmomatrix(date_id,0:m) = col_vect
EndFor
if altfix then                                                               $
   zmolat0.badvals =                                                         $
      TOTAL( zmolat0.valOK(0:n,0:m) ) ne depth0.dim*x0.dim
If zmolat0.badvals then $
   zmomatrix( where ( zmolat0.valOK(0:n,0:m) eq 0B ) ) = arch0.missval
zmolat0.matrix(0:n,0:m) = zmomatrix

If depth0.type ne 'days0' and depth0.coord(depth0.dim) gt 1000.*365. then Begin
   days0date, depth0.coord(depth.dim), date
   zmolat0.loc(0) = date.month+'/'+STRTRIM(date.day,2)+'/'+STRTRIM(date.year,2)
EndIf

If altfix then Begin
   zmolat0.loc(1) = y0.name+' : '+STRING(y0.coord(jp),FORMAT='(f5.1)')+y0.units
   if (N_ELEMENTS(yp) ne 0 and yp.dim gt 0) then                                          $
      zmolat0.loc(1) = zmolat0.loc(1)+' ; '+yp.name+' : '                 $
                    + strtrim(yp.coord(jp),2)+' '+yp.units
 EndIf Else Begin
   If ((jp0 eq 0) and (jp1 eq y0.dim-1)) then Begin
      zmolat0.name = z0.name + ' total column'
    EndIf Else Begin
      zmolat0.name = z0.name + ' column from '                            $
                 + STRTRIM( STRING(y0.coord(jp0),format=Yfmt), 2 )        $
          +' to '+ STRTRIM( STRING(y0.coord(jp1),format=Yfmt), 2 ) + 'km'
   EndElse
   If (z0.name eq 'o3') or (z0.name eq 'O3') or (z0.name eq 'ozone') then $
      zmolat0.units = 'Dobson Units' Else zmolat0.units = 'molec/cm2'
EndElse

If KEYWORD_SET(bg_arch) then Begin
   zmolat2 = zmolat0
 EndIf Else If KEYWORD_SET(ref) then Begin
   zmolat3 = zmolat0
 EndIf Else Begin
   zmolat = zmolat0
EndElse

end
