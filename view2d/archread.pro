pro archread, varname, depth_id, z, bg_arch=bg_arch, ref=ref, noconvert=noconvert

common arch_info 	; in : arch, arch2, arch3, arch_to_compOK, arch_refOK
common axes		; in : x.dim, y.dim, depth.name ; out : x.coord
common axes2		; in : x2.dim, y2.dim, depth2.name
common axes3		; in : x3.dim, y3.dim, depth3.name
common plots_info	; in : densSW

If not KEYWORD_SET(bg_arch) and not KEYWORD_SET(ref) then Begin
   arch0 = arch   &   x0 = x   &   y0 = y   &   depth0 = depth
 EndIf Else Begin
   If KEYWORD_SET(bg_arch) then Begin
      If not arch_to_compOK then Begin
         msg = 'archread : no valid background file to read'
         error = WIDGET_MESSAGE( msg, /ERROR )
         return
      EndIf
      arch0 = arch2   &   x0 = x2   &   y0 = y2   &   depth0 = depth2
   EndIf Else If KEYWORD_SET(ref) then Begin
      If not arch_refOK then Begin
         msg = 'archread : no valid reference file to read'
         error = WIDGET_MESSAGE( msg, /ERROR )
         return
      EndIf
      arch0 = arch3   &   x0 = x3   &   y0 = y3   &   depth0 = depth3
   EndIf 
EndElse

mymatrix = fltarr(x0.dim,y0.dim)
m = x0.dim - 1   &   n = y0.dim - 1


junk = where( varname eq arch0.list.vars, count )
If count lt 1 then Begin
   junk = DIALOG_MESSAGE( 'ARCHREAD: variable '+varname+' not in var list' )
   return
EndIf
 
z.name = varname
z.units = NCDF_UNITSGET( arch0.cdfid, z.name )

;-------------------- Find if the var is time-dependent --------------------

info = NCDF_VARINQ( arch0.cdfid, z.name )
If info.ndims eq 3 then z.tdep = 1B        $    ; usual case: var is time-dep
 Else If info.ndims eq 2 then z.tdep = 0B  $
 Else Begin
   msg = ['ARCHREAD: var '+z.name+' has '+strtrim(info.ndims,2)+' dims',      $
          'view2d *needs* variables with 2 (x & y) or 3 (x & y & time) dims', $
          'Returning missing values and exiting']
   junk = DIALOG_MESSAGE( msg )
   z.valOK(0:m,0:n) = 0B   &   z.badvals = 0B   &   z.units='error'
   return   
EndElse

;--------------- Set time-related information ------------------------------

If depth0.type eq 'days0' then Begin
   days0date, depth0.coord(depth_id), date 
 EndIf Else Begin
   If z.tdep then z.loc(0) = arch0.list.depth(depth_id)                $
                           + ' ' + depth.units                         $
    else z.loc(0) = ''
   If depth0.coord(depth0.dim) gt 1000.*365. then Begin
      days0date, depth0.coord(depth.dim), date
    EndIf Else date = { datetype, month:'??', day:0, year:0 }
EndElse
 
z.date = date

;------------------------ Get the values -----------------------------------

If z.tdep then NCDF_VARGET, arch0.cdfid, z.name, mymatrix,             $			$
                    OFFSET=[0,0,depth_id], COUNT=[x0.dim,y0.dim,1]     $
 Else NCDF_VARGET, arch0.cdfid, z.name, mymatrix, COUNT=[x0.dim,y0.dim]

;------------------- Check for bad values ---------------------------------

z.valOK(0:m,0:n) = FINITE( mymatrix )                       ;  NaNQ and INF
z.valOK(0:m,0:n) = z.valOK(0:m,0:n) and ABS(mymatrix le 1.e30)  ;  out of range
If arch0.ok.missval then             $                  ; explicit missing vals
   z.valOK(0:m,0:n) = z.valOK(0:m,0:n) and ( mymatrix ne arch0.missval )
z.badvals = TOTAL( z.valOK(0:m,0:n) ) ne x0.dim*y0.dim

;----------- If required, convert to number density -----------------------

If arch0.OK.totdens and densSW and (z.units eq 'vmr') and $
                                    not KEYWORD_SET(noconvert) then Begin
   mymatrix = mymatrix * arch0.totdens(0:m,0:n,depth_id) 
   z.units = 'molec/cm3'
EndIf

If z.badvals then mymatrix( where ( z.valOK(0:m,0:n) eq 0 ) ) = arch0.missval
z.matrix(0:m,0:n) = mymatrix

end
