function ncdf_valsget, varname, bg=bg, ref=ref, ok=ok

common arch_info 	; in : arch, arch2, arch3, arch_to_compOK, arch_refOK
common axes		; in : x.dim, y.dim, depth.name ; out : x.coord
common axes2		; in : x2.dim, y2.dim, depth2.name
common axes3		; in : x3.dim, y3.dim, depth3.name
common last_fields      ; in : zl.did, z2.did, z3.did

ok = 0   &   msg=''
If not KEYWORD_SET(bg) and not KEYWORD_SET(ref) then Begin
   arch0 = arch   &   x0 = x   &   y0 = y
   depth0 = depth   &   did = zl.did
 EndIf Else Begin
   If KEYWORD_SET(bg) then Begin
      If not arch_to_compOK then Begin
         msg = ['NCDF_VALSGET: trying to read background '+varname,          $
                'no valid background file to read'] 
         GOTO, FAIL
      EndIf 
      arch0 = arch2   &   x0 = x2   &   y0 = y2
      depth0 = depth2   &   did = z2.did
   EndIf Else If KEYWORD_SET(ref) then Begin
      If not arch_refOK then Begin
         msg = ['NCDF_VALSGET: trying to read background '+varname,          $
                'no valid background file to read'] 
         GOTO, FAIL
      EndIf 
      arch0 = arch3   &   x0 = x3   &   y0 = y3
      depth0 = depth3   &   did = z3.did
   EndIf
EndElse

junk = WHERE( varname eq arch0.list.vars, count )
If count eq 0 then return, 'DO NOT USE'

mymatrix = fltarr(x0.dim,y0.dim)
ncdf_varget, arch0.cdfid, varname, mymatrix,      $
             OFFSET=[0,0,did], COUNT=[x0.dim,y0.dim,1]

nb_bad_el = N_ELEMENTS(mymatrix) - TOTAL(FINITE(mymatrix))
If nb_bad_el ne 0 then Begin
   msg = ['The field '+z.name+' has ',	  $
           STRTRIM(nb_bad_el,2)+' elements with a value of "NaNQ" or "INF"' ]
   GOTO, FAIL
EndIf
nb_bad_el = TOTAL(ABS(mymatrix gt 1.e30))              
If nb_bad_el ne 0 then Begin
   msg = ['The field '+z.name+' has ',	              $
          STRTRIM(nb_bad_el,2)+' elements with an abs value > 1.e30' ]
   GOTO, FAIL
EndIf

ok = 1
return, mymatrix

FAIL:
  error = DIALOG_MESSAGE( msg, /ERROR )   &   return, 'DO NOT USE'

END
