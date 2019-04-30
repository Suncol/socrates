pro find_ids, ref=ref, read=read

common axes             ; in : depth.coord
common axes2            ; in : depth2.coord, depth2.dim
common axes3            ; in : depth3.coord, depth3.dim
common arch_info        ; in : arch, arch2, arch3, arch_to_compOK, arch_refOK
common plots_info       ; in : show.ygeo
common last_fields      ; out : z2 or z3

ref = KEYWORD_SET(ref)
If ref then Begin
   z3.compOK = 0B   &   if arch_refOK eq 0 then return
   arch0 = arch3    &   z0 = z3
   x0 = x3   &   y0 = y3   &   depth0 = depth3
   if KEYWORD_SET(read) and show.ygeo then ygeo0 = ygeo3
 EndIf Else Begin
   z2.compOK = 0B   &   if arch_to_compOK eq 0 then return
   arch0 = arch2    &   z0 = z2
   x0 = x2   &   y0 = y2   &   depth0 = depth2
   if KEYWORD_SET(read) and show.ygeo then ygeo0 = ygeo2
EndElse

;------ Find z2.vid or z3.vid

z0.compOK = 1B
If z0.fix ne 1 then Begin
   z0.vid = -1
   For id = 0, arch0.list.nbvars-1 Do                                   $
      if STRUPCASE(arch0.list.vars(id)) eq STRUPCASE(arch.list.vars(zl.vid)) $
                             then z0.vid = id
   z0.compOK = z0.vid ge 0
EndIf

;------ Look for the closest date

If z0.fix ne 2 then Begin
   z0.did = closest_date( zl.did, ref=ref )
   z0.compOK = z0.compOK and ( z0.did ge 0 )
EndIf

;------ Read the new values if that was asked

If KEYWORD_SET(read) and z0.compOK then Begin
   mybg = 1B   &   if ref then mybg = 0B
   archread, arch0.list.vars(z0.vid), z0.did, z0, bg_arch=mybg, ref=ref
   if show.ygeo then ygeo0.matrix(0:x0.dim-1,0:y0.dim-1) = calc_ygeo( bg=mybg, ref=ref )
EndIf

;------ Promote the result

If ref then Begin
   z3 = z0
   if KEYWORD_SET(read) and z0.compOK and show.ygeo then ygeo3 = ygeo
 EndIf Else Begin
   z2 = z0
   if KEYWORD_SET(read) and z0.compOK and show.ygeo then ygeo2 = ygeo
EndElse

end
