pro densswitch

common axes
common axes2
common axes3
common arch_info
common select_id
common last_fields
common last_com
common plots_info

densSW = 1B   &   reset_titles = 1B
If (zl.units eq 'vmr') then Begin
   zl.matrix = zl.matrix * arch.totdens(*,*,zl.did)
   zl.units = 'molec/cm3'
   If z2.compOK then Begin
      z2.matrix = z2.matrix * arch2.totdens(*,*,z2.did)
      z2.units = 'molec/cm3'
   EndIf
   If z3.compOK then Begin
      z3.matrix = z3.matrix * arch3.totdens(*,*,z3.did)
      z3.units = 'molec/cm3'
   EndIf
EndIf
If (last.plot2d eq 'depth-x') and (zmolat.units eq 'vmr') then Begin
   zmolat.units = 'molec/cm3'
   For l = 0, x.dim-1 Do zmolat.matrix(*,l) = $
                       zmolat.matrix(*,l) * arch.totdens(l,jp,*)
   If z2.compOK then Begin
      zmolat2.units = 'molec/cm3'
      For l = 0, x2.dim-1 Do zmolat2.matrix(*,l) = $
                       zmolat2.matrix(*,l) * arch2.totdens(l,jp,*)
   EndIf
   If z3.compOK then Begin
      zmolat3.units = 'molec/cm3'
      For l = 0, x3.dim-1 Do zmolat3.matrix(*,l) = $
                       zmolat3.matrix(*,l) * arch3.totdens(l,jp,*)
   EndIf
 EndIf Else If (last.plot2d eq 'depth-y') and (zmolev.units eq 'vmr') then Begin
   zmolev.units = 'molec/cm3'
   For iz = 0, y.dim-1 Do zmolev.matrix(*,iz) = $
                       zmolev.matrix(*,iz) * arch.totdens(ip,iz,*)
   If z2.compOK then Begin
      zmolev2.units = 'molec/cm3'
      For iz = 0, y2.dim-1 Do zmolev2.matrix(*,iz) = $
                       zmolev2.matrix(*,iz) * arch2.totdens(ip,iz,*)
   EndIf
   If z3.compOK then Begin
      zmolev3.units = 'molec/cm3'
      For iz = 0, y3.dim-1 Do zmolev3.matrix(*,iz) = $
                       zmolev3.matrix(*,iz) * arch3.totdens(ip,iz,*)
   EndIf
EndIf

autoscaler, /force   &   redo_last

end
