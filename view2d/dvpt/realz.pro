pro realz

common axes
common last_fields

If zl.name ne 'temperature' then stop,'U did not select temperature !'

t = zl
zl.name = 'geometric altitude z'   &   zl.units = 'km'
zl.matrix = 0.

y.name = 'log-pressure altitude z*'
mixN2 = fltarr(y.dim)    &   mixO2 = fltarr(y.dim)
mixN2(0:104) = .78       &   mixO2(0:99) = .18
mixN2(105:109) = .777    &   mixO2(100:104) = .16
mixN2(110:114) = .75     &   mixO2(105:109) = .14
mixN2(115:120) = .73     &   mixO2(110:114) = .11
                             mixO2(115:120) = .09

H = fltarr(y.dim)

For ilat = 0, x.dim-1 Do Begin
   For iz = 0, y.dim-1 Do Begin
      m = 28. * mixN2(iz) + 32. * mixO2(iz) + 16. * (1.-mixN2(iz)-mixO2(iz))
      m = 1.e-3 * m / 6.02e23		; kg/molec
      H(iz) = 1.e-3 * 1.38e-23 * t.matrix(ilat,iz) / m / 9.806
      If (iz gt 0) then Begin			; trapeze integration
	 dalt = ((H(iz)+H(iz-1))/7.) * (y.coord(iz)-y.coord(iz-1)) / 2.
	 zl.matrix(ilat,iz) = zl.matrix(ilat,iz-1) + dalt
      EndIf
   EndFor
EndFor

return
end
   
   
