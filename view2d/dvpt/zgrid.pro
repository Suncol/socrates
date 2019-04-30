pro zgrid, t, ilat

common axes

If t.name ne 'temperature' then stop,'U did not select temperature !'

openw, 1, 'data/zgrid1.txt'
FmtStr = '(A3,"/",I2,"/",I4)'
header = string(t.date.month,t.date.day,t.date.year,FORMAT=FmtStr)
printf, 1, header+'  at ilat = '+string(ilat)+'  ; '+t.loc(0)+' ; '+t.loc(1)+' ; '+t.loc(2)
printf, 1, FORMAT='(A)'
printf, 1,' z* (km) | T (K)  |   p  (mb)   | n (molec/cm3) | H(z) (km) | z (km)  |  dz (km)'
printf, 1,'--------------------------------------------------------------------------------'
FmtStr = '(f4.0,"     | ",f6.2," | ",f11.6," | ",1e10.3,"    | ",f6.3,"    | ",f7.3," | ",f5.3)'

mixN2 = fltarr(y.dim)    &   mixO2 = fltarr(y.dim)
mixN2(0:104) = .78       &   mixO2(0:99) = .18
mixN2(105:109) = .777    &   mixO2(100:104) = .16
mixN2(110:114) = .75     &   mixO2(105:109) = .14
mixN2(115:120) = .73     &   mixO2(110:114) = .11
                             mixO2(115:120) = .09

p = fltarr(y.dim)   &   alt = fltarr(y.dim)   &   n = fltarr(y.dim)
H = fltarr(y.dim)   &   dalt = fltarr(y.dim)  &   m = fltarr(y.dim)
alt(0) = 0.

For iz = 0, y.dim-1 Do Begin
   m(iz) = 28. * mixN2(iz) + 32. * mixO2(iz) + 16. * (1.-mixN2(iz)-mixO2(iz))
   m(iz) = 1.e-3 * m(iz) / 6.02e23		; kg/molec
   p(iz) = 1013. * EXP(-y.coord(iz)/7.)
   n(iz) = 7.34e21 * EXP(-y.coord(iz)/7.) / t.matrix(ilat,iz)
   H(iz) = 1.e-3 * 1.38e-23 * t.matrix(ilat,iz) / m(iz) / 9.806
   If (iz gt 0) then Begin			; trapeze integration
      dalt(iz) = ((H(iz)+H(iz-1))/7.) * (y.coord(iz)-y.coord(iz-1)) / 2.
      alt(iz) = alt(iz-1) + dalt(iz)
   EndIf
   dataline = string( y.coord(iz), t.matrix(ilat,iz), p(iz), n(iz), H(iz),     $
                         alt(iz), dalt(iz), FORMAT=FmtStr )
   printf, 1, dataline
EndFor

close,1
return
end
   
   
