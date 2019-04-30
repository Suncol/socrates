pro absdiff, z1, z0, zdiff

common axes

if (z0.units ne z1.units) then Begin
   print, 'I will not substract two fields with different units !'
   return
EndIf

print,'Here is z1-z0 where z1.name : '+z1.name+' and z0.name : '+z0.name
FmtStr = '(A3,"/",I2,"/",I4)'
print, 'z1.date : '+string(z1.date.month,z1.date.day,z1.date.year,FORMAT=FmtStr),  $
   ' ; z0.date : '+string(z0.date.month,z0.date.day,z0.date.year,FORMAT=FmtStr)

zdiff = z0
For iloc = 0, 2 Do if (z1.loc(iloc) ne z0.loc(iloc)) then   $
		      zdiff.loc(iloc) = z1.loc(iloc)+'-'+z0.loc(iloc)
zdiff.name = z0.name+' absolute difference'
zdiff.matrix = z1.matrix - z0.matrix

return
end
