pro reldiff, z1, z2, zdiff

common axes 	; in : x.dim, y.dim
common arch_info        ; in: arch.missval
common axes2    ; in : x2.dim, y2.dim

if (z2.units ne z1.units) then Begin
   msg = ['reldiff : the two fields to compare have different units !',   $
          'Relative difference calculation cancelled.']
   warning = WIDGET_MESSAGE(msg)
   return
Endif
if (x.dim ne x2.dim) or (y.dim ne y2.dim) then Begin
   msg = ['reldiff : the two archives to compare have different dims !',   $
          'Relative difference calculation cancelled.']
   warning = WIDGET_MESSAGE(msg)
   return
Endif

;print,'Here is (z1-z2)/z2 where z1.name : '+z1.name+' and z2.name : '+z2.name
;FmtStr = '(A3,"/",I2,"/",I4)'
;print, 'z1.date : '+string(z1.date.month,z1.date.day,z1.date.year,FORMAT=FmtStr),  $
;   ' ; z2.date : '+string(z2.date.month,z2.date.day,z2.date.year,FORMAT=FmtStr)

zdiff = z1
For iloc = 0, 2 Do   $
    if (z1.loc(iloc) ne z2.loc(iloc)) then   $
       zdiff.loc(iloc) = '('+z1.loc(iloc)+'-'+z2.loc(iloc)+')/'+z2.loc(iloc)
thistring = up_subscript( z1 )
zdiff.name = thistring(0) + ' relative difference'
zdiff.units = '%'

m = x.dim-1   &   n = y.dim-1
z1OK = z1.valOK(0:m,0:n)   &   z2OK = z2.valOK(0:m,0:n)
z1mat = z1.matrix(0:m,0:n)   &   z2mat = z2.matrix(0:m,0:n)
idx = WHERE( z1OK and z2OK and z2mat ne 0.,   nidx )
If nidx eq 0 then Begin
   junk = WIDGET_MESSAGE( 'Relative difference impossible', /ERROR )
   zdiff.matrix(*,*) = arch.missval
   return
EndIf
mymatrix = fltarr(x.dim,y.dim)   &   myvalOK = bytarr(x.dim,y.dim)
mymatrix(idx) = 100. * ( z1mat(idx) - z2mat(idx) ) / z2mat(idx)
myvalOK(idx) = 1B
zdiff.valOK(*,*) = 0B   &   zdiff.badvals = 1B
zdiff.matrix(0:m,0:n) = mymatrix   &   zdiff.valOK(0:m,0:n) = myvalOK
zdiff.badvals = TOTAL( zdiff.valOK(0:m,0:n) ) ne x.dim*y.dim

end
