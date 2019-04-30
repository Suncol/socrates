pro absdiff, z1, z2, zdiff

common axes 	; in : x.dim, y.dim

If (z2.units ne z1.units) then Begin
   msg = ['absdiff : the two fields to compare have different units !',   $
          'Absolute difference calculation cancelled.']
   warning = WIDGET_MESSAGE(msg)
   return
Endif

;print,'Here is z1-z2 where z1.name : '+z1.name+' and z2.name : '+z2.name
;FmtStr = '(A3,"/",I2,"/",I4)'
;print, 'z1.date : '+string(z1.date.month,z1.date.day,z1.date.year,FORMAT=FmtStr),  $
;   ' ; z2.date : '+string(z2.date.month,z2.date.day,z2.date.year,FORMAT=FmtStr)

zdiff = z1
For iloc = 0, 2 Do   $
    if (z1.loc(iloc) ne z2.loc(iloc)) then   $
       zdiff.loc(iloc) = z1.loc(iloc)+'-'+z2.loc(iloc)
zdiff.name = z1.name + ' ABSOLUTE DIFFERENCE'

m = x.dim-1   &   n = y.dim-1
z1OK = z1.valOK(0:m,0:n)   &   z2OK = z2.valOK(0:m,0:n)
z1mat = z1.matrix(0:m,0:n)   &   z2mat = z2.matrix(0:m,0:n)
idx = WHERE( z1OK and z2OK,   nidx )
If nidx eq 0 then Begin
   junk = WIDGET_MESSAGE( 'Absolute difference impossible', /ERROR )
   zdiff.matrix(*,*) = arch.missval
   return
EndIf
mymatrix = fltarr(x.dim,y.dim)   &   myvalOK = bytarr(x.dim,y.dim)
mymatrix(idx) = z1mat(idx) - z2mat(idx)
myvalOK(idx) = 1B
zdiff.valOK(*,*) = 0B   &   zdiff.badvals = 1B
zdiff.matrix(0:m,0:n) = mymatrix   &   zdiff.valOK(0:m,0:n) = myvalOK
zdiff.badvals = TOTAL( zdiff.valOK(0:m,0:n) ) ne x.dim*y.dim

end
