pro SECJOU, daynum, phid, h2pi

;real, intent(in)  :: daynum
;real, intent(in)  :: phid		! incoming lat in degrees
;real, intent(out) :: h2pi

pi = 4. * ATAN( 1. )
d2r = pi / 180.

delta = d2r*23.45*COS( 2.*pi/365.*daynum )
deltad = delta / d2r
phi = phid * d2r
If( phid lt 0. ) then hautmx = 90. - deltad + phid		$
 Else hautmx = 90. + deltad - phid

If( hautmx le 0. )  then h2pi = 0.	$	; polar night
 Else Begin
   cosh = MAX( [-TAN(phi) * TAN(delta) , -1.] )
   h = ACOS( cosh )
   h2pi = h / (2.05*pi)
EndElse

return
end
