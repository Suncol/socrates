pro test

fi_max = [   -35.,     5. ,  50.  ]
a  =     [ -5000., 10000. , 2000. ]
s =      [   20.,      5. ,   10. ]

fi = 85.* (findgen(1000)/500 - 1)   ; from -85 degrees to 85 degrees
g = fltarr(1000)
For i = 0, 2 Do g(*) = g(*) + $
   a(i) * EXP(-0.5*(fi(*)-fi_max(i))*(fi(*)-fi_max(i))/s(i)/s(i)) $
   / s(i) / SQRT(2.*!PI)

plot, fi, g, xrange=[-90.,90.], xstyle=1
plots, [0.,0.], [-1.e4,1.e4], color=72
plots, [-100.,100.], [0.,0.], color=72

end
