PRO polardn, arch0

For id = 0, arch0.d.dim-1 Do Begin
   daynum = ((arch0.d.coord(id) - 1) MOD 365) + 1 - 172   ; Julian day - 172
   declind = 23.45 * COS( 2.*!pi/365.*daynum )
   If( ABS(daynum) gt 91. ) then Begin			  ; northern polar night
      arch0.latpn(id) = 90. + declind
    EndIf Else Begin					  ; southern polar night
      arch0.latpn(id) = -90. + declind
   EndElse
EndFor

END

;======================================================================

PRO smooth_field             ; OOP: this could be a method for the "field" class

common last_com      ; inout : last.plot.d2

xdim = last.plot.d2.x.axis.dim   &    ydim = last.plot.d2.y.axis.dim
f = last.plot.d2.field
f.matrix(0:xdim,0:ydim) = MEDIAN( f.matrix(0:xdim,0:ydim), 3 )
f.loc(2) = 'smoothed'   & last.plot.d2.field = f   &   redo_plot

END

;======================================================================

PRO days0date, days0, date
;--------------------------------------------------------------
;	... Convert incoming time in days since 0/0/0 to date in days,
;   months, years. Tis routine does not take leap years into account
;--------------------------------------------------------------

date = { datetype, month:'', day:0, year:0 }
mon = [ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ]
month_name = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', $
                 'Sep', 'Oct', 'Nov', 'Dec' ]

date.year = (days0 - 1) / 365
jul_day = ((days0 - 1) MOD 365 ) + 1
For i = 12,1,-1 do Begin
    if (jul_day le mon(i)) then month_nb = i
EndFor
month_nb = month_nb > 1
date.month = month_name(month_nb-1)
date.day = jul_day - mon(month_nb-1)

END

;======================================================================

PRO calc_utils
END
