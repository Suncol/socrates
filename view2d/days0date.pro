pro days0date, days0, date
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

return
end
