pro vrecdrawer, filename, mode

recmax = 9

openr, 11, filename
yname =''   &   yunits = ''   &    ydim = 0
readf, 11, yname    &    readf, 11, yunits   &   readf, 11, ydim
y = {vertaxe, name:yname, units:yunits, dim:ydim, coord:fltarr(ydim) }
ycoord = y.coord
readf, 11, ycoord
y.coord = ycoord

help, /structure, y
print, y.coord

close, 11
return
end
