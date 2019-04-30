pro savedater

print,'Give path and name of NetCDF Restart file'
prov_flnm=''   &   read, prov_flnm

arch_flnm0 = prov_flnm
cdfid0 = ncdf_open(arch_flnm0,/NOWRITE)	; Open the file
glob0 = ncdf_inquire( cdfid0 )		; Find out general info

ncdf_varget, cdfid0, 2, days0
days0date, days0, date
print, 'NetCDF Restart file '+arch_flnm0+' for date : '
print, 'month : ', date.month
print, 'day   : ', date.day
print, 'year  : ', date.year

return
end
