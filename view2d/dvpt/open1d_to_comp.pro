pro open1d_to_comp

common arch_info2	; out : arch_flnm2, cdfid2, glob2, glob_attr2,
			;	nbfields2, depthlist2, varlist2
common axes2		; out : x2, y2, depth2
common select_id	; out : comp_oned
common tot_dens_com	; in : totdensOK, dz
common tot_dens_com2	; out : totdensOK2, totdens2


;------------------- Get name of NetCDF file and open it -------------------

arch_flnm2 = PICKFILE(TITLE='View2D : Select ONED NetCDF Archive to Open',   	$
                                  FILTER='*.nc', path='/unicos_tmp/simonc/oned')
if (arch_flnm2 eq '') then return


cdfid2 = ncdf_open(arch_flnm2,/NOWRITE)	; Open the file
glob2 = ncdf_inquire( cdfid2 )		; Find out general info

;------------------- Get global attributes of NetCDF file -------------------

If (glob2.ngatts gt 2) then Begin
   glob_attr2 = strarr(glob2.ngatts-2)
   For j=0, glob2.ngatts-3 do begin
      attname = ncdf_attname(cdfid2, j+2, /GLOBAL)      
      ncdf_attget, cdfid2, attname, attvalue, /GLOBAL
      glob_attr2(j) = string(attvalue)
   EndFor
; print, 'Global attributes of ONED NetCDF archive :'
; print, glob_attr2
EndIf

;------------------- Get x and y axes ----------------------------------------

ncdf_diminq, cdfid2, 0, dumb, xdim
x2 = { name:'', units:'', dim:xdim, coord:fltarr(xdim), depthdep:0 }
info = ncdf_varinq(cdfid2, 0)
x2.name = info.name
For j=0, info.natts-1 do begin	; Get attributes assoc w/ variable
    attname = ncdf_attname(cdfid2,0,j)
    If (attname eq 'units') then ncdf_attget, cdfid2, 0, attname, attvalue
EndFor
x2.units = string(attvalue)
If (info.ndims gt 1) then Begin
   If (info.ndims gt 2) or (info.dim(1) ne 2) then Begin
      msg = ['This ONED archive has an X-axis with more than one dimension...',	$
             '..but I do not understand its structure !',	$
             'Failed do open '+arch_flnm2]
      junk = WIDGET_MESSAGE(msg, /ERROR)
      return
   EndIf Else x2.depthdep = 1	; X-axis is depth-dependent : applies to ONED model
EndIf
If not x2.depthdep then Begin   
   ncdf_varget, cdfid, 0, xcoord
   x2.coord(0:x2.dim-1) = xcoord
EndIf
; print, 'ONED axis X :'
; help, /structure, x2

ncdf_diminq, cdfid2, 1, dumb, ydim
y2 = { name:'', units:'', dim:ydim, coord:fltarr(ydim) }
info = ncdf_varinq(cdfid2, 1)
y2.name = info.name
if (y2.name eq 'levels') then y2.name = 'approx altitude'
For j=0, info.natts-1 do begin	; Get attributes assoc w/ variable
    attname = ncdf_attname(cdfid2,1,j)
    If (attname eq 'units') then ncdf_attget, cdfid2, 1, attname, attvalue
EndFor
y2.units = string(attvalue)
if (y2.units eq 'kilometers') then y2.units = 'km'
ncdf_varget, cdfid2, 1, ycoord
y2.coord(0:y2.dim-1) = ycoord
; print, 'ONED axis Y :'
; help, /structure, y2

;----------------- Get depth (time) axis ------------------------------

depth2 = { name:'', units:'', dim:0, coord:fltarr(730) }
ncdf_diminq, cdfid2, 2, dumb, depth_dim
depth2.dim = depth_dim
info = ncdf_varinq(cdfid2, 2)
depth2.name = info.name
For j=0, info.natts-1 do begin	; Get attributes assoc w/ variable
    attname = ncdf_attname(cdfid2,2,j)
    If (attname eq 'units') then ncdf_attget, cdfid2, 2, attname, attvalue
EndFor
depth2.units = string(attvalue)
ncdf_varget, cdfid2, 2, depth_coord
depth2.coord(0:depth2.dim-1) = depth_coord
; print, 'ONED axis DEPTH :'
; help, /structure, depth

;--- Get list of time-dependent variables contained in NetCDF file --------

nbfields2 = glob2.nvars - 3
varlist2 = strarr(nbfields2)	; Now tell user about the variables
varlistunits = strarr(nbfields2)
For i = 0, nbfields2-1 do begin
    iarch = i + 3
    info = ncdf_varinq(cdfid2, iarch)
    varlist2(i) = info.name
Endfor
   
;------ Convert the depth coordinates to mm/dd/yyyy julian dates -----------

If (depth2.units eq 'days') and (depth2.coord(0) gt 6e5) then Begin
     depthlist2 = strarr(depth2.dim)
     FmtStr = '(A3,"/",I2,"/",I4)'
     For i = 0, depth2.dim-1 Do Begin
	days0date, depth2.coord(i), date
        depthlist2(i) = string(date.month,date.day,date.year,FORMAT=FmtStr)
    EndFor
EndIf

comp_oned = 1
if not totdensOK then return

;------------- Calculate total number density from temperature ------------

totdensOK2 = 0
For i = 0, nbfields2-1 Do Begin
   If (varlist2(i) eq 'temperature') then Begin
      totdensOK2 = 1
      t_id = i + 3
   EndIf
EndFor
If totdensOK2 then Begin
   WIDGET_CONTROL, /HOURGLASS
   totdens2 = fltarr(x2.dim,y2.dim,depth2.dim)
   For d = 0, depth2.dim-1 Do Begin
      ncdf_varget, cdfid2, t_id, temp, OFFSET=[0,0,d], COUNT=[x2.dim,y2.dim,1]
      For i = 0, x2.dim-1 Do 						       $
         totdens2(i,*,d) = 7.34e21 * EXP(-y2.coord(0:y2.dim-1)*(dz/1e5)/7.)    $
                          / temp(i,*)
   EndFor
 EndIf Else Begin
   msg = ['No "temperature" variable found in this archive',	$
	  'Not able to calculate total number densities',	$
	  'Not able to convert volume mixing ratios (vmr) to number densities (molec/cm3)',	$
	  'Do not switch between "vmr" and "dens" !!']
   warning = WIDGET_MESSAGE(msg)
EndElse

return
end
