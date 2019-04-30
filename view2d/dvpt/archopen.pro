pro archopen, flnm=flnm, nowin=nowin, arch_to_comp=arch_to_comp, ref=ref

common path_com         ; in : local_path, remote_path, other_path
common arch_info        ; out : arch *OR* arch2, arch_to_compOK
common select_id
common axes             ; out : x, y, depth, yp
common axes2            ; out : x2, y2, depth2
common axes3            ; out : x3, y3, depth3
common dataranges       ; out : idp0, idp1 *OR* idp2_0, idp2_1 *OR* idp3_0, idp3_1
common last_com         ; out : last.action
common formats          ; in : Dfmt

If KEYWORD_SET(arch_to_comp) then arch_to_compOK = 0
If KEYWORD_SET(ref) then arch_refOK = 0

;------------------- Get name of NetCDF file and open it -------------------

If KEYWORD_SET(flnm) or KEYWORD_SET(nowin) then Begin
   If not KEYWORD_SET(flnm) then Begin
      print,'Give path and name of NetCDF Archive to Open'
      prov_flnm=''   &   read, prov_flnm
    EndIf Else prov_flnm = flnm
 EndIf Else Begin
   path = local_path
   If (other_path ne '' or remote_path ne '') then Begin
      desc = ['0, BUTTON, '+local_path+', QUIT, TAG=local',             $
              '0, BUTTON, '+other_path+', QUIT, TAG=other',             $
              '2, BUTTON, '+remote_path+', QUIT, TAG=remote'       ]
      form = CW_FORM(desc, /COLUMN, title='Pick path for input NetCDF file')
      if form.remote then path = remote_path                            $
       else if form.other then path = other_path else path = local_path
   EndIf
   prov_flnm = DIALOG_PICKFILE(TITLE='view2d : Select NetCDF file to Open',  $
                                                FILTER='*.nc', PATH=path)
EndElse

If (prov_flnm eq '') then Begin
   If (N_ELEMENTS(arch) le 0) then Begin
      print, 'ERROR : no new NetCDF Archive file to open ! View2D aborts !'
      last.action = 'exit'   &   return
    EndIf Else If KEYWORD_SET(arch_to_comp) then Begin
      arch_to_compOK = 0   &   return
    EndIf Else If KEYWORD_SET(ref) then Begin
      arch_refOK = 0   &   return
    EndIf Else Begin
      msg = ['No new NetCDF Archive file to open !',    $
             'Re-opening the current file',     $
             'To exit, choose QUIT from the file menu']
      warning = WIDGET_MESSAGE(msg)
      prov_flnm = arch.flnm
   EndElse
EndIf

arch_flnm0 = prov_flnm
cdfid0 = ncdf_open(arch_flnm0,/NOWRITE) ; Open the file
glob0 = ncdf_inquire( cdfid0 )          ; Find out general info

desc = strarr(6)
If (glob0.ndims ne 3) then Begin
   desc(0) = '0, LABEL, The netCDF file you opened has '   $
           +strtrim(glob0.ndims,2)+' dimensions. Maybe an initcond.nc file?'
   desc(1) = '0, LABEL, Anyway - view2d can only open files with 3 dimensions'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf

;------------------- Get global attributes of NetCDF file -------------------

WIDGET_CONTROL, /HOURGLASS

diuvar0 = 0   &   nlon0 = -999
If (glob0.ngatts gt 2) then Begin
   glob_attr0 = strarr(glob0.ngatts-2,2)
   For j=0, glob0.ngatts-3 do begin
      attname = ncdf_attname(cdfid0, j+2, /GLOBAL)
      glob_attr0(j,0) = attname
      ncdf_attget, cdfid0, attname, attvalue, /GLOBAL
      glob_attr0(j,1) = string(attvalue)
   EndFor
   If (glob_attr0(0,0) eq 'model_name')                         $
                    and (glob_attr0(0,1) eq 'SOCRATES') then    $
      If (glob_attr0(2,0) eq 'nlon') then Begin    ; test if we really have diur. var.
         For i = 0, glob0.nvars-1 Do Begin
            info = ncdf_varinq( cdfid0, i)
            if info.name eq 'SLT' then diuvar0 = 1
         EndFor
         If diuvar0 then Begin
            ON_IOERROR, CONTINUE
            nlon0 = FIX( glob_attr0(2,1) )
            glob_attr0(2,1) = 'nlon=' + glob_attr0(2,1)
            ON_IOERROR, NULL
         EndIf
      EndIf
   CONTINUE:
; print, 'Global attributes of NetCDF archive :'
; print, glob_attr0
EndIf

;------------------- Get x and y axes ----------------------------------------

ncdf_diminq, cdfid0, 0, dumb, x0dim
If (N_ELEMENTS(x) gt 0) and not KEYWORD_SET(arch_to_comp) then Begin
   If (x.dim ne x0dim) then Begin
      desc(0) = '0, LABEL, The dimension for axis X in this new archive...'
      desc(1) = '0, LABEL, ...is not he same than in the previous archive !'
      last.action = 'problem'   &   GOTO, PROBLEM
   EndIf
EndIf

x0 = { name:'', units:'', dim:x0dim, coord:fltarr(x0dim) }
x0info = ncdf_varinq(cdfid0, 0)
x0.name = x0info.name
x0.units = ncdf_unitsget( cdfid0, 0 )
If (x0info.ndims gt 1) and not KEYWORD_SET(arch_to_comp) then Begin
   If (x0info.ndims gt 2) or (x0info.dim(1) ne 2) then Begin
      desc(0) = '0, LABEL, Axis X has more than one dimensions...'
      desc(1) = '0, LABEL, ...but I do not understand its structure !'
      last.action = 'problem'   &   GOTO, PROBLEM
   EndIf 
EndIf
ncdf_varget, cdfid0, 0, x0coord
x0.coord(0:x0.dim-1) = x0coord
; print, 'axis X :'
; help, /structure, x0

ncdf_diminq, cdfid0, 1, dumb, y0dim
If (N_ELEMENTS(y) gt 0) and not KEYWORD_SET(arch_to_comp) then Begin
   If (y.dim ne y0dim) then Begin
      desc(0) = '0, LABEL, The dimension for axis Y in this new archive...'
      desc(1) = '0, LABEL, ...is not he same than in the previous archive !'
      last.action = 'problem'   &   GOTO, PROBLEM
   EndIf
EndIf

y0 = { name:'', units:'', dim:y0dim, coord:fltarr(y0dim) }
info = ncdf_varinq(cdfid0, 1)
y0.name = info.name
If (y0.name eq 'levels') then y0.name = 'log-p altitude'
y0.units = ncdf_unitsget( cdfid0, 1 )
if (y0.units eq 'kilometers') then y0.units = 'km'
ncdf_varget, cdfid0, 1, y0coord
y0.coord(0:y0.dim-1) = y0coord
; print, 'axis Y :'
; help, /structure, y0

;----------------- create pressure axis ------------------------------

If (y0.name eq 'log-p altitude') and not KEYWORD_SET(arch_to_comp) then Begin
   yp = y0   &   yp.name = 'pressure'   &   yp.units = 'mb'
   yp.coord = 1013.25 * EXP( - y0.coord/7. )
EndIf

;----------------- Get depth (time) axis ------------------------------

label = {axislabels, name:'', nb : 0, txt : strarr(30), coord : fltarr(30) }
depth0 = { name:'', units:'', dim:0, coord:fltarr(730), label:label }
ncdf_diminq, cdfid0, 2, dumb, depth0_dim
If (depth0_dim eq 0) then Begin
   desc(0) = '0, LABEL, The dimension for depth axis is ZERO!'
   desc(1) = '0, LABEL, This netCDF archive is probably empty.'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf
depth0.dim = depth0_dim
info = ncdf_varinq(cdfid0, 2)
depth0.name = info.name
depth0.units = ncdf_unitsget( cdfid0, 2 )
ncdf_varget, cdfid0, 2, depth0_coord
depth0.coord(0:depth0.dim-1) = depth0_coord
; print, 'axis DEPTH :'
; help, /structure, depth0

;--- Get list of time-dependent variables contained in NetCDF file --------

nbvars0 = glob0.nvars - 3
if (diuvar0) then nbvars0 = glob0.nvars - 4
varlist0 = strarr(nbvars0)      ; Now tell user about the variables
For i = 0, nbvars0-1 do begin
    iarch = i + 3
    if (diuvar0) then iarch = i + 4
    info = ncdf_varinq(cdfid0, iarch)
    varlist0(i) = info.name
Endfor

;------ Convert the depth coordinates to mm/dd/yyyy julian dates -----------

make_depthlist, depth0, depth_is_date0, depthlist0
if depth_is_date0 then polardn, depth0, latpn0
molatOK0 = (depth0.dim gt 6) and depth_is_date0 and not diuvar0 and       $
           (depth0.coord(depth0.dim-1) - depth0.coord(0) lt 50*365) and   $
           ((x0.name eq 'latitudes') or (x0.name eq 'latitude'))
molevOK0 = (depth0.dim gt 6) and depth_is_date0 and not diuvar0 and       $
           (depth0.coord(depth0.dim-1) - depth0.coord(0) lt 50*365)
If (depth0.dim gt 730) then Begin
   msg = ['Axis depth (time) in this archive has a dimension > 730 !',  $
          'The month-latitude or month-level 2D plots will not be available']
   if (N_ELEMENTS(nowin) eq 0) then warning = WIDGET_MESSAGE(msg)
   molatOK0 = 0
   molevOK0 = 0
EndIf

;------------------ Check and read total number density ----------------

totdensOK0 = 0
For i = 0, nbvars0-1 Do Begin
   If not totdensOK0 and (varlist0(i) eq 'totdens') then Begin
      var_units = NCDF_UNITSGET( cdfid0, varlist0(i) )
      If (var_units eq 'molec/cm3') then totdensOK0 = 1
   EndIf
EndFor
If (not totdensOK0) then Begin
   msg = ['No "totdens" variable found in this archive',                $
          'Not able to convert volume mixing ratios (vmr)...',          $
          ' ...to number densities (molec/cm3)']
   if (N_ELEMENTS(nowin) eq 0) then warning = WIDGET_MESSAGE(msg)
   totdens0 = 0.
 EndIf Else                                                             $
   ncdf_varget, cdfid0, 'totdens', totdens0, OFFSET=[0,0,0],            $
                                     COUNT=[x0.dim,y0.dim,depth0.dim]

;------------- Check possibility to calc ygeo from lo-g alt ----------------

junk = WHERE( varlist0 eq 'o3p', count_o )
junk = WHERE( varlist0 eq 'o2', count_o2 )
junk = WHERE( varlist0 eq 'temperature', count_t2d )
ygeoOK0 = (y0.name eq 'log-p altitude') and (count_o gt 0)            $
                and (count_o2 gt 0) and (count_t2d gt 0)
If ygeoOK0 then                                                       $
   ygeo0 = { name:'geometric altitude', units:'km',                   $
             matrix:fltarr(x0.dim,y0.dim) }

;----------------------------------------------------------------------------
;-------- Store arch0 parameters in arch, or in arch2 if arch_to_compOK -----
;--------      THE MAIN STRUCTURES arch AND arch2 ARE DEFINED HERE      -----
;----------------------------------------------------------------------------

ok0 = { diuvar:diuvar0, molev:molevOK0, molat:molatOK0, date:depth_is_date0, $
        totdens:totdensOK0, ygeo:ygeoOK0 }

list0 = { depth:depthlist0, nbvars:nbvars0, vars:varlist0 }

If not KEYWORD_SET(arch_to_comp) and not KEYWORD_SET(ref) then Begin

   arch = { flnm:arch_flnm0, cdfid:cdfid0, glob:glob0, attr:glob_attr0,     $
            nlon:nlon0, ok:ok0, list:list0, totdens:totdens0, latpn:latpn0 }
   x = x0   &   y = y0   &   depth = depth0
   idp0 = 0   &   idp1 = depth.dim-1
   if arch.OK.ygeo then ygeo = ygeo0
   if arch.OK.molat or arch.OK.molev then time_labels, depth

 EndIf Else Begin

   index = where(x.coord ne x0.coord, count)
   If (count ne 0) then Begin
      msg = ['archopen : this file can not be compared',                    $
             'with '+arch_flnm0+' because axis X is different',             $
             'Failed to open '+arch_flnm0]
      error = WIDGET_MESSAGE(msg, /ERROR)
      return
   EndIf
   index = where(y.coord ne y0.coord, count)
   If (count ne 0) then Begin
      msg = ['archopen : this file can not be compared',                    $
             'with '+arch_flnm0+' because axis Y is different',             $
             'Failed to open '+arch_flnm0]
      error = WIDGET_MESSAGE(msg, /ERROR)
      return
   EndIf
   reason = ''
   if (arch.glob.ndims ne glob0.ndims) then reason = 'different nb of dims'
   if (arch.glob.recdim ne glob0.recdim) then reason = 'different recdim'
   if (arch.ok.diuvar ne diuvar0) then reason = 'only one has diurnal variation'
   if (arch.ok.date ne depth_is_date0) then                                 $
      reason = 'only one has 3rd dim in days since 0/0/0'
   if (arch.ok.totdens ne totdensOK0) then reason = 'only one has "totdens" var'
   If (reason ne '') then Begin
      msg = ['archopen : this file can not be compared',                    $
             'with '+arch.flnm+' for the following reason :',               $
             reason, 'Failed to open '+arch_flnm0]
      error = DIALOG_MESSAGE(msg, /ERROR)
      return
   EndIf
   
   If KEYWORD_SET(arch_to_comp) then Begin
      arch_to_compOK = 1
      arch2 = { flnm:arch_flnm0, cdfid:cdfid0, glob:glob0, attr:glob_attr0,    $
                nlon:nlon0, ok:ok0, list:list0, totdens:totdens0, latpn:latpn0 }
      x2 = x0   &   y2 = y0   &   depth2 = depth0
      idp2_0 = 0   &   idp2_1 = depth2.dim-1
      if arch2.OK.ygeo then ygeo2 = ygeo0
      if (TOTAL(depth.coord eq depth2.coord) eq depth.dim) and                 $
       (depth2.dim eq depth.dim)  then d2_ne_d = 0 else d2_ne_d = 1 
      if (TOTAL(arch.list.vars eq arch2.list.vars) eq arch.list.nbvars) and                 $
       (arch2.list.nbvars eq arch.list.nbvars)  then v2_ne_v = 0 else v2_ne_v = 1 
   EndIf Else If KEYWORD_SET(ref) then Begin
      arch_refOK = 1
      arch3 = { flnm:arch_flnm0, cdfid:cdfid0, glob:glob0, attr:glob_attr0,    $
                nlon:nlon0, ok:ok0, list:list0, totdens:totdens0, latpn:latpn0 }
      x3 = x0   &   y3 = y0   &   depth3 = depth0
      idp3_0 = 0   &   idp3_1 = depth3.dim-1
      if arch3.OK.ygeo then ygeo3 = ygeo0
      if (TOTAL(depth.coord eq depth3.coord) eq depth.dim) and                 $
       (depth3.dim eq depth.dim)  then d3_ne_d = 0 else d3_ne_d = 1 
      if (TOTAL(arch.list.vars eq arch3.list.vars) eq arch.list.nbvars) and                 $
       (arch3.list.nbvars eq arch.list.nbvars)  then v3_ne_v = 0 else v3_ne_v = 1 
   EndIf

EndElse


PROBLEM :
If (last.action eq 'problem') then Begin
   If KEYWORD_SET(arch_to_comp) or KEYWORD_SET(ref) then Begin
      desc(3) = '0, LABEL, Opening of Background/Reference netCDF file failed.'
      desc(3) = '1, BASE,, ROW, CENTER'
      desc(4) = '0, BUTTON, OK, QUIT, TAG=ok'
      form = CW_FORM(desc(0:4), /COLUMN, title='! ERROR !')
      return
   EndIf
   desc(2) = '1, BASE,, COLUMN, CENTER'
   desc(3) = '0, BUTTON, Open this file with ncbrowse, QUIT, TAG=ncbrowse'
   desc(4) = '0, BUTTON, Open another file with view2d, QUIT, TAG=another'
   desc(5) = '0, BUTTON, Exit, QUIT, TAG=exit'
   form = CW_FORM(desc, /COLUMN, title='! ERROR !')
   If form.ncbrowse then last.action = 'ncbrowse'       $
    Else If form.another then last.action = 'archopen'  $
    Else last.action = 'exit'
   arch = { flnm:arch_flnm0, cdfid:cdfid0, glob:glob0 }
EndIf

return
end
