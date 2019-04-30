pro archopen, flnm=flnm, nowin=nowin, arch_to_comp=arch_to_comp, ref=ref

common path_com         ; in : local_path, remote_path, other_path
common maxcom           ; in : maxdimx, maxdimy, maxdimdepth, maxvars, maxldesc
common arch_info        ; out : arch *OR* arch2 *OR* arch3, arch_to_compOK, arch_refOK
common axes             ; out : x, y, yp, depth
common axes2            ; out : x2, y2, depth2
common axes3            ; out : x3, y3, depth3
common dataranges       ; out : idp0, idp1 *OR* idp2_0, idp2_1 *OR* idp3_0, idp3_1
common last_com         ; out : last.action
common formats          ; in : Dfmt

axes_arch_null, x0, y0, yp, ygeo0, depth0, arch0    ; routine in initialize.pro

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
   print,'path= >'+path+'<'
   prov_flnm = DIALOG_PICKFILE(TITLE='view2d : Select NetCDF file to Open',  $
                                                FILTER='*.nc', PATH=path)
EndElse

If (prov_flnm eq '') then Begin
   If arch0.flnm eq '' then Begin
      print, 'ERROR : no new NetCDF Archive file to open ! View2D aborts !'
      last.action = 'exit'   &   return
    EndIf Else If KEYWORD_SET(arch_to_comp) then Begin
      arch_to_compOK = 0   &   return
    EndIf Else If KEYWORD_SET(ref) then Begin
      arch_refOK = 0   &   return
    EndIf Else Begin
      msg = ['No new NetCDF Archive file to open !',    $
             'To exit, choose QUIT from the file menu']
      junk = DIALOG_MESSAGE( msg )   &   return
   EndElse
EndIf

desc = STRARR( 6 )
arch0.flnm = prov_flnm
arch0.cdfid = NCDF_OPEN( arch0.flnm, /NOWRITE )       ; Open the file
glob0 = NCDF_INQUIRE( arch0.cdfid )                   ; Find out general info

;------------------- Get global attributes of NetCDF file -------------------

WIDGET_CONTROL, /HOURGLASS

If glob0.ngatts gt 2 then Begin
   For j=0, glob0.ngatts-3 do begin
      attname = NCDF_ATTNAME( arch0.cdfid, j+2, /GLOBAL )
      arch0.attr(j,0) = attname
      NCDF_ATTGET, arch0.cdfid, attname, attvalue, /GLOBAL
      arch0.attr(j,1) = STRING( attvalue )
   EndFor
EndIf
; print, 'Global attributes of NetCDF archive :'
; print, arch0.attr

;------------------- Check missing value attribute ---------------------------
arch0.OK.missval = 0B
arch0.missval = -9.e29
For j=0, glob0.ngatts-3 do Begin
    If arch0.attr(j,0) eq 'missing_value' then Begin
       arch0.OK.missval = 1B
       arch0.missval = FLOAT( arch0.attr(j,1) )
    EndIf
EndFor

;------------------- Get x and y axes ----------------------------------------

NCDF_DIMINQ, arch0.cdfid, 0, dumb, x0dim
If x0dim gt maxdimx then Begin
   desc(0) = '0, LABEL, The netCDF file you opened has an axis X of dim'   $
           +strtrim(x0dim,2)
   desc(1) = '0, LABEL, Increase maxdimx in preferences.pro'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf
;If (N_ELEMENTS(x) gt 0) and not KEYWORD_SET(arch_to_comp) then Begin
;   If (x.dim ne x0dim) then Begin
;      desc(0) = '0, LABEL, The dimension for axis X in this new archive...'
;      desc(1) = '0, LABEL, ...is not he same than in the previous archive !'
;      last.action = 'problem'   &   GOTO, PROBLEM
;   EndIf
;EndIf

x0.dim = x0dim   &   x0.i0 = 0   &   x0.i1 = x0.dim - 1
x0info = NCDF_VARINQ( arch0.cdfid, 0 )
x0.name = x0info.name
x0.units = NCDF_UNITSGET( arch0.cdfid, 0 )
If (x0info.ndims gt 1) and not KEYWORD_SET(arch_to_comp) then Begin
   If (x0info.ndims gt 2) or (x0info.dim(1) ne 2) then Begin
      desc(0) = '0, LABEL, Axis X has more than one dimensions...'
      desc(1) = '0, LABEL, ...I do not understand this structure !'
      last.action = 'problem'   &   GOTO, PROBLEM
   EndIf
EndIf
NCDF_VARGET, arch0.cdfid, 0, x0coord
x0.coord(0:x0.dim-1) = x0coord
; print, 'axis X :'
; help, /structure, x0

NCDF_DIMINQ, arch0.cdfid, 1, dumb, y0dim
If y0dim gt maxdimy then Begin
   desc(0) = '0, LABEL, The netCDF file you opened has an axis Y of dim'   $
           +strtrim(y0dim,2)
   desc(1) = '0, LABEL, Increase maxdimy in preferences.pro'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf
;If (N_ELEMENTS(y) gt 0) and not KEYWORD_SET(arch_to_comp) then Begin
;   If (y.dim ne y0dim) then Begin
;      desc(0) = '0, LABEL, The dimension for axis Y in this new archive...'
;      desc(1) = '0, LABEL, ...is not he same than in the previous archive !'
;      last.action = 'problem'   &   GOTO, PROBLEM
;   EndIf
;EndIf

y0.dim = y0dim   &   y0.i0 = 0   &   y0.i1 = y0.dim - 1
info = NCDF_VARINQ( arch0.cdfid, 1 )
y0.name = info.name
If (y0.name eq 'levels') then y0.name = 'log-p altitude'
y0.units = NCDF_UNITSGET( arch0.cdfid, 1 )
if (y0.units eq 'kilometers') then y0.units = 'km'
NCDF_VARGET, arch0.cdfid, 1, y0coord
y0.coord(0:y0.dim-1) = y0coord
; print, 'axis Y :'
; help, /structure, y0

;----------------- Get depth (time) axis ------------------------------

dimname = '?'
if glob0.ndims ge 3 then NCDF_DIMINQ, arch0.cdfid, 2, dimname, depth0_dim

if dimname ne 'time' then Begin
   info = NCDF_VARINQ( arch0.cdfid, 2 )
   If info.name eq 'save_time' then Begin
      depth0.name = info.name   &   depth0.dim = 1
      depth0.units = 'days'   &   depth0.type = 'days0'
      GOTO, GET_DEPTH_COORD
    EndIf Else Begin
      desc(0) = '0, LABEL, The 3rd dimension is not named "time" !'
      desc(1) = '0, LABEL, view2d can not read this netCDF file'
      last.action = 'problem'   &   GOTO, PROBLEM
   EndElse
EndIf
If (depth0_dim eq 0) then Begin
   desc(0) = '0, LABEL, The dimension for depth axis is ZERO!'
   desc(1) = '0, LABEL, This netCDF archive is probably empty.'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf
If depth0_dim gt maxdimdepth then Begin
   desc(0) = '0, LABEL, The netCDF file you opened has an time (depth) dim'   $
           +strtrim(depth0_dim,2)
   desc(1) = '0, LABEL, Increase maxdimdepth in preferences.pro'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf
depth0.dim = depth0_dim  &   depth0.i0 = 0   &   depth0.i1 = depth0.dim - 1
info = NCDF_VARINQ( arch0.cdfid, 2 )
depth0.name = info.name

GET_DEPTH_COORD:
NCDF_VARGET, arch0.cdfid, 2, depth0_coord
If depth0.dim gt 1 then Begin
   For idp = 1, depth0.dim-1 Do Begin
      If depth0_coord(idp) lt depth0_coord(idp-1) then Begin
         msg = ['The time (depth) axis is not monotously increasing',                 $
                'depth.coord('+STRTRIM(idp-1,2)+')= '+STRTRIM(depth0_coord(idp-1),2), $
                'depth.coord('+STRTRIM(idp,2)+')= '+STRTRIM(depth0_coord(idp),2),     $
                'Keeping only dates before idp= '+STRTRIM(idp,2) ]
         junk = DIALOG_MESSAGE( msg )
         depth0.dim = idp   &   GOTO, CHECKED_DEPTH
      EndIf
   EndFor
EndIf
CHECKED_DEPTH:
depth0.coord(0:depth0.dim-1) = depth0_coord(0:depth0.dim-1)

;--- Find if time is simulation date (in days since 0/0/0) or if there is ----
;---------------- one sim date in a global attribute -------------------------

depth0.units = NCDF_UNITSGET( arch0.cdfid, depth0.name )
junk = WHERE( depth0.coord(0:depth0.dim-1) gt 1000.*365., count )
If depth0.units eq 'days since 0/0/0, no leap years' or           $
                                           count eq depth0.dim  then Begin
   depth0.units = 'days'   &   depth0.type = 'days0'
 EndIf Else if depth0.units eq 'solar local time, hours' then Begin
   depth0.units = 'hours'   &   depth0.type = 'slt'
EndIf Else depth0.type = '???'

If depth0.type ne 'days0' then Begin
   For j=0, glob0.ngatts-3 do If arch0.attr(j,0) eq 'sim_days0' then    $
          depth0.coord(depth0.dim) = FLOAT( arch0.attr(j,1) ) ; put after last coord
EndIf

; print, 'axis DEPTH :'
; help, /structure, depth0

;------ Convert the depth coordinates to mm/dd/yyyy julian dates -----------

make_depthlist, depth0, depthlist0   &   arch0.list.depth = depthlist0
polardn, depth0, latpn0   &   arch0.latpn = latpn0

arch0.OK.molev = depth0.dim gt 6
if depth0.type eq 'days0' then arch0.OK.molev = arch0.OK.molev and                 $
              (depth0.coord(depth0.dim-1) - depth0.coord(0) lt 150.*365.)
arch0.OK.molat = arch0.OK.molev and                                       $
              ((x0.name eq 'latitudes') or (x0.name eq 'latitude'))

;------ Take care of the description var and dims (SOCRATES v5s21) -----------

arch0.OK.desc = 0B   &   arch0.desc = STRARR( maxldesc )
For i = 0, glob0.nvars-1 Do Begin
   info = NCDF_VARINQ( arch0.cdfid, i )
   If info.name eq 'description'  then Begin
      For j = 0, glob0.ndims-1 Do Begin
         NCDF_DIMINQ, arch0.cdfid, j, name, nltext
         If name eq 'nltext' then Begin
            If nltext gt maxldesc then Begin
               msg = [ 'Too many lines in description of the file',            $
                       'Keeping only the '+STRARR(maxldesc,2)+' first lines' ]
              junk = DIALOG_MESSAGE( msg )   &   nltext = maxldesc
            EndIf
            arch0.OK.desc = 1B   &   NCDF_VARGET, arch0.cdfid, i, prov
            arch0.desc(0:nltext-1) = STRING( prov )
            GOTO, LAST_CHECK
         EndIf
      EndFor
   EndIf
EndFor

LAST_CHECK:
desc = strarr(6)
If not arch0.OK.desc and (glob0.ndims ne 3) and            $
                             not (depth0.name eq 'save_time') then Begin
   desc(0) = '0, LABEL, The netCDF file you opened has '   $
           +strtrim(glob0.ndims,2)+' dimensions.'
   desc(1) = '0, LABEL, view2d can only open files with 3 dimensions'
   last.action = 'problem'   &   GOTO, PROBLEM
EndIf

;--- Get list of 2 or 3-dim variables contained in NetCDF file --------

ivar = 0
For i = 0, glob0.nvars-1 do Begin
    info = NCDF_VARINQ( arch0.cdfid, i )
    If info.datatype ne 'CHAR' and                                $
              (info.ndims eq 2 or info.ndims eq 3) then Begin
       ivar = ivar + 1
       If ivar gt maxvars then Begin
          desc(0) = '0, LABEL, The netCDF file you opened has '   $
                +strtrim(arch0.list.nbvars,2)+' variables'
          desc(1) = '0, LABEL, Increase maxvars in preferences.pro'
          last.action = 'problem'   &   GOTO, PROBLEM
       EndIf
       arch0.list.vars(ivar-1) = info.name
     EndIf Else print,'Variable rejected from list: ',info.name
Endfor
arch0.list.nbvars = ivar

;------------------ Check and read total number density ----------------

arch0.OK.totdens = 0B
For i = 0, arch0.list.nbvars-1 Do Begin
   If not arch0.OK.totdens and (arch0.list.vars(i) eq 'totdens') then Begin
      var_units = NCDF_UNITSGET( arch0.cdfid, arch0.list.vars(i) )
      If (var_units eq 'molec/cm3') then arch0.OK.totdens = 1B
   EndIf
EndFor
If arch0.OK.totdens then Begin
   info = NCDF_VARINQ( arch0.cdfid, 'totdens')
   If info.ndims eq 3 then Begin               ; usual case: totdens is time-dep
      NCDF_VARGET, arch0.cdfid, 'totdens', totdens0, OFFSET=[0,0,0],       $
                                     COUNT=[x0.dim,y0.dim,depth0.dim]
      arch0.totdens(0:x0.dim-1,0:y0.dim-1,0:depth0.dim-1) = totdens0
    EndIf Else If info.ndims eq 2 then Begin    ; new dvout case (SOCRATES v6s38)
      NCDF_VARGET, arch0.cdfid, 'totdens', totdens0, OFFSET=[0,0],       $
                                     COUNT=[x0.dim,y0.dim]
      For idp = 0, depth0.dim-1 Do                                       $
         arch0.totdens(0:x0.dim-1,0:y0.dim-1,idp) = totdens0
    EndIf Else arch0.OK.totdens = 0B
EndIf
If not arch0.OK.totdens then Begin
   msg = ['No usable "totdens" variable found in this archive',         $
          'Not able to convert volume mixing ratios (vmr)...',          $
          ' ...to number densities (molec/cm3)']
   junk = DIALOG_MESSAGE( msg )
   arch0.totdens = FLTARR(maxdimx,maxdimy,maxdimdepth)
EndIf

;------------- Check possibility to calc ygeo from log-p alt ----------------

junk = WHERE( STRMID(arch0.list.vars,0,3) eq 'o3p', count_o )
junk = WHERE( STRMID(arch0.list.vars,0,2) eq 'o2', count_o2 )
junk = WHERE( arch0.list.vars eq 'temperature', count_t2d )
arch0.OK.ygeo = (y0.name eq 'log-p altitude') and (count_o gt 0)            $
                and (count_o2 gt 0) and (count_t2d gt 0)

;----------------------------------------------------------------------------
;-------- Store arch0 parameters in arch, arch2 or arch3       --------------
;----------------------------------------------------------------------------

If KEYWORD_SET(arch_to_comp) then Begin
   arch_to_compOK = 1
   arch2 = arch0
   x2 = x0   &   y2 = y0   &   depth2 = depth0
   idp2_0 = 0   &   idp2_1 = depth2.dim-1
   if arch2.OK.ygeo then ygeo2 = ygeo0
 EndIf Else If KEYWORD_SET(ref) then Begin
   arch_refOK = 1
   arch3 = arch0
   x3 = x0   &   y3 = y0   &   depth3 = depth0
   idp3_0 = 0   &   idp3_1 = depth3.dim-1
   if arch3.OK.ygeo then ygeo3 = ygeo0
 EndIf Else Begin
   arch = arch0
   x = x0   &   y = y0   &   depth = depth0
   depth.i0 = 0   &   depth.i1 = depth.dim-1
   idp0 = depth.i0   &   idp1 = depth.i1     ; to remove ASAP
   time_labels, depth
   If (y0.name eq 'log-p altitude') then Begin     ; create pressure axis
      yp.name = 'pressure'   &   yp.units = 'hPa'
      yp.coord = 1013.25 * EXP( - y0.coord/7. )
    EndIf Else Begin
      yp.units = 'UNKNOWN'   &   yp.coord = fltarr(maxdimy)   &   yp.dim = -1L
   EndElse
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
EndIf

end
