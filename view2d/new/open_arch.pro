pro open_arch, flnm=flnm ; OOP: this could become a method for "archstruc" class

common path_com            ; in : arch_path1, arch_path2
common max_com             ; in : maxdim, narchmax
common i_com               ; inout : narch, action, ifore ; out : iback, pos
common widgets_info        ; in : wid.main.id ; out : wid.(*).set
common arch_info           ; out : arch(narch)

;=========================================================================
;------------------- Get name of NetCDF file and open it -----------------
;=========================================================================

If KEYWORD_SET( flnm ) then prov_flnm = flnm                                   $
 Else Begin
   path = arch_path1
   If arch_path2 ne '' then Begin
      desc = [ '0, BUTTON, '+arch_path1+', QUIT, TAG=path1',                   $
               '0, BUTTON, '+arch_path2+', QUIT, TAG=path2',                   $
               '2, BUTTON, Cancel, QUIT, TAG=cancel'            ]
      form = CW_FORM(desc, /COLUMN, title='Pick path for input NetCDF file')
      if form.cancel then GOTO, PROBLEM 
      if form.path2 then path = arch_path2 else path = arch_path1
   EndIf
   prov_flnm = DIALOG_PICKFILE( TITLE='view2d : Select NetCDF file to Open',   $
                                FILTER='*.nc', PATH=path )
EndElse

if prov_flnm eq '' then GOTO, problem
desc = strarr(4)
junk = FINDFILE( prov_flnm, COUNT=c )
If c eq 0 then Begin
   desc(0) = 'The netCDF file you specified could not be found.'
   action = 'problem'   &   GOTO, PROBLEM
EndIf

arch0 = {archstruc}
arch0.flnm = prov_flnm
arch0.cdfid = NCDF_OPEN( arch0.flnm, /NOWRITE ) ; Open the file

;=========================================================================
;------------------- Get global attributes of NetCDF file ----------------
;=========================================================================

glob0 = NCDF_INQUIRE( arch0.cdfid )             ; Find out general info

If (glob0.ndims ne 3) then Begin
   desc(0) = 'The netCDF file you opened has '                                 $
           + strtrim(glob0.ndims,2)+' dimensions. Maybe an restart.nc file?'
   desc(1) = 'Anyway - view2d can only open files with 3 dimensions'
   action = 'problem'   &   GOTO, PROBLEM
EndIf

WIDGET_CONTROL, /HOURGLASS

arch0.nlon = -999   &   arch0.ok.diuvar = 0
If (glob0.ngatts gt 2) then Begin
   For j = 0, 2 do begin
      attname = NCDF_ATTNAME( arch0.cdfid, j+2, /GLOBAL )
      arch0.attr(j,0) = attname
      NCDF_ATTGET, arch0.cdfid, attname, attvalue, /GLOBAL
      arch0.attr(j,1) = STRING( attvalue )
   EndFor
   arch0.ok.socrates = ( arch0.attr(0,1) eq  'SOCRATES' )
   If arch0.ok.socrates and (arch0.attr(2,0) eq 'nlon') then Begin
      ON_IOERROR, CONTINUE
      arch0.nlon = FIX( arch0.attr(2,1) )
      arch0.ok.diuvar = 1
      arch0.attr(2,1) = 'nlon=' + arch0.attr(2,1)
      ON_IOERROR, NULL
   EndIf
; print, 'Global attributes of NetCDF archive :'
; print, arch0.attr
EndIf
CONTINUE:

;=========================================================================
;-- Get x and y axes. OOP: this could become a method for "axis" class ---
;=========================================================================

NCDF_DIMINQ, arch0.cdfid, 0, dumb, this_dim
If this_dim gt maxdim then Begin
   action = 'X'   &   GOTO, TOOBIGDIM 
 EndIf Else arch0.x.dim = this_dim
x0info = NCDF_VARINQ( arch0.cdfid, 0 )
arch0.x.name = x0info.name
arch0.x.units = nf_unitsget( arch0.cdfid, 0 )
If (x0info.ndims gt 1) and not KEYWORD_SET(arch_to_comp) then Begin
   If (x0info.ndims gt 2) or (x0info.dim(1) ne 2) then Begin
      desc(0) = 'Axis X has more than one dimensions...'
      desc(1) = '... but its structure '
      last.action = 'problem'   &   GOTO, PROBLEM
   EndIf 
EndIf
NCDF_VARGET, arch0.cdfid, 0, x0coord
arch0.x.coord(0:arch0.x.dim-1) = x0coord
; print, 'axis X :'
; help, /structure, arch0.x

NCDF_DIMINQ, arch0.cdfid, 1, dumb, this_dim
If this_dim gt maxdim then Begin
   action = 'Y'   &   GOTO, TOOBIGDIM 
 EndIf Else arch0.y.dim = this_dim
info = NCDF_VARINQ( arch0.cdfid, 1 )
arch0.y.name = info.name
if arch0.ok.socrates and (arch0.y.name eq 'levels') then                       $
   arch0.y.name = 'log-p altitude'
arch0.y.units = nf_unitsget( arch0.cdfid, 1 )
if arch0.y.units eq 'kilometers' then arch0.y.units = 'km'
NCDF_VARGET, arch0.cdfid, 1, y0coord
arch0.y.coord(0:arch0.y.dim-1) = y0coord
; print, 'axis Y :'
; help, /structure, arch0.y

;=========================================================================
;---------- If possible, create 2d vertical axis -------------------------
;=========================================================================

arch0.ok.y2p = 0
If arch0.y.name eq 'log-p altitude' then Begin
   arch0.ok.y2p = 1   &   arch0.y2 = arch0.y
   arch0.y2.name = 'pressure'   &   arch0.y2.units = 'mb'
   arch0.y2.coord = 1013.25 * EXP( - arch0.y.coord / 7. )
EndIf

;=========================================================================
;---------------------- Get d (time) axis --------------------------------
;=========================================================================

NCDF_DIMINQ, arch0.cdfid, 2, dumb, this_dim
If this_dim eq 0 then Begin
   desc(0) = 'The dimension for 3rd (time) axis is ZERO!'
   desc(1) = 'This netCDF archive is probably empty.'
   last.action = 'problem'   &   GOTO, PROBLEM
 EndIf Else If this_dim gt maxdim then Begin
   action = 'time (3rd axis)'   &   GOTO, TOOBIGDIM 
EndIf Else arch0.d.dim = this_dim

info = NCDF_VARINQ( arch0.cdfid, 2 )
arch0.d.name = info.name
arch0.d.units = nf_unitsget( arch0.cdfid, 2 )
NCDF_VARGET, arch0.cdfid, 2, d0_coord
arch0.d.coord(0:arch0.d.dim-1) = d0_coord
; print, 'axis D :'
; help, /structure, arch0.d

;=========================================================================
;-------- Get list of time-dependent variables and their units -----------
;=========================================================================

arch0.nbvars = glob0.nvars - 3
if arch0.ok.diuvar then arch0.nbvars = glob0.nvars - 4
For i = 0, arch0.nbvars-1 do Begin
    iarch = i + 3
    if arch0.ok.diuvar then iarch = i + 4
    info = NCDF_VARINQ( arch0.cdfid, iarch )
    arch0.var(i) = info.name
    arch0.units(i) = nf_unitsget( arch0.cdfid, iarch )
Endfor

;=========================================================================
;------ Convert the depth coordinates to mm/dd/yyyy julian dates -----------
;=========================================================================

make_dstring, arch0
if arch0.ok.d_is_date then polardn, arch0
arch0.ok.d_as_xplot = (arch0.d.dim gt 6) and not arch0.ok.diuvar and not       $
                       ( arch0.ok.d_is_date and                                $
                 (arch0.d.coord(arch0.d.dim-1) - arch0.d.coord(0) ge 50*365) )

;=========================================================================
;--------------------- Check for total number density --------------------
;=========================================================================

arch0.ok.ntot = 0
i = WHERE( arch0.var eq 'totdens' )
If (i(0) ge 0) then                                                               $
   arch0.ok.ntot = nf_unitsget(arch0.cdfid,arch0.var(i(0))) eq 'molec/cm3'
If not arch0.ok.ntot then Begin
   msg = ['No "totdens" variable found in this archive',                       $
          'Not able to convert volume mixing ratios (vmr)...',                 $
          ' ...to number densities (molec/cm3)']
   junk = DIALOG_MESSAGE( msg, DIALOG_PARENT=wid.main.id )
EndIf

;=========================================================================
;----------- Commit new archive and initialize pos counters --------------
;=========================================================================

arch(narch) = arch0
pos(narch).ixr = [ 0, arch0.x.dim-1 ]
pos(narch).iyr = [ 0, arch0.y.dim-1 ]
pos(narch).idr = [ 0, arch0.d.dim-1 ]
If (narch gt 0) then Begin
   ivar = pos(ifore).ivar
   if (ivar le arch0.nbvars-1) then                                            $
      if (arch0.var(ivar) eq arch(ifore).var(ivar)) then                       $
         pos(narch).ivar = pos(ifore).ivar
 EndIf Else pos(narch).ivar = 0
; if arch0.ok.d_is_date then time_labels, depth

;=========================================================================
;-------- Set and test file counters ; reset widgets if necessary --------
;=========================================================================

narch = narch +1
wid.list_fore.set = 1   &   wid.list_back.set = 1
If action eq 'open_fore' then Begin
   iback = ifore   &   ifore = narch-1
   xface_update_chgfore
 EndIf Else If action eq 'open_back' then iback = narch-1
if narch eq narchmax then                                                      $
   junk = DIALOG_MESSAGE('Reached max nb of opened files !',DIAL=wid.main.id)
return

;=========================================================================
;------------------- If failed, error display message --------------------
;=========================================================================
TOOBIGDIM :
desc(0) = 'The dimension of axis '+action+' is '+STRTRIM( this_dim,2)
desc(1) = 'The maximum is '+STRTRIM(maxdim,2)+'. To open anyway:'
desc(2) = 'exit IDL, edit file "preferences.pro" and re-run view2d'
action = 'problem'

PROBLEM :
If action eq 'problem' then Begin
   desc(3) = 'Opening netCDF file '+prov_flnm+' failed.'
   junk = DIALOG_MESSAGE( desc, /ERROR, DIALOG_PARENT=wid.main.id )
EndIf
action = 'cancelled'

END
