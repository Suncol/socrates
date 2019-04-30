;+
; NAME: Selecter
;
; PURPOSE:
;       This routine creates and manages the widgets for S.Chabrillat's
;       IDL viewer of 2-dimensional (+time) NetCDF files. Used the templates
;       acd.ucar.edu:/local/rsi/idl_4/lib/xmng_tmpl.pro and
;       acd.ucar.edu:/local/rsi/idl_4/examples/wexmast/mbar.pro and the example
;       program xpcolor.pro from IDL User's guide.
;
; CATEGORY: Widgets.
;
; CALLING SEQUENCE: selecter
;
; INPUTS: most of the commons defined in view2d.pro, and read in archopen.pro
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;       GROUP:  The widget ID of the widget that calls selecter.  When this
;               ID is specified, the death of the caller results in the death
;               of selecter.
;
; OUTPUTS: contour and one-dimensional plots, to screen or specified
;          PostScript files. ASCII files of data extracted from NetCDF archive
;
; OPTIONAL OUTPUT PARAMETERS:
;
; COMMON BLOCKS: most of the commons defined in view2d.pro, and read in
;                archopen.pro. The most important one is arch_info
;
; SIDE EFFECTS: Initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       Created from a template written by: Steve Richards, January, 1991.
;       Written by S.Chabrillat (simonc@acd.ucar.edu), march, 1996
;       Developed by S.Chabrillat (simonc@oma.be) at least up to september, 1998
;-

;------------------------------------------------------------------------------
;       procedure selecter_ev
; This procedure processes the events being sent by the XManager.
;------------------------------------------------------------------------------
PRO selecter_ev, event

common hardware
common arch_info
common select_id
common plots_info
common last_fields
common moving_box
common dataranges
common axes
common axes2
common axes3
common last_com
common movies
common comscale
common label_widgets
common widgets_info

WIDGET_CONTROL, event.id, GET_UVALUE = action           ;find the user value
                                                        ;of the widget where
                                                        ;the event occured
device, window_state = win_exist

;if (z2.compOK) then Begin
;   help, z2.date, /struc   &    help, z2.matrix(ip,jp)
;endif

CASE action OF

;*** here is where you would add the actions for your events.  Each widget
;*** you add should have a unique string for its user value.  Here you add
;*** a case for each of your widgets that return events and take the
;*** appropriate action.

     'quit':       BEGIN
                     wdelete1
                     WIDGET_CONTROL, event.top, /DESTROY
                     last.action = action
                   END
     'archopen' :  BEGIN
                     WIDGET_CONTROL, event.top, /DESTROY   &   wdelete1
                     ncdf_close, arch.cdfid   &   pointed = 0
                     reset_titles = 1
                     fields_null, zl, zmolat, zmolev
                     last.action = action
                   END
     'open_same' : BEGIN
                     if arch_to_compOK then ncdf_close, arch2.cdfid
                     wdelete1   &   arch_to_compOK = 0
                     If (last.plot2d ne 'x-y') then Begin
                        reset_titles = 1
                        archread, arch.list.vars(zl.vid), zl.did, zl
                        last.plot2d = 'x-y'   &   viewpreproc
                     EndIf
                     fields_null, z2, zmolat2, zmolev2
                     archopen, /arch_to_comp   &   find_ids, /read
                     last.action = action
                   END
     'descfgfile': xdisplayfile, height=36, text=arch.desc, title='description of '+arch.flnm
     'closebgfile':BEGIN
                     arch_to_compOK = 0   &   z2.compOK = 0
                     ncdf_close, arch2.cdfid   &   redo_last
                   END
     'open_ref' :  BEGIN
                     if arch_refOK then ncdf_close, arch3.cdfid
                     wdelete1   &   arch_refOK = 0
                     If (last.plot2d ne 'x-y') then Begin
                        reset_titles = 1
                        archread, arch.list.vars(zl.vid), zl.did, zl
                        last.plot2d = 'x-y'   &   viewpreproc
                     EndIf
                     fields_null, z3, zmolat3, zmolev3
                     archopen, /ref  &   find_ids, /ref, /read
                     last.action = action
                   END
     'closerefile':BEGIN
                     arch_refOK = 0   &   z3.compOK = 0
                     ncdf_close, arch3.cdfid   &   redo_last
                   END
     'ncbrowse'  : BEGIN
                     WIDGET_CONTROL, event.top, /DESTROY   &   wdelete1
                     NCDF_CLOSE, arch.cdfid
                     last.action = action
                   END
     'asciiwrite': BEGIN
                     my_device = 'ascii'   &   redo_last, only1d=win_exist(1)   &   my_device = screen
                   END
     'asciiwrite_allchem': BEGIN
                     my_device = 'ascii'   &   last.action = action   &   last.plot2d = 'x-y'   &   keep_vid = zl.vid
                     For i = 0, arch.list.nbvars-1 Do Begin
                         reset_titles = 1
                         archread, arch.list.vars(i), zl.did, zl
                         if zl.units eq 'vmr' then viewpreproc
                     EndFor
                     my_device = screen   &   zl.vid = keep_vid
                     archread, arch.list.vars(zl.vid), zl.did, zl
                     viewpreproc
                   END
     'plot_ps'   : plot_ps
     'plot_eps'  : plot_ps, /encaps
     'plot_cgm'  : plot_cgm
     'plot_gif'  : plot_gif
     'print':      If (!D.name ne 'X') then junk = DIALOG_PRINTJOB() Else plot_ps, /to_print
     'printsetup': junk = DIALOG_PRINTERSETUP()
     'manscaler' : manscaler
     'form_titles':BEGIN
                     last.action = action   &   form_titles
                   END
     'chgaxes'   : BEGIN
                     desc = strarr(9)
                     desc(0) = '0, TEXT,'+x.name+',  LABEL_LEFT=Axis x  - name :, WIDTH=20, TAG=xname'
                     desc(1) = '0, TEXT,'+x.units+', LABEL_LEFT=Axis x  - units:, WIDTH=20, TAG=xunits'
                     desc(2) = '0, TEXT,'+y.name+',  LABEL_LEFT=Axis y  - name :, WIDTH=20, TAG=yname'
                     desc(3) = '0, TEXT,'+y.units+', LABEL_LEFT=Axis y  - units:, WIDTH=20, TAG=yunits'
                     desc(4) = '0, TEXT,'+yp.name+', LABEL_LEFT=Axis yp - name :, WIDTH=20, TAG=ypname'
                     desc(5) = '0, TEXT,'+yp.units+',LABEL_LEFT=Axis yp - units:, WIDTH=20, TAG=ypunits'
                     desc(6) = '1, BASE,, ROW, CENTER'
                     desc(7) = '0, BUTTON, OK, QUIT, TAG=OK'
                     desc(8) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
                     form = CW_FORM(desc, /COLUMN, title='Change axes labels')
                     if form.cancel then GOTO, CONTINUE
                     x.name = form.xname   &   x.units = form.xunits
                     y.name = form.yname   &   y.units = form.yunits
                     yp.name = form.ypname   &   yp.units = form.ypunits
                     redo_last
                   END
     'sync_years': sync_years
     'autolevs':   BEGIN
                     autolevSW = 1B   &   viewpreproc
                   END
     'fixlevs':    autolevSW = 0B
     'smooth'    : BEGIN
                     If (last.plot2d eq 'depth-x') then Begin
                        zmolat.matrix = MEDIAN(zmolat.matrix,3)
                        zmolat.loc(2) = 'smoothed'
                      EndIf Else Begin
                        zl.matrix = MEDIAN(zl.matrix,3)
                        zl.loc(2) = 'smoothed'
                     EndElse
                     redo_last
                   END
     'showSW'    : form_show
     'colorSW'   : BEGIN
                     if colorSW then colorSW=0 else colorSW=1
                     If colorSW then Begin
                        loadct, 39  ; "Rainbow+white" color table
                        device, DECOMPOSED=0
                        ncols_avail = (!D.N_COLORS < 256 )
                     EndIf
                     redo_last
                   END
     'colorch'   : BEGIN
                     If colorSW eq 0 then Begin
                        colorSW = 1  &   device, DECOMPOSED=0
                        loadct, 39  ; "Rainbow+white" color table
                        viewpreproc
                     EndIf
                     xloadct
                   END
     'molatfixlev' : BEGIN
                        reset_titles = 1   &   wdelete1
                        month_lat_calc, /altfix
                        If z2.compOK then month_lat_calc, /altfix, /bg_arch
                        If z3.compOK then month_lat_calc, /altfix, /ref
                        last.plot2d = 'depth-x'   &   viewpreproc   &   last.action = action
                     END
     'molattotal'  : BEGIN
                        reset_titles = 1   &   wdelete1
                        month_lat_calc
                        If z2.compOK then month_lat_calc, /bg_arch
                        If z3.compOK then month_lat_calc, /ref
                        last.plot2d = 'depth-x'   &   viewpreproc   &   last.action = action
                     END
     'molevfixlat' : BEGIN
                        reset_titles = 1   &   wdelete1
                        month_lev_calc, /latfix
                        If z2.compOK then month_lev_calc, /latfix, /bg_arch
                        If z3.compOK then month_lev_calc, /latfix, /ref
                        last.plot2d = 'depth-y'   &   viewpreproc   &   last.action = action
                     END
     'molevtotal' :  BEGIN
                        reset_titles = 1   &   ip = -9
                        month_lev_calc
                        If z2.compOK then month_lev_calc, /bg_arch
                        If z3.compOK then month_lev_calc, /ref
                        last.plot2d = 'depth-y'   &   viewpreproc   &   last.action = action
                     END
     'usrfcts'     : user_functions
     'unzoom'    : BEGIN
                     depth.i0 = 0   &   depth.i1 = depth.dim-1
                     idp0 = depth.i0   &   idp1 = depth.i1     ; to remove ASAP
                     if arch.OK.molev then time_labels, depth
                     x.i0 = 0   &   x.i1 = x.dim-1
                     y.i0 = 0   &   y.i1 = y.dim-1
                    jp0 = y.i0   &   jp1 = y.i1   &   ip0 = x.i0   &   ip1 = x.i1  ; to remove ASAP
                    if (last.plot2d eq 'depth-x') then               $
                        if (zmolat.units eq 'molec/cm2') or           $
                               (zmolat.units eq 'Dobson Units') then  $
                           month_lat_calc
                     redo_last
                     msg = 'Ready.'
                   END
     'valsetter' : BEGIN
                     valsetter
                     if win_exist(1) then redo_last
                   END
     'vertslice' : BEGIN
                     reset_titles = 1   &   wdelete1
                     vertslicer   &   last.plot1d = 'f(y)'
                     viewpreproc
                   END
     'horslice'  : BEGIN
                     reset_titles = 1   &   wdelete1
                     If (last.plot2d eq 'depth-x') then Begin
                        month_val   &    last.plot1d = 'f(depth)'
                     EndIf Else If (last.plot2d eq 'depth-y') then Begin
                        month_val2   &    last.plot1d = 'f(depth)'
                     EndIf Else Begin
                        horslicer   &   last.plot1d = 'f(x)'
                     EndElse
;                     viewpreproc
                   END
     'vertglobslice' : BEGIN
                         reset_titles = 1
                         vertslicer, /lat_avg   &   last.plot1d = 'f(y)'
                         pointed = 0   &   viewpreproc   &   last.action = action
                       END
     'vertavgslice' : BEGIN
                         reset_titles = 1
                         vertslicer, /lat_avg, /time_avg   &   last.plot1d = 'f(y)'
                         pointed = 0   &   viewpreproc   &   last.action = action
                       END
     'mesotvals' : BEGIN
                     reset_titles = 1
                     mesotvals   &   last.plot1d = 'mesotvals'
                     pointed = 0   &   viewpreproc   &   last.action = action
                   END
     'lifetimes' : lifetimes
     'heat_budget': heat_budget
     'correl_n2o_var': BEGIN
                           reset_titles = 1
                           correl, 'n2o'   &   last.plot1d = 'correl_n2o_var'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_n2o_noy': BEGIN
                           reset_titles = 1
                           correl, 'n2o', /noy   &   last.plot1d = 'correl_n2o_noy'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_n2o_cly': BEGIN
                           reset_titles = 1
                           correl, 'n2o', /cly   &   last.plot1d = 'correl_n2o_cly'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_n2o_bry': BEGIN
                           reset_titles = 1
                           correl, 'n2o', /bry   &   last.plot1d = 'correl_n2o_bry'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_ch4_var': BEGIN
                           reset_titles = 1
                           correl, 'ch4'   &   last.plot1d = 'correl_ch4_var'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_ch4_noy': BEGIN
                           reset_titles = 1
                           correl, 'ch4', /noy   &   last.plot1d = 'correl_ch4_noy'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_ch4_cly': BEGIN
                           reset_titles = 1
                           correl, 'ch4', /cly   &   last.plot1d = 'correl_ch4_cly'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_ch4_bry': BEGIN
                           reset_titles = 1
                           correl, 'ch4', /bry   &   last.plot1d = 'correl_ch4_bry'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_ch4_bry_src': BEGIN
                           reset_titles = 1
                           correl, 'ch4', /bry_src   &   last.plot1d = 'correl_ch4_bry_src'
                           pointed = 0   &   viewpreproc   &   last.action = action
                        END
     'correl_h2o': BEGIN
                     reset_titles = 1
                     correl_h2o   &   last.plot1d = 'correl_h2o'
                     pointed = 0   &   viewpreproc   &   last.action = action
                   END
     'anim_preview': make_mpeg
     'anim_mpeg' : make_mpeg, /savesw
     'help_intro': xdisplayfile, 'README.txt', TITLE='view2d Introduction and Installation'
     'help_use'  : xdisplayfile, 'USAGE.txt', TITLE='view2d usage manual'
     'help_bugs' : xdisplayfile, 'BUGS.txt', TITLE='view2d bugs and workarounds'
     'about'     : xdisplayfile, 'VERSION.txt', TITLE='about view2d...'
     'idl_shell' : BEGIN
                     print,'!!!!!----------------------- TYPE "return" TO RESUME EXECUTION'
                     stop
                   END
     'wvarlist': BEGIN
                     zl.vid = event.index   &   reset_titles = 1   &   valSW = 0
                     archread, arch.list.vars(zl.vid), zl.did, zl
                     find_ids, /read   &   find_ids, /ref, /read
                     wdelete1   &   last.plot2d = 'x-y'   &   viewpreproc
                     last.action = action
                 END
     'wdepthlist': BEGIN
                     zl.did = event.index   &   reset_titles = 1
                     archread, arch.list.vars(zl.vid), zl.did, zl
                     If show.ygeo then ygeo.matrix(0:x.dim-1,0:y.dim-1) = calc_ygeo( )
                     find_ids, /read   &   find_ids, /ref, /read
                     wdelete1   &   last.plot2d = 'x-y'   &   viewpreproc
                     last.action = action
                   END
     'LogSwitchOn': BEGIN
                       logsw = 1
                       if win_exist(1) then redo_last
                    END
     'LogSwitchOff': BEGIN
                       logsw = 0
                       if win_exist(1) then redo_last
                     END
     'vmrSwitch' : vmrswitch
     'densSwitch': densswitch
     'ResetPoint': BEGIN
                     pointed = 0   &   wdelete1   &   viewpreproc   &   last.action = action
                   END
     'zgrid':      BEGIN
                     show.ygeo = event.index
                     If show.ygeo then Begin
                        ygeo.matrix(0:x.dim-1,0:y.dim-1) = calc_ygeo(ok=ok)
                        If not ok then Begin
                           error = DIALOG_MESSAGE('Calculation of geometric altitude failed', /ERROR)
                           arch.OK.ygeo = 0   &   show.ygeo = 0
                        EndIf Else Begin
                           If arch_to_compOK then if arch2.OK.ygeo then Begin
                              ygeo2.matrix(0:x2.dim-1,0:y2.dim-1) = calc_ygeo( ok=ok, /bg )
                              If not ok then Begin
                                 error = DIALOG_MESSAGE('Calc geo alt for BG file failed', /ERROR)
                                 arch2.OK.ygeo = 0   &   z2.compOK = 0
                              EndIf
                           EndIf
                           If arch_refOK then if arch3.OK.ygeo then Begin
                              ygeo3.matrix(0:x3.dim-1,0:y3.dim-1) = calc_ygeo( ok=ok, /ref )
                              If not ok then Begin
                                 error = DIALOG_MESSAGE('Calc geo alt for Ref file failed', /ERROR)
                                 arch3.OK.ygeo = 0   &   z3.compOK = 0
                              EndIf
                           EndIf
                        EndElse
                     EndIf
                     wdelete1   &   viewpreproc   &   last.action = action
                   END
     'SwitchFB':   switchfiles, ref=0
     'SwitchFR':   switchfiles, /ref
     'Bfollow' :   BEGIN
                     z2.fix = event.index   &   find_ids, /read
                     if z2.fix eq 0 then redo_last, /only1d
                   END
     'Rfollow' :   BEGIN
                     z3.fix = event.index   &   find_ids, /ref, /read
                     if z3.fix eq 0 then redo_last, /only1d
                   END
     'reldiff'   : BEGIN
                     wdelete1   &   reset_titles = 1   &   valSW = 0
                     If (last.plot2d eq 'x-y') then Begin
                        zprov = zl
                        reldiff, zprov, z2, zl   &   redo_last
                      EndIf Else If (last.plot2d eq 'depth-x') then Begin
                        zprov = zmolat
                        reldiff, zprov, zmolat2, zmolat   &    redo_last
                     EndIf
                     z2.compOK = 0
                   END
     'absdiff'   : BEGIN
                     wdelete1   &   reset_titles = 1   &   valSW = 0
                     If (last.plot2d eq 'x-y') then Begin
                        zprov = zl
                        absdiff, zprov, z2, zl   &   redo_last
                      EndIf Else If (last.plot2d eq 'depth-x') then Begin
                        zprov = zmolat
                        absdiff, zprov, zmolat2, zmolat   &    redo_last
                     EndIf
                     z2.compOK = 0
                   END
     'drawEvent' : contour_roi, event, action

  ELSE: MESSAGE, 'Event User Value "'+action+'" Not Found', /TRACEBACK
ENDCASE

If (last.action ne 'quit') and (last.action ne 'archopen') and          $
   (last.action ne 'ncbrowse') and (last.action ne 'drawEvent') then Begin
   update_widgets
   WIDGET_CONTROL, draw_id, GET_VALUE=draw_win
   wset, draw_win
EndIf

;help, last, /struc
CONTINUE :

END ;============= end of selecter event handling routine task ================

;------------------------------------------------------------------------------
;       procedure selecter
; This routine creates the widget and registers it with the XManager.
;------------------------------------------------------------------------------
PRO selecter, GROUP = GROUP, _EXTRA=e

common hardware         ; in ; main_xoffset, main_yoffset ; inout : draw_xsize, draw_ysize
common arch_info
common axes
common select_id
common plots_info
common dataranges
common last_fields
common last_com
common label_widgets
common widgets_info

;=========================================================================
;====================== Create the full-screen widget ====================
;=========================================================================

mainbase_id = WIDGET_BASE(TITLE = "view2d : an IDL browser for 2D+time netCDF files",   $
                            /COLUMN, MBAR=bar_base)
bigbase_id = WIDGET_BASE( mainbase_id, /ROW )
toolbase_id = WIDGET_BASE( bigbase_id, /COLUMN )

;============  Create the file widgets (F, B & R labels & buttons)  ==================

filebase_id = WIDGET_BASE( toolbase_id, /COLUMN, /FRAME)

base_id = WIDGET_BASE( filebase_id, /ROW )
junk = WIDGET_LABEL( base_id, Value='Foreground: ', /ALIGN_LEFT )
junk = WIDGET_BUTTON( base_id, Value='Open', Uvalue='archopen')
Fgfname_id = WIDGET_LABEL( filebase_id, /ALIGN_RIGHT )

base_id = WIDGET_BASE( filebase_id, /ROW )
junk = WIDGET_LABEL( base_id, Value='Background: ', /ALIGN_LEFT )
Bfollow_id = WIDGET_DROPLIST( base_id, Value=['follows F','fix var','fix date'], $
                              Uvalue='Bfollow', /ALIGN_LEFT )
Bgfname_id = WIDGET_LABEL( filebase_id, /ALIGN_RIGHT )

cmpbase_id = WIDGET_BASE( filebase_id, /ROW )
junk = WIDGET_BUTTON( cmpbase_id, Value='Switch', Uvalue='SwitchFB' )
junk = WIDGET_BUTTON( cmpbase_id, Value='(F-B)/B', Uvalue = 'reldiff')
junk = WIDGET_BUTTON( cmpbase_id, Value='F-B', Uvalue = 'absdiff')

base_id = WIDGET_BASE( filebase_id, /ROW )
junk = WIDGET_LABEL( base_id, Value='Reference: ', /ALIGN_LEFT )
Rfollow_id = WIDGET_DROPLIST( base_id, Value=['follows F','fix var','fix date'], $
                              Uvalue='Rfollow', /ALIGN_LEFT )
Refname_id = WIDGET_LABEL( filebase_id, /ALIGN_RIGHT )

;=======     Create the other tool widgets (left column of lists)   =================

listbase_id = WIDGET_BASE( toolbase_id, /ROW )

varlist_id = WIDGET_LIST( listbase_id, VALUE=arch.list.vars(0:arch.list.nbvars-1),     $
                     UVALUE='wvarlist', YSIZE=(arch.list.nbvars<12) )

depthlistwidid = WIDGET_LIST( listbase_id, VALUE=arch.list.depth(0:depth.dim-1),       $
                     UVALUE='wdepthlist', YSIZE=(depth.dim<12) ) ;, XSIZE=11 )
listsgeo = WIDGET_INFO( listbase_id, /GEOMETRY )

togglebase = WIDGET_BASE( toolbase_id, /ROW )
loglinframe = WIDGET_BASE( togglebase, /COLUMN, /FRAME )
junk = WIDGET_LABEL( loglinframe, value='1D-axis', /ALIGN_CENTER )
loglinbase = WIDGET_BASE( loglinframe, /COLUMN, /EXCLUSIVE)
logbutton = WIDGET_BUTTON( loglinbase, Value='Log', Uvalue='LogSwitchOn', /NO_RELEASE)
linbutton = WIDGET_BUTTON( loglinbase, Value='Lin', Uvalue='LogSwitchOff', /NO_RELEASE)
If arch.OK.totdens then Begin
   vmrdensframe = WIDGET_BASE( togglebase, /COLUMN, /FRAME )
   junk = WIDGET_LABEL( vmrdensframe, value='Chem', /ALIGN_CENTER )
   vmrdensbase = WIDGET_BASE( vmrdensframe, /COLUMN, /EXCLUSIVE)
   vmrbutton = WIDGET_BUTTON( vmrdensbase, Value='vmr', Uvalue='vmrSwitch', /NO_RELEASE)
   densbutton = WIDGET_BUTTON( vmrdensbase, Value='dens', Uvalue='densSwitch', /NO_RELEASE)
EndIf
autofixframe = WIDGET_BASE( togglebase, /COLUMN, /FRAME )
junk = WIDGET_LABEL( autofixframe, value='2D-levs', /ALIGN_CENTER )
autofixbase = WIDGET_BASE( autofixframe, /COLUMN, /EXCLUSIVE)
autobutton = WIDGET_BUTTON( autofixbase, Value='auto', Uvalue='autolevs', /NO_RELEASE)
fixbutton = WIDGET_BUTTON( autofixbase, Value='fix', Uvalue='fixlevs', /NO_RELEASE)
; buttonbase = WIDGET_BASE( togglebase, /COLUMN )
; autolev = WIDGET_BUTTON( buttonbase, Value='AutoLev', Uvalue='autoscaler')
; redraw = WIDGET_BUTTON( buttonbase, Value='Re-draw', Uvalue='redraw')
togglegeo = WIDGET_INFO( togglebase, /GEOMETRY )

pointbase_id = WIDGET_BASE(toolbase_id, /FRAME, /COLUMN)
pointtitle = WIDGET_BASE(pointbase_id, /ROW)
junk = WIDGET_LABEL(pointtitle, Value='Pointed Coordinates - ')
resetbttn = WIDGET_BUTTON( pointtitle, Value='Reset', Uvalue = 'ResetPoint')

labels_xs = (listsgeo.xsize < togglegeo.xsize) - 4*listsgeo.xoffset
WIDGET_CONTROL, Fgfname_id, SCR_XSIZE=labels_xs
WIDGET_CONTROL, Bgfname_id, SCR_XSIZE=labels_xs
WIDGET_CONTROL, Refname_id, SCR_XSIZE=labels_xs

pointedX_id = WIDGET_LABEL(pointbase_id, /ALIGN_LEFT, SCR_XSIZE=labels_xs)
zgrid_base = WIDGET_BASE(pointbase_id, /ROW)
zgrid_id = WIDGET_DROPLIST(zgrid_base, VALUE=['log-p','geo'], UVALUE='zgrid')
zgridinfo = WIDGET_INFO( zgrid_id, /GEOMETRY )
labels_xs = labels_xs - zgridinfo.xsize - 4*zgridinfo.xoffset
pointedY_id = WIDGET_LABEL(zgrid_base, /ALIGN_LEFT, SCR_XSIZE=labels_xs)

;============ Create the draw widget, for contour plots  ==================
;============         Create the message widget          ==================
;============ !! SCREEN SIZE AND COLOR MAP TAKEN HERE !! ==================

toolgeo = WIDGET_INFO(toolbase_id, /GEOMETRY)
If (draw_xsize eq 0) then Begin
   ipads = 14   &   if !D.NAME eq 'WIN' then ipads = 6
   draw_xsize = screen_size(0) - (toolgeo.xsize + toolgeo.xoffset + ipads*toolgeo.xpad)
EndIf   
msg_xsize = draw_xsize + toolgeo.xsize
msg_id = WIDGET_LABEL( mainbase_id, /FRAME, SCR_XSIZE=msg_xsize, /ALIGN_LEFT )
msggeo = WIDGET_INFO( msg_id, /GEOMETRY )
If (draw_ysize eq 0) then Begin
   ybot = 200   &   if !D.NAME eq 'WIN' then ybot = 57
   draw_ysize = screen_size(1) - msggeo.scr_ysize - ybot
EndIf

draw_id = WIDGET_DRAW( bigbase_id, Uvalue='drawEvent', /BUTTON_EVENTS,   $
                       COLORS=take_colors, XSIZE=draw_xsize, YSIZE=draw_ysize)

DEVICE, DECOMPOSED=0     ; turn off decomposed color representation for 24-bit devices

;======================== Create the upper menu bar ======================

;----------- File menu -----------------

file_menu = WIDGET_BUTTON(bar_base, Value='File', /Menu)
junk = WIDGET_BUTTON(file_menu, Value='Open new Foreground file...', Uvalue='archopen')
desc_fgfile_id = WIDGET_BUTTON( file_menu, Value='Describe Foreground file...', Uvalue='descfgfile' )
junk = WIDGET_BUTTON(file_menu, Value='Open new Background file...', Uvalue='open_same')
closebgfile_id = WIDGET_BUTTON(file_menu, Value='Close Background file', Uvalue='closebgfile')
junk = WIDGET_BUTTON(file_menu, Value='Open new Reference file...', Uvalue='open_ref')
switchFR_id = WIDGET_BUTTON( file_menu, Value='Switch Ref & Fore files', Uvalue='SwitchFR' )
closerefile_id = WIDGET_BUTTON(file_menu, Value='Close Reference file', Uvalue='closerefile')
junk = WIDGET_BUTTON(file_menu, Value='ncbrowse...', Uvalue='ncbrowse')
export_menu = WIDGET_BUTTON(file_menu, Value='Export', /Menu)
junk = WIDGET_BUTTON(export_menu, Value='last plot to ASCII...', Uvalue='asciiwrite')
junk = WIDGET_BUTTON(export_menu, Value='all chem vars (this date) to ASCII...', Uvalue='asciiwrite_allchem')
junk = WIDGET_BUTTON(export_menu, Value='last plot to PostScript...', Uvalue='plot_ps')
junk = WIDGET_BUTTON(export_menu, Value='last plot to Encapsulated PostScript...',       $
                     Uvalue='plot_eps')
junk = WIDGET_BUTTON(export_menu, Value='last plot to GGM...',       $
                     Uvalue='plot_cgm')
junk = WIDGET_BUTTON(file_menu, Value='Print last plot...', Uvalue='print')
if (!D.name ne 'X') then  $
   junk = WIDGET_BUTTON(file_menu, Value='Printer Setup...', Uvalue='printsetup')
junk = WIDGET_BUTTON(file_menu, Value='Pause execution', Uvalue='idl_shell')
junk = WIDGET_BUTTON(file_menu, Value='Exit', Uvalue='quit')

;----------- display menu --------------

display_menu = WIDGET_BUTTON(bar_base, Value='Display', /Menu)
junk = WIDGET_BUTTON(display_menu, Value='UnZoom', UVALUE='unzoom')
junk = WIDGET_BUTTON(display_menu, Value='Edit texts...', Uvalue='form_titles')
junk = WIDGET_BUTTON(display_menu, Value='Edit axes labels...', Uvalue='chgaxes')
junk = WIDGET_BUTTON(display_menu, Value='Synchronize the years...', Uvalue='sync_years')
junk = WIDGET_BUTTON(display_menu, Value='Smooth field', UVALUE='smooth')
junk = WIDGET_BUTTON(display_menu, Value='Show/NotShow...', UVALUE='showSW')
junk = WIDGET_BUTTON(display_menu, Value='Color/NoColor', UVALUE='colorSW')
junk = WIDGET_BUTTON(display_menu, Value='Change colors...', UVALUE='colorch')

;----------- 2D-plots menu ----------------

plot2d = WIDGET_BUTTON(bar_base, Value='2D-plots', /Menu)
junk = WIDGET_BUTTON(plot2d, Value='Manual levels...', Uvalue='manscaler')
molatfixlev_id = WIDGET_BUTTON(plot2d, Value=                     $
                      'Time-Latitude at Pointed '+y.name,                $
                      Uvalue='molatfixlev')
If arch.OK.molat then WIDGET_CONTROL, molatfixlev_id, /SENSITIVE Else  $
     WIDGET_CONTROL, molatfixlev_id, SENSITIVE=0

molattotal_id = -999
if arch.OK.totdens and (y.units eq 'km') then                          $
   molattotal_id = WIDGET_BUTTON(plot2d, Value=                        $
                      'Time-Latitude (column)', Uvalue='molattotal')
molevfixlat_id = WIDGET_BUTTON(plot2d,                                 $
                               Value='Time-Level at Pointed '+x.name,    $
                               Uvalue='molevfixlat')
if arch.OK.molev then WIDGET_CONTROL, molevfixlat_id, /SENSITIVE Else  $
     WIDGET_CONTROL, molevfixlat_id, SENSITIVE=0
molevtotal_id = WIDGET_BUTTON(plot2d,                                 $
                               Value='Time-Level (global avg)',    $
                               Uvalue='molevtotal')
junk = WIDGET_BUTTON(plot2d, Value='User functions...', Uvalue='usrfcts')

;----------- 1D-plots menu ----------------

plot1d_id = WIDGET_BUTTON(bar_base, Value='1D-plots', /Menu)
junk = WIDGET_BUTTON(plot1d_id, Value='Values range...', Uvalue='valsetter')
plot1dvert_id = WIDGET_BUTTON(plot1d_id, Value='Vertical', UVALUE='vertslice')
plot1dhor_id = WIDGET_BUTTON(plot1d_id, Value='Horizontal', UVALUE='horslice')
plot1dvertglob_id = WIDGET_BUTTON(plot1d_id, Value='Vertical, latitude average',   $
                                  UVALUE='vertglobslice')
plot1dvertavg_id = WIDGET_BUTTON(plot1d_id, Value='Vertical, time & lat average',   $
                                  UVALUE='vertavgslice')
plot1dmesoval_id = WIDGET_BUTTON(plot1d_id, Value='Mesopause temperatures',   $
                                  UVALUE='mesotvals')
plot1dtimes_id = WIDGET_BUTTON(plot1d_id, Value='Chemical times',   $
                                  UVALUE='lifetimes')
if STRPOS(arch.flnm, 'heat') ge 0 then                                        $
   junk = WIDGET_BUTTON(plot1d_id, Value='Heat budget', UVALUE='heat_budget')

;----------- Scatter menu ----------------

scatter_id = WIDGET_BUTTON(bar_base, Value='Scatter', /Menu)
n2o_menu = WIDGET_BUTTON(scatter_id, Value='Correlate current time N2O with ', $
                         /Menu)
junk = WIDGET_BUTTON(n2o_menu, Value='current var', UVALUE='correl_n2o_var')
junk = WIDGET_BUTTON(n2o_menu, Value='NOy sum', UVALUE='correl_n2o_noy')
junk = WIDGET_BUTTON(n2o_menu, Value='Cly sum', UVALUE='correl_n2o_cly')
junk = WIDGET_BUTTON(n2o_menu, Value='Bry sum', UVALUE='correl_n2o_bry')
ch4_menu = WIDGET_BUTTON(scatter_id, Value='Correlate current time ch4 with ', $
                         /Menu)
junk = WIDGET_BUTTON(ch4_menu, Value='current var', UVALUE='correl_ch4_var')
junk = WIDGET_BUTTON(ch4_menu, Value='NOy sum', UVALUE='correl_ch4_noy')
junk = WIDGET_BUTTON(ch4_menu, Value='Cly sum', UVALUE='correl_ch4_cly')
junk = WIDGET_BUTTON(ch4_menu, Value='Bry sum', UVALUE='correl_ch4_bry')
junk = WIDGET_BUTTON(ch4_menu, Value='Bry_src sum', UVALUE='correl_ch4_bry_src')
junk = WIDGET_BUTTON(scatter_id  $
 , Value='Correlate H2O (all times,selected alt) with current var'  $
 , UVALUE='correl_h2o')

;----------- Animate ----------------

If arch.OK.molat then Begin
   animate_id = WIDGET_BUTTON(bar_base, Value='Animate', /Menu)
   junk = WIDGET_BUTTON(animate_id, Value='Preview', Uvalue='anim_preview')
   junk = WIDGET_BUTTON(animate_id, Value='Save as MPEG file...',         $
                        Uvalue='anim_mpeg')
EndIf

;----------- Help menu ----------------

help_id = WIDGET_BUTTON(bar_base, Value='Help', /HELP, /Menu)
junk = WIDGET_BUTTON(help_id, Value='Intro/get started...', UVALUE='help_intro')
junk = WIDGET_BUTTON(help_id, Value='Using view2d...', UVALUE='help_use')
junk = WIDGET_BUTTON(help_id, Value='Bugs/workarounds...', UVALUE='help_bugs')
junk = WIDGET_BUTTON(help_id, Value='About...', UVALUE='about')


;================ Make sure no parameters are out of range =================

if (zl.did gt (depth.dim-1)) then zl.did = 0
if (zl.vid gt (arch.list.nbvars-1)) then zl.vid = 0

;=================== Realize the widgets defined above =====================

WIDGET_CONTROL, depthlistwidid, SET_LIST_SELECT=zl.did
WIDGET_CONTROL, varlist_id, SET_LIST_SELECT=zl.vid
If logsw then WIDGET_CONTROL, logbutton, /SET_BUTTON            $
 Else WIDGET_CONTROL, linbutton, /SET_BUTTON
If arch.OK.totdens then Begin
   If densSW then WIDGET_CONTROL, densbutton, /SET_BUTTON       $
    Else WIDGET_CONTROL, vmrbutton, /SET_BUTTON
EndIf
If arch.OK.ygeo then WIDGET_CONTROL, zgrid_id, SET_DROPLIST_SELECT=show.ygeo
If autolevSW then WIDGET_CONTROL, autobutton, /SET_BUTTON               $
 Else WIDGET_CONTROL, fixbutton, /SET_BUTTON

If (main_xoffset eq -9999) or (main_yoffset eq -9999) then      $
  WIDGET_CONTROL, mainbase_id, /REALIZE                         $
 Else                                                           $
  WIDGET_CONTROL, mainbase_id, XOFFSET=main_xoffset, YOFFSET=main_yoffset, /REALIZE

;====================== draw first contour =================================

last.action = 'create_widgets'
last.plot2d = 'x-y'   &   last.plot1d = ''
archread, arch.list.vars(zl.vid), zl.did, zl
viewpreproc
update_widgets

;===================== Call the event manager ==============================

XManager, CATCH=0      ; 0:prgm execution will stop when error is encountered.
XManager, "selecter", mainbase_id, $            ;register the widgets
                EVENT_HANDLER = "selecter_ev", $        ;with the XManager
                GROUP_LEADER = GROUP, _EXTRA=e

return

END ;==================== end of selecter main routine =======================
