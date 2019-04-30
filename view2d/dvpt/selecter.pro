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
common select_id2
common plots_info
common last_fields
common last_fields2
common moving_box
common dataranges
common axes
common axes2
common last_com
common movies
common label_widgets
common widgets_info

WIDGET_CONTROL, event.id, GET_UVALUE = action           ;find the user value
                                                        ;of the widget where
                                                        ;the event occured

; help, event, /struc

CASE action OF

;*** here is where you would add the actions for your events.  Each widget
;*** you add should have a unique string for its user value.  Here you add
;*** a case for each of your widgets that return events and take the
;*** appropriate action.

     'quit':       BEGIN
;                     wdelete, 1
                     WIDGET_CONTROL, event.top, /DESTROY
                     last.action = action
                   END
     'archopen' :  BEGIN
                     WIDGET_CONTROL, event.top, /DESTROY   ;&   wdelete, 1
                     ncdf_close, arch.cdfid
                     reset_titles = 1   &   idp0 = 9999   &   latpn = 9999.
                     last.action = action
                   END
     'open_same' : BEGIN
;                     wdelete, 1
                     archopen, /arch_to_comp
                     If arch_to_compOK then Begin
                        de_ne_d = 1
                        If (depth.dim eq depth2.dim) then                                       $
                           indexd = where(arch2.list.depth ne arch.list.depth, d2_ne_d)
                        v2_ne_v = 1
                        If (arch.list.nbvars eq arch2.list.nbvars) then                         $
                           indexv = where(arch2.list.vars ne arch.list.vars, v2_ne_v)
                        if d2_ne_d then find_depth_id2 else depth_id2 = depth_id
                        if v2_ne_v then find_var_id2 else var_id2 = var_id
                        If (depth_id2 eq -1) or (var_id2 eq -1) then field_to_compOK = 0        $
                         Else Begin
                           archread, arch2.list.vars(var_id2), depth_id2, z2, /bg_arch
                           field_to_compOK = 1
                           If last.plot2d eq 'depth-x' and arch2.ok.molat then                  $
                              month_lat_calc, z2, zmolat2, /altfix, /bg_arch
                           If last.plot2d eq 'depth-y' and arch2.ok.molev then                  $
                              month_lev_calc, z2, zmolev2, /latfix, /bg_arch
                        EndElse
                     EndIf
                     last.action = action
                   END
     'closebgfile':BEGIN
                     arch_to_compOK = 0   &   field_to_compOK = 0   &   redo_last
                   END
     'ncbrowse'  : BEGIN
                     WIDGET_CONTROL, event.top, /DESTROY   ;&   WDELETE, 1
                     NCDF_CLOSE, arch.cdfid
                     last.action = action
                   END
     'asciiwrite': write_ascii_xface
     'plot_to_file': BEGIN
                     If (N_ELEMENTS(zl) eq 0) then Begin
                        error = WIDGET_MESSAGE('No plot in memory !', /ERROR)
                        GOTO, CONTINUE
                     EndIf
                     plot_to_file
                   END
     'plot_gif'    : plot_gif
     'print':      BEGIN
                     If (N_ELEMENTS(zl) eq 0) then Begin
                        error = WIDGET_MESSAGE('No plot in memory !', /ERROR)
                        GOTO, CONTINUE
                     EndIf
                     junk = DIALOG_PRINTJOB()   ; on hercules.oma.be, just OK/Cancel stuff
                     if (junk eq 0) then GOTO, CONTINUE
                     device, window_state = win_exist
                     set_plot, 'printer'   &   !p.font = 0
                     redo_last, only1d=win_exist(1)
                     device, /close   &   set_plot, screen   &   !p.font = -1
                   END
     'printsetup': junk = DIALOG_PRINTERSETUP()
     'manscaler' : BEGIN
                     manscaler   &   autolevSW = 0   &   viewpreproc
                   END
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
     'chgyears':   BEGIN
                     desc = strarr(5)
                     desc(0) = '0, LABEL, Number of years to add/substract to the dates of the, LEFT'
                     desc(1) = '0,INTEGER, 0,LABEL_LEFT=Foreground file (integer):,WIDTH=5,TAG=nyears'
                     desc(2) = '1, BASE,, ROW, CENTER'
                     desc(3) = '0, BUTTON, OK, QUIT, TAG=OK'
                     desc(4) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
                     form = CW_FORM(desc, /COLUMN, title='Change years')
                     if form.cancel then GOTO, CONTINUE
                     depth.coord(*) = depth.coord(*) + 365.*form.nyears
                     if arch.OK.molev then time_labels, depth
                     make_depthlist, depth, depth_is_date, depthlist   &   arch.list.depth = depthlist
                                                                ; update depthlist widget
                         WIDGET_CONTROL, depthlistwidid, SET_VALUE=arch.list.depth, SET_LIST_SELECT=depth_id
                     days0date, depth.coord(depth_id), date
                     zl.date = date
                     redo_last
                   END
     'autolevs':   BEGIN
                     autolevSW = 1   &   viewpreproc
                   END
     'fixlevs': autolevSW = 0
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
                     colorSW = NOT colorSW   &   device, DECOMPOSED=0
                     if colorSW then loadct, 39  ; "Rainbow+white" color table
                     redo_last
                   END
     'colorch'   : BEGIN
                     If (NOT colorSW) then Begin
                        colorSW = 1  &   device, DECOMPOSED=0
                        loadct, 39  ; "Rainbow+white" color table
                        viewpreproc
                     EndIf
                     xloadct
                   END
     'molatfixlev' : BEGIN
                        reset_titles = 1   ;&   wdelete, 1
                        month_lat_calc, zl, zmolat, /altfix
                        If field_to_compOK then month_lat_calc, z2, zmolat2, /altfix, /bg_arch
                        last.plot2d = 'depth-x'   &   viewpreproc   &   last.action = action
                     END
     'molevfixlat' : BEGIN
                        reset_titles = 1   ;&   wdelete, 1
                        month_lev_calc, zl, zmolev, /latfix
                        If field_to_compOK then month_lev_calc, z2, zmolev2, /latfix, /bg_arch
                        last.plot2d = 'depth-y'   &   viewpreproc   &   last.action = action
                     END
     'molattotal'  : BEGIN
                        reset_titles = 1   ;&   wdelete, 1
                        month_lat_calc, zl, zmolat
                        If field_to_compOK then month_lat_calc, z2, zmolat2, /bg_arch
                        last.plot2d = 'depth-x'   &   viewpreproc   &   last.action = action
                     END
     'dvplotzoomed': BEGIN
                       reset_titles = 1   &   dvzoom = 1   ;&   wdelete,1
                       archread, arch.list.vars(var_id), depth_id, zl
                       diuvarplot   &   last.plot1d = 'diuvarplot'
                     END
     'dvplotunzoom': BEGIN
                       reset_titles = 1   &   dvzoom = 0   ;&   wdelete,1
                       archread, arch.list.vars(var_id), depth_id, zl
                       diuvarplot   &   last.plot1d = 'diuvarplot'
                     END
     'unzoom'    : BEGIN
                     idp0 = 0   &   idp1 = depth.dim-1
                     if arch.OK.molev then time_labels, depth
                     ip0 = 0   &   ip1 = x.dim-1
                     jp0 = 0   &   jp1 = y.dim-1
                     if (last.plot2d eq 'depth-x') then               $
                        if (zmolat.units eq 'molec/cm2') or           $
                               (zmolat.units eq 'Dobson Units') then  $
                           month_lat_calc, zl, zmolat
                     redo_last
                     msg = 'Ready.'
                   END
     'vertslice' : BEGIN
                     reset_titles = 1   ;&   wdelete,1
                     vertslicer, zl   &   last.plot1d = 'f(y)'
                     viewpreproc
                   END
     'horslice'  : BEGIN
                     reset_titles = 1   ;&   wdelete,1
                     If (last.plot2d eq 'depth-x') then Begin
                        month_val   &    last.plot1d = 'f(depth)'
                     EndIf Else If (last.plot2d eq 'depth-y') then Begin
                        month_val2   &    last.plot1d = 'f(depth)'
                     EndIf Else Begin
                        horslicer   &   last.plot1d = 'f(x)'
                     EndElse
                     viewpreproc
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
                   var_id = event.index   &   reset_titles = 1
                   archread, arch.list.vars(var_id), depth_id, zl
                   If arch_to_compOK then Begin
                      if v2_ne_v then find_var_id2 else var_id2 = var_id
                      if d2_ne_d then find_depth_id2 else depth_id2 = depth_id
                      If (depth_id2 eq -1) or (var_id2 eq -1) then field_to_compOK = 0   $
                       Else Begin
                         archread, arch2.list.vars(var_id2), depth_id2, z2, /bg_arch
                         field_to_compOK = 1
                      EndElse
                   EndIf
;                   wdelete, 1   &   last.plot2d = 'x-y'   &   viewpreproc
                   last.plot2d = 'x-y'   &   viewpreproc
                   last.action = action
                 END
     'wdepthlist': BEGIN
                     depth_id = event.index   &   reset_titles = 1
                     archread, arch.list.vars(var_id), depth_id, zl
                     If arch_to_compOK then Begin
                        if d2_ne_d then find_depth_id2 else depth_id2 = depth_id
                        if v2_ne_v then find_var_id2 else var_id2 = var_id
                        If (depth_id2 eq -1) or (var_id2 eq -1) then field_to_compOK = 0   $
                         Else Begin
                           archread, arch2.list.vars(var_id2), depth_id2, z2, /bg_arch
                           field_to_compOK = 1
                        EndElse
                     EndIf
;                     wdelete, 1   &   last.plot2d = 'x-y'   &   viewpreproc
                     last.plot2d = 'x-y'   &   viewpreproc
                     last.action = action
                   END
     'LogSwitchOn': BEGIN
                       logsw = 1
                       device, window_state = win_exist
                       if win_exist(1) then redo_last
                    END
     'LogSwitchOff': BEGIN
                       logsw = 0
                       device, window_state = win_exist
                       if win_exist(1) then redo_last
                     END
     'vmrSwitch' : BEGIN
                     densSW = 0   &   reset_titles = 1
                     If (last.plot2d eq 'x-y') then Begin
                         If (zl.units eq 'molec/cm3') then Begin
                            zl.matrix = zl.matrix / arch.totdens(*,*,depth_id)
                            zl.units = 'vmr'
                            If field_to_compOK then Begin
                              z2.matrix = z2.matrix / arch2.totdens(*,*,depth_id2)
                              z2.units = 'vmr'
                            EndIf
                         EndIf
                         autoscaler   &   redo_last
                      EndIf Else If (last.plot2d eq 'depth-x')  $
                                      and (zmolat.units eq 'molec/cm3') then Begin
                           For l = 0, x.dim-1 Do zmolat.matrix(*,l) = $
                                            zmolat.matrix(*,l) / arch.totdens(l,jp,*)
                           zmolat.units = 'vmr'   &   autoscaler   &   redo_last
                     EndIf
                   END
     'densSwitch': BEGIN
                      densSW = 1   &   reset_titles = 1
                      If (last.plot2d eq 'x-y') then Begin
                         If (zl.units eq 'vmr') then Begin
                            zl.matrix = zl.matrix * arch.totdens(*,*,depth_id)
                            zl.units = 'molec/cm3'
                            If field_to_compOK then Begin
                               z2.matrix = z2.matrix * arch2.totdens(*,*,depth_id2)
                               z2.units = 'molec/cm3'
                            EndIf
                         EndIf
                         autoscaler   &   redo_last
                       EndIf Else If (last.plot2d eq 'depth-x') then Begin
                         If (zmolat.units eq 'vmr') then Begin
                             For l = 0, x.dim-1 Do zmolat.matrix(*,l) = $
                                            zmolat.matrix(*,l) * arch.totdens(l,jp,*)
                             zmolat.units = 'molec/cm3'   &   autoscaler   &   redo_last
                         EndIf
                      EndIf
                   END
     'ResetPoint': BEGIN
;                     pointed = 0   &   wdelete, 1   &   viewpreproc
                     pointed = 0    &   viewpreproc
                   END
     'SwitchFiles':BEGIN
                     find_depth_id2   &   find_var_id2   &   reset_titles = 1
                                                                ; now switch all files parameters
                     depth_id0 = depth_id   &   depth_id = depth_id2   &   depth_id2 = depth_id0
                     var_id0 = var_id   &   var_id = var_id2   &   var_id2 = var_id0
                     arch0 = arch   &   arch = arch2   &   arch2 = arch0
                     x0 = x   &   x = x2   &   x2 = x0
                     y0 = y   &   y = y2   &   y2 = y0
                     depth0 = depth   &   depth = depth2   &   depth2 = depth0
                     idp0 = 0   &   idp1 = depth.dim-1
                     idp2_0 = 0   &   idp2_1 = depth2.dim-1
                     if arch.OK.molat then time_labels, depth
                     if arch.OK.date then polardn, depth, latpn
                                                                ; update depthlist widget
                     WIDGET_CONTROL, depthlistwidid, SET_VALUE=arch.list.depth, SET_LIST_SELECT=depth_id
                                                                ; update varlist widget
                     WIDGET_CONTROL, varlist_id, SET_VALUE=arch.list.vars, SET_LIST_SELECT=var_id
                                                                ; now re-read fields and re-draw all
                     archread, arch.list.vars(var_id), depth_id, zl
                     if d2_ne_d then find_depth_id2 else depth_id2 = depth_id
                     if v2_ne_v then find_var_id2 else var_id2 = var_id
                     If (depth_id2 eq -1) or (var_id2 eq -1) then field_to_compOK = 0   $
                      Else Begin
                        archread, arch2.list.vars(var_id2), depth_id2, z2, /bg_arch
                        field_to_compOK = 1
                     EndElse
;                     last.plot2d = 'x-y'   &   wdelete,1   &   viewpreproc
                     last.plot2d = 'x-y'    &   viewpreproc
                   END
     'reldiff'   : BEGIN
;                     wdelete, 1   &   reset_titles = 1
                     reset_titles = 1
                     If (last.plot2d eq 'x-y') then Begin
                        zstack1 = zl
                        reldiff, zstack1, z2, zl   &   redo_last
                      EndIf Else If (last.plot2d eq 'depth-x') then Begin
                        zstack1 = zmolat
                        reldiff, zstack1, zmolat2, zmolat   &    redo_last
                     EndIf
                     field_to_compOK = 0
                   END
     'absdiff'   : BEGIN
;                     wdelete, 1   &   reset_titles = 1
                     reset_titles = 1
                     If (last.plot2d eq 'x-y') then Begin
                        zstack1 = zl
                        absdiff, zstack1, z2, zl   &   redo_last
                      EndIf Else If (last.plot2d eq 'depth-x') then Begin
                        zstack1 = zmolat
                        absdiff, zstack1, zmolat2, zmolat   &    redo_last
                     EndIf
                     field_to_compOK = 0
                   END
     'drawEvent' : BEGIN
                    If event.press then Begin
                       pressed = 1   &   xPress = event.x   &   yPress = event.y
                       nx=0   &   ny=0
                       px = [xPress, xPress + nx, xPress + nx, xPress, xPress]
                       py = [yPress, yPress, yPress + ny, yPress + ny, yPress]
                       device, GET_GRAPHICS_FUNCTION=old_graph_fct, $   ;Set xor graphics fct so that
                               SET_GRAPHICS_FUNCTION=6                  ;line overplotting returns
                                                                        ;to previous pixel value
                       WIDGET_CONTROL, draw_id, /DRAW_MOTION_EVENTS
                       last.action = action
                    EndIf
                    If (event.type eq 2) then Begin                     ;cursor is moving...
                       plots, px, py, col=ncols_avail-1, /dev, thick=1, lines=0 ;UnDraw previous box
                       nx = event.x - xPress   &   ny = event.y - yPress
                       px = [xPress, xPress + nx, xPress + nx, xPress, xPress]
                       py = [yPress, yPress, yPress + ny, yPress + ny, yPress]
                       plots, px, py, col=ncols_avail-1, /dev, thick=1, lines=0 ;Re-Draw box
                       last.action = action
                    EndIf
                    If event.release then Begin
                       if not pressed then GOTO, CONTINUE
                       plots, px, py, col=ncols_avail-1, /dev, thick=1, lines=0 ;UnDraw previous box
                       xRelease = event.x   &   yRelease = event.y
                       device, SET_GRAPHICS_FUNCTION=old_graph_fct              ; turn off line overplotting
                       WIDGET_CONTROL, draw_id, DRAW_MOTION_EVENTS=0
                       point_or_zoom, xPress, yPress, xRelease, yRelease
                       pressed = 0
                    EndIf
                 END

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

END ;============= end of selecter event handling routine task =============



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

bigbase_id = WIDGET_BASE(mainbase_id, /ROW)
toolbase_id = WIDGET_BASE(bigbase_id, /COLUMN)

fnamebase_id = WIDGET_BASE(toolbase_id, /FRAME, /COLUMN)
fgfiletitle = WIDGET_BASE(fnamebase_id, /ROW)
junk = WIDGET_LABEL(fgfiletitle, Value='Foreground file - ')
openfgfile = WIDGET_BUTTON( fgfiletitle, Value='Open..', Uvalue = 'archopen')
Fgfname_id = WIDGET_LABEL(fnamebase_id, /ALIGN_RIGHT)
bgfiletitle = WIDGET_BASE(fnamebase_id, /ROW)
junk = WIDGET_LABEL(bgfiletitle, Value='Background file - ')
openbgfile = WIDGET_BUTTON( bgfiletitle, Value='Open..', Uvalue = 'open_same')
Bgfname_id = WIDGET_LABEL(fnamebase_id, /ALIGN_RIGHT)
cmpbase_id = WIDGET_BASE(fnamebase_id, /ROW)
switchfiles_bttn_id = WIDGET_BUTTON( cmpbase_id, Value='Switch', Uvalue = 'SwitchFiles')
reldiff_bttn_id = WIDGET_BUTTON( cmpbase_id, Value='(F-B)/B', Uvalue = 'reldiff')
absdiff_bttn_id = WIDGET_BUTTON( cmpbase_id, Value='F-B', Uvalue = 'absdiff')

listbase_id = WIDGET_BASE(toolbase_id, /ROW)

varlist_id = WIDGET_LIST( listbase_id, VALUE=arch.list.vars,            $
                     UVALUE='wvarlist', YSIZE=(arch.list.nbvars<12) )

depthlistwidid = WIDGET_LIST( listbase_id, VALUE=arch.list.depth,       $
                     UVALUE='wdepthlist', YSIZE=(depth.dim<12) ) ;, XSIZE=11 )
listsgeo = WIDGET_INFO( listbase_id, /GEOMETRY )

togglebase = WIDGET_BASE( toolbase_id, /ROW)
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

labels_xs = listsgeo.xsize < togglegeo.xsize
pointedX_id = WIDGET_LABEL(pointbase_id, /ALIGN_LEFT, SCR_XSIZE=labels_xs)
pointedY_id = WIDGET_LABEL(pointbase_id, /ALIGN_LEFT, SCR_XSIZE=labels_xs)

WIDGET_CONTROL, Fgfname_id, SCR_XSIZE=labels_xs
WIDGET_CONTROL, Bgfname_id, SCR_XSIZE=labels_xs

;============ Create the draw widget, for contour plots  ==================
;============         Create the message widget          ==================
;============ !! SCREEN SIZE AND COLOR MAP TAKEN HERE !! ==================

toolgeo = WIDGET_INFO(toolbase_id, /GEOMETRY)
device, get_screen_size=screen_size
if (draw_xsize eq 0) then               $
   draw_xsize = screen_size(0) - (toolgeo.xsize + toolgeo.xoffset + 10*toolgeo.xpad)
msg_xsize = draw_xsize + toolgeo.xsize
msg_id = WIDGET_LABEL(mainbase_id, /FRAME, SCR_XSIZE=msg_xsize, /ALIGN_LEFT)
msggeo = WIDGET_INFO(msg_id, /GEOMETRY)
if (draw_ysize eq 0) then               $
   draw_ysize = screen_size(1) - msggeo.scr_ysize - 200
draw_id = WIDGET_DRAW(bigbase_id, Uvalue='drawEvent', /BUTTON_EVENTS,   $
                      COLORS=take_colors, XSIZE=draw_xsize, YSIZE=draw_ysize)
DEVICE, DECOMPOSED=0     ; turn off decomposed color representation for 24-bit devices

;======================== Create the upper menu bar ======================

;----------- File menu -----------------

file_menu = WIDGET_BUTTON(bar_base, Value='File', /Menu)
junk = WIDGET_BUTTON(file_menu, Value='Open new Foreground file...', Uvalue='archopen')
junk = WIDGET_BUTTON(file_menu, Value='Open Background file...', Uvalue='open_same')
closebgfile_id = WIDGET_BUTTON(file_menu, Value='Close Background file', Uvalue='closebgfile')
junk = WIDGET_BUTTON(file_menu, Value='Run ncbrowse instead of view2d', Uvalue='ncbrowse')
junk = WIDGET_BUTTON(file_menu, Value='Export data to ASCII...', Uvalue='asciiwrite')
junk = WIDGET_BUTTON(file_menu, Value='Export last plot to PostScript file...', Uvalue='plot_to_file')
junk = WIDGET_BUTTON(file_menu, Value='Export last plot to GIF file...',Uvalue='plot_gif')
junk = WIDGET_BUTTON(file_menu, Value='Print last plot...', Uvalue='print')
junk = WIDGET_BUTTON(file_menu, Value='Printer Setup...', Uvalue='printsetup')
junk = WIDGET_BUTTON(file_menu, Value='Pause execution', Uvalue='idl_shell')
junk = WIDGET_BUTTON(file_menu, Value='Exit', Uvalue='quit')

;----------- display menu --------------

display_menu = WIDGET_BUTTON(bar_base, Value='Display', /Menu)
disp_bttn2 = WIDGET_BUTTON(display_menu, Value='UnZoom', UVALUE='unzoom')
disp_bttn4 = WIDGET_BUTTON(display_menu, Value='Edit texts...', Uvalue='form_titles')
disp_bttn5 = WIDGET_BUTTON(display_menu, Value='Edit axes labels...', Uvalue='chgaxes')
junk =       WIDGET_BUTTON(display_menu, Value='Change years for Foreground file...',   $
                           Uvalue='chgyears')
disp_bttn6 = WIDGET_BUTTON(display_menu, Value='Smooth field', UVALUE='smooth')
If arch.OK.date and (x.name eq 'latitudes') then        $
   disp_bttn7 = WIDGET_BUTTON(display_menu, Value='Show/NotShow...', UVALUE='showSW')
disp_bttn8 = WIDGET_BUTTON(display_menu, Value='Color/NoColor', UVALUE='colorSW')
disp_bttn9 = WIDGET_BUTTON(display_menu, Value='Change colors...', UVALUE='colorch')

;----------- 2D-plots menu ----------------

molatfixlev_id = -999   &   molattotal_id = -999
plot2d = WIDGET_BUTTON(bar_base, Value='2D-plots', /Menu)
junk = WIDGET_BUTTON(plot2d, Value='Manual levels...', Uvalue='manscaler')
molatfixlev_id = WIDGET_BUTTON(plot2d, Value=                     $
                      'Month-Latitude at Pointed '+y.name,                $
                      Uvalue='molatfixlev')
If arch.OK.molat then WIDGET_CONTROL, molatfixlev_id, /SENSITIVE Else  $
     WIDGET_CONTROL, molatfixlev_id, SENSITIVE=0
molevfixlat_id = WIDGET_BUTTON(plot2d,                                 $
                               Value='Month-Level at Pointed '+x.name,    $
                               Uvalue='molevfixlat')
if arch.OK.molev then WIDGET_CONTROL, molevfixlat_id, /SENSITIVE Else  $
     WIDGET_CONTROL, molevfixlat_id, SENSITIVE=0
if arch.OK.totdens and (y.units eq 'km') then                          $
   molattotal_id = WIDGET_BUTTON(plot2d, Value=                   $
                      'Month-Latitude (column)', Uvalue='molattotal')

;----------- 1D-plots menu ----------------

plot1d_id = WIDGET_BUTTON(bar_base, Value='1D-plots', /Menu)
If arch.OK.diuvar then Begin
   junk = WIDGET_BUTTON(plot1d_id, Value='Time-dependent', /Menu)
   junk1 = WIDGET_BUTTON(junk, Value='24 hours', UVALUE='dvplotzoomed')
   junk2 = WIDGET_BUTTON(junk, Value='48 hours', UVALUE='dvplotunzoom')
EndIf
plot1dvert_id = WIDGET_BUTTON(plot1d_id, Value='Vertical', UVALUE='vertslice')
plot1dhor_id = WIDGET_BUTTON(plot1d_id, Value='Horizontal', UVALUE='horslice')

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

if (depth_id gt (depth.dim-1)) then depth_id = 0
if (var_id gt (arch.list.nbvars-1)) then var_id = 0

;=================== Realize the widgets defined above =====================

WIDGET_CONTROL, depthlistwidid, SET_LIST_SELECT=depth_id
WIDGET_CONTROL, varlist_id, SET_LIST_SELECT=var_id
If logsw then WIDGET_CONTROL, logbutton, /SET_BUTTON            $
 Else WIDGET_CONTROL, linbutton, /SET_BUTTON
If arch.OK.totdens then Begin
   If densSW then WIDGET_CONTROL, densbutton, /SET_BUTTON       $
    Else WIDGET_CONTROL, vmrbutton, /SET_BUTTON
EndIf
If autolevSW then WIDGET_CONTROL, autobutton, /SET_BUTTON               $
 Else WIDGET_CONTROL, fixbutton, /SET_BUTTON

If (main_xoffset eq -9999) or (main_yoffset eq -9999) then      $
  WIDGET_CONTROL, mainbase_id, /REALIZE                         $
 Else                                                           $
  WIDGET_CONTROL, mainbase_id, XOFFSET=main_xoffset, YOFFSET=main_yoffset, /REALIZE

; Determine nb of available colors BEFORE setting 'ps' and no more than 256 cols
; to account for decomposed color representationon 24-bit devices
ncols_avail = (!D.N_COLORS < 256 )

;====================== draw first contour =================================

last.action = 'create_widgets'
last.plot2d = 'x-y'   &   last.plot1d = ''
archread, arch.list.vars(var_id), depth_id, zl
viewpreproc
update_widgets

;===================== Call the event manager ==============================

XManager, CATCH=0      ; 0:prgm execution will stop when error is encountered.
XManager, "selecter", mainbase_id, $            ;register the widgets
                EVENT_HANDLER = "selecter_ev", $        ;with the XManager
                GROUP_LEADER = GROUP, _EXTRA=e

return

END ;==================== end of selecter main routine =======================
