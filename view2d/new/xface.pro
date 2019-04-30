;+
; NAME: Xface
;
; PURPOSE:
;       This routine creates the widgets for S.Chabrillat's IDL viewer of
;       2-dimensional (+time) NetCDF files and registers it with Xmanager.
;       Used the templates
;       acd.ucar.edu:/local/rsi/idl_4/lib/xmng_tmpl.pro and
;       acd.ucar.edu:/local/rsi/idl_4/examples/wexmast/mbar.pro and the example
;       program xpcolor.pro from IDL User's guide.
;
; MODIFICATION HISTORY:
;       Created from a template written by: Steve Richards, January, 1991.
;       Written by S.Chabrillat (simonc@oma.be), begun march, 1996
;       older version "selecter" developed by simonc until september, 1999
;       Totally revamped by simonc, new name "xface", begun september, 1999
;---------------
PRO xface

common hardware     ; inout : main_xoffset, main_yoffset, draw_xsize, draw_ysize
common i_com        ; in : narch
common draw_info     ; out : draw.widid
common widgets_info ; out : wid

;only one instance of the main "xface" widget is allowed. 
If XREGISTERED( 'xface' ) ne 0 then Begin
   junk = DIALOG_MESSAGE( 'view2d is already running !', /ERROR )
   return
EndIf

;=========================================================================
;====================== Create the full-screen widget ====================
;=========================================================================

wid.main.id = WIDGET_BASE( TITLE="view2d : an IDL browser for 2D+time netCDF files", $
                           /COLUMN, MBAR=menus )

bigbase = WIDGET_BASE( wid.main.id, /ROW )

;================= Create the left-side "tools" widgets ==================

tools = WIDGET_BASE( bigbase, /COLUMN )

;------- Foreground and Background files widgets -------------

foreback = WIDGET_BASE( tools, /FRAME, /COLUMN )
foretitle = WIDGET_BASE( foreback, /ROW )
junk = WIDGET_LABEL( foretitle, Value='Foreground file - ' )
wid.open_fore.id = WIDGET_BUTTON(foretitle, Value='Open...', Uvalue = 'open_fore' )
If narch eq 0 then Begin
   archlist = REPLICATE( '------- empty -------', 1 )
 EndIf Else archlist = arch(0:narch-1).flnm
wid.list_fore.id = WIDGET_DROPLIST( foreback, Value=archlist, Uvalue = 'set_fore' )
wid.back.id = WIDGET_BASE( foreback, /ROW )
junk = WIDGET_LABEL( wid.back.id, Value='Background file - ' )
wid.open_back.id = WIDGET_BUTTON(wid.back.id,Value='Open...',Uvalue = 'open_back' )
wid.list_back.id = WIDGET_DROPLIST( foreback, Value=archlist, Uvalue = 'set_back' )
wid.use_back.id = WIDGET_BASE( foreback, /ROW )
junk = WIDGET_BUTTON( wid.use_back.id, Value='Switch',          Uvalue = 'Switch' )
wid.diff.id = WIDGET_BASE( wid.use_back.id, /ROW )
junk = WIDGET_BUTTON( wid.use_back.id, Value='(F-B)/B',        Uvalue = 'reldiff' )
junk = WIDGET_BUTTON( wid.use_back.id, Value='F-B',            Uvalue = 'absdiff' )

;---------------- varlist and dlist widgets ------------------

wid.tools2.id = WIDGET_BASE( tools, /COLUMN )
lists = WIDGET_BASE( wid.tools2.id, /ROW )
If narch eq 0 then varlist = REPLICATE( '-- empty --',13)                           $
 Else varlist = arch(ifore).var(0:arch(ifore).nbvars-1)
wid.varlist.id = WIDGET_LIST( lists, VALUE=varlist, YSIZE=12,  Uvalue = 'varlist' )
If narch eq 0 then dlist = REPLICATE( '-- empty --',13)                             $
 Else dlist = arch(ifore).dstring(0:arch(ifore).d.dim-1)
wid.dlist.id = WIDGET_LIST( lists, VALUE=dlist, YSIZE=12,        Uvalue = 'dlist' )

;---------------------- switches widgets ---------------------

d2toggles = WIDGET_BASE( wid.tools2.id, /ROW, /FRAME )
junk = WIDGET_BUTTON( d2toggles, Value = '2D plot',             Uvalue = 'd2plot' )
junk = WIDGET_LABEL( d2toggles, Value = 'Levs:' )
levs = WIDGET_BASE( d2toggles, /ROW, /EXCLUSIVE )
junk = WIDGET_BUTTON( levs, Value = 'auto', /NO_RELEASE,      Uvalue = 'autolevs' )
junk = WIDGET_BUTTON( levs, Value = 'fix', /NO_RELEASE,        Uvalue = 'fixlevs' )

d1toggles = WIDGET_BASE( wid.tools2.id, /ROW, /FRAME )
junk = WIDGET_LABEL( d1toggles, Value='1D plot axis: ' )
linlog = WIDGET_BASE( d1toggles, /ROW, /EXCLUSIVE )
junk = WIDGET_BUTTON( linlog, Value='lin', /NO_RELEASE,          Uvalue = 'logsw' )
junk = WIDGET_BUTTON( linlog, Value='log', /NO_RELEASE,          Uvalue = 'linsw' )

wid.chem.id = WIDGET_BASE( wid.tools2.id, /ROW, /FRAME )
junk = WIDGET_LABEL( wid.chem.id, Value='Chem: ' )
vmrdens = WIDGET_BASE( wid.chem.id, /ROW, /EXCLUSIVE )
junk = WIDGET_BUTTON( vmrdens, Value='vmr', /NO_RELEASE,         Uvalue = 'vmrsw' )
junk = WIDGET_BUTTON( vmrdens, Value='dens', /NO_RELEASE,       Uvalue = 'denssw' )

;----------------------- Pointed widgets ---------------------

point = WIDGET_BASE( wid.tools2.id, /FRAME, /COLUMN )
pointtitle = WIDGET_BASE( point, /ROW )
junk = WIDGET_LABEL( pointtitle, Value='Pointed Coordinates - ' )
junk = WIDGET_BUTTON( pointtitle, Value='Reset',            Uvalue = 'ResetPoint' )
wid.pointedX.id = WIDGET_LABEL( point, /ALIGN_LEFT )
wid.pointedY.id = WIDGET_LABEL( point, /ALIGN_LEFT )

;========== Create the draw widget and the message widget =================
;============ !! SCREEN SIZE AND COLOR MAP TAKEN HERE !! ==================

toolgeo = WIDGET_INFO( tools, /GEOMETRY )
device, get_screen_size=screen_size
if draw_xsize eq 0 then                                                              $
   draw_xsize = screen_size(0)                                                       $
              - (toolgeo.xsize + toolgeo.xoffset + 10*toolgeo.xpad)
msg_xsize = draw_xsize + toolgeo.xsize
wid.msg.id = WIDGET_LABEL( wid.main.id, /FRAME,                                      $
                           SCR_XSIZE=msg_xsize, /ALIGN_LEFT )
msggeo = WIDGET_INFO( wid.msg.id, /GEOMETRY )
if draw_ysize eq 0 then               $
   draw_ysize = screen_size(1) - msggeo.scr_ysize - 200
draw.widid = WIDGET_DRAW( bigbase, /BUTTON_EVENTS, COLORS=take_colors,               $
                          XSIZE=draw_xsize, YSIZE=draw_ysize, Uvalue = 'drawEvent' )

;======================== Create the upper menu bar ======================

;----------- File menu -----------------

fmenu = WIDGET_BUTTON( menus, Value='File', /Menu )
wid.close_fore.id = WIDGET_BUTTON( fmenu, Value='Close Foreground',                  $
                                                             Uvalue = 'close_fore' )
wid.close_back.id = WIDGET_BUTTON( fmenu, Value='Close Background',                  $
                                                             Uvalue = 'close_back' )
junk = WIDGET_BUTTON( fmenu, Value='ncbrowse...',              Uvalue = 'ncbrowse' )
wid.xdata.id = WIDGET_BUTTON( fmenu,Value='Export data...', Uvalue = 'export_data' )
wid.xplot.id = WIDGET_BUTTON(fmenu,Value='Export plot...', Uvalue = 'plot_to_file' )
wid.print.id = WIDGET_BUTTON( fmenu, Value='Print plot...',       Uvalue = 'print' )
junk = WIDGET_BUTTON( fmenu, Value='Printer Setup...',       Uvalue = 'printsetup' )
junk = WIDGET_BUTTON( fmenu, Value='Pause execution',         Uvalue = 'idl_shell' )
junk = WIDGET_BUTTON( fmenu, Value='Exit',                         Uvalue = 'exit' )

;----------- display menu --------------

wid.disp.id = WIDGET_BUTTON( menus, Value='Display', /Menu)
junk = WIDGET_BUTTON( wid.disp.id, Value='UnZoom',               Uvalue = 'unzoom' )
junk = WIDGET_BUTTON( wid.disp.id, Value='Smooth field',         Uvalue = 'smooth' )
junk = WIDGET_BUTTON( wid.disp.id, Value='Edit texts...',     Uvalue = 'chgtitles' )
junk = WIDGET_BUTTON( wid.disp.id, Value='Edit axes labels...', Uvalue = 'chgaxes' )
junk = WIDGET_BUTTON(wid.disp.id,Value='Change years for F...', Uvalue= 'chgyears' )
junk = WIDGET_BUTTON( wid.disp.id, Value='Show/NotShow...',      Uvalue = 'showsw' )
junk = WIDGET_BUTTON( wid.disp.id, Value='Change colors...',    Uvalue = 'colorch' )
junk = WIDGET_BUTTON( wid.disp.id, Value='Color/NoColor',       Uvalue = 'colorsw' )

;----------- 2D-plots menu ----------------

wid.d2menu.id = WIDGET_BUTTON( menus, Value='2D-plots', /Menu )
wid.d2xy.id      = WIDGET_BUTTON( wid.d2menu.id, Value='d2xy',     Uvalue = 'd2xy' )
wid.d2dx_fix.id  = WIDGET_BUTTON( wid.d2menu.id, Value='d2dx_fix',                    $
                                                               Uvalue = 'd2dx_fix' )
wid.d2dx_col.id  = WIDGET_BUTTON( wid.d2menu.id, Value='d2dx_col',                    $
                                                               Uvalue = 'd2dx_col' )
wid.d2dy_fix.id  = WIDGET_BUTTON( wid.d2menu.id, Value='d2dy_fix',                    $
                                                               Uvalue = 'd2dy_fix' )
wid.d2dy_glob.id = WIDGET_BUTTON( wid.d2menu.id, Value='d2dy_glob',                   $
                                                              Uvalue = 'd2dy_glob' )
junk = WIDGET_BUTTON( wid.d2menu.id, Value='Manual levels...', Uvalue = 'chgscale' )

;----------- 1D-plots menu ----------------

wid.d1menu.id = WIDGET_BUTTON( menus, Value='1D-plots', /Menu )
wid.d1vert.id  = WIDGET_BUTTON( wid.d1menu.id, Value='Vertical', Uvalue = 'd1vert' )
wid.d1vertg.id = WIDGET_BUTTON( wid.d1menu.id, Value='d1vertg', Uvalue = 'd1vertg' )
wid.d1hor.id  = WIDGET_BUTTON( wid.d1menu.id, Value='Horizontal', Uvalue = 'd1hor' )
wid.d1time.id  = WIDGET_BUTTON( wid.d1menu.id, Value='d1time',   Uvalue = 'd1time' )
wid.d1diuvar.id = WIDGET_BUTTON( wid.d1menu.id, Val='Diurnal variation ', /Menu )
junk = WIDGET_BUTTON( wid.d1diuvar.id, Value='24 hours',     Uvalue = 'd1diuvar24' )
junk = WIDGET_BUTTON( wid.d1diuvar.id, Value='48 hours',     Uvalue = 'd1diuvar48' )

;----------- Animate ----------------

wid.movie.id = WIDGET_BUTTON( menus, Value='Movie', /Menu )
junk = WIDGET_BUTTON( wid.movie.id, Value='Preview',              Uvalue = 'movie' )
junk = WIDGET_BUTTON( wid.movie.id, Value='Save as MPEG file...',                     $
                                                             Uvalue = 'movie_mpeg' )

;----------- Help menu ----------------

help = WIDGET_BUTTON( menus, Value='Help', /HELP, /Menu)
junk = WIDGET_BUTTON( help, Value='Intro/get started...',    Uvalue = 'help_intro' )
junk = WIDGET_BUTTON( help, Value='Using view2d...',           Uvalue = 'help_use' )
junk = WIDGET_BUTTON( help, Value='Bugs/workarounds...',      Uvalue = 'help_bugs' )
junk = WIDGET_BUTTON( help, Value='About...',                     Uvalue = 'about' )

;======================================================================
;========== Realize and initialize the widgets defined above ==========
;======================================================================

If (main_xoffset eq -9999) or (main_yoffset eq -9999) then Begin
  WIDGET_CONTROL, wid.main.id, /REALIZE
 EndIf Else Begin
  WIDGET_CONTROL, wid.main.id, XOFFSET=main_xoffset, YOFFSET=main_yoffset, /REALIZE
EndElse
xface_update

;======================================================================
;================= Run the GUI, i.e. call the event manager ===========
;======================================================================

; Set CATCH=0 to stop prgm execution when error is encountered
XManager, CATCH=0

XManager, "xface", wid.main.id, EVENT_HANDLER = "xface_events"

END
