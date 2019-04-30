PRO view2D, flnm=flnm

;=== ALL common blocks definition statements are made here ===============

common last_com, last

;----- Variables set in file 'preferences.pro' ----------------

common maxcom, maxdimx, maxdimy, maxdimdepth, maxvars, maxldesc
common hardware, colorSW, ncolors, l2color, l3color, white, black,            $
       main_xoffset, main_yoffset, screen_size,                               $
       draw_xsize, draw_ysize, take_colors, print_command
common path_com, local_path, other_path, remote_path, write_plots_path,       $
       write_ascii_path
common formats, Xfmt, Yfmt, Dfmt

;----- Variables set in file 'archopen.pro' ---------------------------

common arch_info, arch, arch2, arch3, arch_to_compOK, arch_refOK
common select_id, pointed, ip, jp, idp
common axes, x, y, depth, yp, ygeo

;----- Variables to compare with a second & 3rd archive file ----------

common axes2, x2, y2, depth2, ygeo2
common axes3, x3, y3, depth3, ygeo3

;-- Variables set elsewhere, mainly in file 'selecter.pro' ------------

common plots_info, my_device, screen, draw_id, old_graph_fct, ncols_avail,   $
       mycolors, logsw, densSW, autolevSW, dvzoom, show,                     $
       titles, reset_titles, valsrange, valSW
common last_fields, zl, zprov, zmolat, zmolev,                               $
       z2, zmolat2, zmolev2, z3, zmolat3, zmolev3, zstack1, zstack2
common moving_box, pressed, xPress, yPress, px, py
common dataranges, idp0, idp1, idp2_0, idp2_1, idp3_0, idp3_1,              $
       ip0, jp0, ip1, jp1
common movies, animtitle
common label_widgets, pointedX_id, pointedY_id, msg, msg_id,                 $
       Fgfname_id, Bgfname_id, Refname_id
common widgets_info, mainbase_id, depthlistwidid, varlist_id,                $
       desc_fgfile_id, closebgfile_id, switchFR_id, closerefile_id,          $
       Bfollow_id, cmpbase_id, Rfollow_id, maingeo, autobutton, fixbutton,   $
       molevfixlat_id, molevtotal_id, molatfixlev_id, molattotal_id,         $
       plot1dhor_id, plot1dvert_id, plot1dvertglob_id, plot1dvertavg_id,     $
       plot1dtimes_id, plot1ddiuv_id, plot1dmesoval_id, zgrid_id

;-- Variables set elsewhere, not in file 'selecter.pro' --------------

common comscale, scale

;======== set decomposed=0 to support 16- and 24-bit displays =========
device, decomposed=0

;==================== Set the preferences =================================
preferences

;================= Build the application window ===========================
; IN CONSTRUCTION : build_win

;================= Pre-compile all user function ==========================
RESOLVE_ROUTINE, 'user_functions'

;==================== Initialize variables ================================
initialize

;====================== Open the archive ==================================
OPEN :
last = { action : 'continue', plot2d : '', plot1d : '' }
If KEYWORD_SET(flnm) then archopen, flnm = flnm Else archopen
If (last.action eq 'archopen') then GOTO, OPEN  $
 Else If (last.action ne 'continue') then GOTO, REOPEN

;==================== Run the program itself ==============================
x.i0 = 0   &   x.i1 = x.dim-1   &   y.i0 = 0   &   y.i1 = y.dim-1
jp0 = y.i0   &   jp1 = y.i1   &   ip0 = x.i0   &   ip1 = x.i1  ; to remove ASAP
selecter

REOPEN :
If (last.action eq 'archopen') then Begin
   last.action = 'continue'
   archopen
   if (last.action eq 'continue') then selecter
   GOTO, REOPEN
 EndIf Else If (last.action eq 'ncbrowse') then Begin
   If (arch.flnm ne '') then ncbrowse, arch.flnm, PATH=path     $
    Else ncbrowse, PATH=path
   desc = strarr(4)
   desc(0) = '0, LABEL, What do you want to do now ?'
   desc(1) = '1, BASE,, COLUMN, CENTER'
   desc(2) = '0, BUTTON, Open another file with view2d, QUIT, TAG=archopen'
   desc(3) = '0, BUTTON, Exit, QUIT, TAG=exit'
   form = CW_FORM(desc, /COLUMN, title='? QUESTION ?')
   If form.archopen then Begin
      last.action = 'archopen'   &   GOTO, REOPEN
   EndIf
EndIf


end
