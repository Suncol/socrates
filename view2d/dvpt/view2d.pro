PRO view2D, flnm=flnm

;=== ALL common blocks definition statements are made here ===============

common last_com, last

;----- Variables set in file 'preferences.pro' ----------------

common hardware, colorSW, take_colors, main_xoffset, main_yoffset,      $
       draw_xsize, draw_ysize
common path_com, local_path, remote_path, write_plots_path, write_ascii_path
common formats, Xfmt, Yfmt, Dfmt

;----- Variables set in file 'archopen.pro' ---------------------------

common arch_info, arch, arch2, arch_to_compOK, d2_ne_d, v2_ne_v
common select_id, pointed, var_id, depth_id, ip, jp, idp, ip2, jp2
common axes, x, y, depth, yp

;----- Variables to compare with a second archive file ----------------

common select_id2, var_id2, depth_id2, field_to_compOK
common axes2, x2, y2, depth2

;-- Variables set elsewhere, mainly in file 'selecter.pro' ------------

common plots_info, screen, draw_id, old_graph_fct, ncols_avail, logsw,       $
       densSW, autolevSW, dvzoom, pdnsw, show, titles, reset_titles
common last_fields, zl, zstack1, zstack2, zmolat, zmolev
common last_fields2, z2, zmolat2, zmolev2
common moving_box, pressed, xPress, yPress, px, py
common dataranges, idp0, idp1, idp2_0, idp2_1, ip0, jp0, ip1, jp1,           $
       jstep, jplat, jplay
common movies, animtitle
common label_widgets, pointedX_id, pointedY_id, msg, msg_id,                 $
       Fgfname_id, Bgfname_id
common widgets_info, mainbase_id, depthlistwidid, varlist_id, closebgfile_id,$
       maingeo, calcbttn_id, calcSW, calcbase_id, autobutton, fixbutton,     $
       molevfixlat_id, molatfixlev_id, molattotal_id,                        $
       plot1d_id, plot1dvert_id, cmpbase_id

;-- Variables set elsewhere, not in file 'selecter.pro' --------------

common comscale, scale

;======== set color depth to 8 to support 16- and 24-bit displays =========
device, pseudo_color=8

;==================== Set the preferences =================================
preferences

;================= Build the application window ===========================
; IN CONSTRUCTION : build_win

;====================== Open the archive ==================================
OPEN :
last = { action : 'continue', plot2d : '', plot1d : '' }
If KEYWORD_SET(flnm) then archopen, flnm = flnm Else archopen
If (last.action eq 'archopen') then GOTO, OPEN  $
 Else If (last.action ne 'continue') then GOTO, REOPEN

;==================== Initialize other variables ==========================
initialize

;==================== Run the program itself ==============================
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
