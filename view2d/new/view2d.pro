PRO view2D

WIDGET_CONTROL, /HOURGLASS

;=== ALL common blocks definition statements are made here ================

;==========================================================================
;==================== Set the preferences =================================
;==========================================================================

common max_com, maxdim, narchmax, maxlabels
common hardware, colorSW, ncolors, take_colors, lcolormin, lcolorint,        $
       main_xoffset, main_yoffset, draw_xsize, draw_ysize, print_command
common path_com, arch_path1, arch_path2, write_plots_path, write_ascii_path
common formats, Xfmt, Yfmt, Dfmt

preferences
maxlabels = 30
device, DECOMPOSED=0
if colorSW then loadct, 39                ; "Rainbow+white" color table

;==========================================================================
;============ Define and initialize important structures ==================
;==========================================================================

common plots_info, show

show = { title : 1,                                                         $
         subtitle : 1B,                                                     $
         legends : 1B,                                                      $
         pdn : 1B,                                                          $
         grid : 1B,                                                         $
         yp : 1B,                                                           $
         autolev : 1B,                                                      $
         dvzoom : 0B,                                                       $
         dens : 0B,                                                         $
         log : 0B }

;==========================================================================

common draw_info, draw

draw = { drawstruc, take_colors : 2,                                        $
                    pointed : 0B,                                           $
                    widid : -1,                                             $
                    winid : -1,                                             $
                    ncols_avail : (!D.N_COLORS<256),                        $
                    screen : !D.NAME  }

;==========================================================================

common i_com, narch, ifore, iback, action, pos

ifore = 0   &   iback = -1   &   narch = 0   &   action = ''

pos = REPLICATE( { ix : -1,                                                 $
                   iy : -1,                                                 $
                   id : 0,                                                  $
                   ivar : 0,                                                $
                   comp : 0B,                                               $
                   ixr : INTARR(2),                                         $
                   iyr : INTARR(2),                                         $
                   idr : INTARR(2) },              narchmax )

;==========================================================================

common arch_info, arch

a = { axis, name : '',                                                      $
            units : '',                                                     $
            dim : 0,                                                        $
            coord : FLTARR(maxdim) }

arch = REPLICATE( { archstruc, flnm : '',                                   $
                               cdfid : -1,                                  $
                               nlon : 0,                                    $
                               attr : STRARR(3,2),                          $
                               ok : { okstruc, socrates : 0B,               $
                                               diuvar : 0B,                 $
                                               y2p : 0B,                    $
                                               d_as_xplot : 0B,             $
                                               d_is_date : 0B,              $
                                               ntot : 0B },                 $
                               nbvars : -1,                                 $
                               var: STRARR(maxdim),                         $
                               units: STRARR(maxdim),                       $
                               x  : {axis},                                 $
                               y  : {axis},                                 $
                               y2 : {axis},                                 $
                               d  : {axis},                                 $
                               dstring : STRARR(maxdim),                    $
                               latpn : FLTARR(maxdim) },    narchmax )

;======================================================================

common last_com, last

junk = { labelstruc, name : '',                                             $
                     nb : 0,                                                $
                     txt : STRARR(maxlabels),                               $
                     coord : FLTARR(maxlabels) }

junk = { plotaxis, labels : {labelstruc},                                   $
                   axis : {axis},                                           $
                   mina : 0.,                                               $
                   maxa : 0. }

d1 = { plot1d, type : 'none',                                               $
               titles : STRARR(4),                                          $
               x : {plotaxis},                                              $
               y : {plotaxis} }

junkfm = REPLICATE( { field, name : '',                                     $
                             units : '',                                    $
                             loc : STRARR(3),                               $
                             slt : FLTARR(maxdim),                          $
                             vals : FLTARR(maxdim,maxdim),                  $
                             ntot : FLTARR(maxdim,maxdim) },   2 )

d2 = { plot2d, levels : { levstruc, minl : 0.,                              $
                                   maxl : 0.,                               $
                                   interv : '',                             $
                                   nbmax : 30,                              $
                                   vals:FINDGEN(30) },                      $
               INHERITS plot1d,                                             $
               field : {field} }

last = { laststruc, action : '',                                            $
                    xyfield : junkfm,                                       $
                    dxfield : junkfm,                                       $
                    dyfield : junkfm,                                       $
                    dlabels : {labelstruc},                                 $
                    plot : { plotstruc, type : 'none',                      $
                                        d2 : {plot2d},                      $
                                        d1 : {plot1d} } }

last.plot.type = 'd2'   &   last.plot.d2.type = 'xy'

;======================================================================

common widgets_info, wid             ; inititalized in file 'xface.pro'

w = { widgetstruc, id : -1L,                                                $
                   val : STRARR(maxdim),                                    $
                   set : 0B,                                                $
                   ok : 0B,                                                 $
                   dim : 1 }

wid = { main:w, open_fore:w, list_fore:w,                                   $
        back:w, open_back:w, list_back:w, use_back:w, diff:w,               $
        tools2:w, varlist:w, dlist:w, chem:w, pointedX:w, pointedY:w,       $
        msg:w,                                                              $
        close_fore:w, close_back:w, xdata:w, xplot:w, print:w,              $
        disp:w,                                                             $
        d2menu:w, d2xy:w, d2dx_fix:w, d2dx_col:w, d2dy_fix:w, d2dy_glob:w,  $
        d1menu:w, d1vert:w, d1vertg:w, d1hor:w, d1time:w, d1diuvar:w,       $
        movie:w }

;ram = N_TAGS(last,/length) + N_TAGS(arch,/length) + N_TAGS(pos,/length)     $
;    + N_TAGS( wid,/length)
;junk = DIALOG_MESSAGE( 'total RAM initialized : '+                          $
;                        STRTRIM(ram/1024.,2)+' kb', /INFO )

; return
;======================================================================
;===== Call "fake" routines to force compilation of small routines ====
;======================================================================
calc_utils
chgarch_utils
draw_utils
file_utils
string_utils

;======================================================================
;==================== Create and run the GUI ==========================
;======================================================================

xface

End
