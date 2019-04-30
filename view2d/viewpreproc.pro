pro viewpreproc

common hardware         ; in : colorSW, ncolors, white, black
common last_com         ; in : last.plot2d, last.action
common last_fields      ; in : zl, zmolat, zmolev
common arch_info        ; in : arch.latpn
common plots_info       ; in : my_device, ncols_avail, screen, show.pdn, reset_titles 
                        ;      reset_titles ; inout: titles
common select_id        ; in : jp, pointed
common dataranges       ; in : idp0, idp1, ip0, ip1, jp0, jp1
common axes             ; in : x.coord, y.coord
common axes2            ; in : x2.coord, y2.coord, ygeo2 ; temp mesopause level plotting
common widgets_info     ; in : draw_id
common comscale         ; in : scale

if my_device eq 'ascii' then GOTO, PLOT2D_CASE

If colorSW then Begin
   mycolors = INDGEN( ncolors+1 )
   mycolors(0) = 2
   mycolors(1:ncolors) = 8 + indgen(ncolors) * (ncols_avail+100) / ncolors
   If (!D.NAME eq 'PS' or !D.NAME eq 'PRINTER' or !D.NAME eq 'CGM') then Begin
;      mycolors(16:17) = mycolors(15)   &   mycolors(18:ncolors-1) = white
      mycolors = 8 + 254*INDGEN( scale.n ) / (scale.n+1)
      print, 'color indexes for Postscipt:'
      print, mycolors
   EndIf
EndIf

If (!D.NAME eq screen) then Begin
;   if screen_size(0) eq 1024 then !P.CHARSIZE = 0.7
   device, window_state = win_exist, DECOMPOSED=0
   If not win_exist(7) then Begin
      WIDGET_CONTROL, draw_id, GET_VALUE=draw_win
      wset, draw_win
   EndIf
EndIf

all_lat = 0   &   my_range = [x.coord(ip0),x.coord(ip1)]
If (ip0 eq 0) and (ip1 eq x.dim-1) and                     $
     ((x.name eq 'latitudes') or (x.name eq 'latitude')) then Begin
   all_lat = 1
   my_range = [ -90., 90. ]
   my_tick = ['90S','60S','30S','Eq','30N','60N','90N']
EndIf

autoscaler

PLOT2D_CASE:

CASE last.plot2d OF

 'depth-x' : BEGIN
     If my_device eq 'ascii' then Begin
        write_ascii, zmolat, depth, x   &   return
     EndIf
     vals = zmolat.matrix(idp0:idp1,ip0:ip1)
     cstval = MAX(vals) eq MIN(vals)
     if (MIN(vals) ne 0. ) then                                             $
        cstval = ( ABS(( MAX(vals) - MIN(vals))/MIN(vals)) lt 1.e-6 )
     if reset_titles then set_titles, zmolat, /molat
     If all_lat then month_lat_view, zmolat, /nodata, yticks=6,                         $
                              ytickname=my_tick, yminor=3, yrange=my_range  $
      Else month_lat_view, zmolat, /nodata, yrange=my_range
     If not cstval then Begin                                                    $
        if ColorSW then month_lat_view, zmolat, /fill, c_color=mycolors, /overplot
        month_lat_view, zmolat, /overplot, c_color=colorSW
      EndIf Else  xyouts, .2, .5, /normal, CHARSIZE=3,                      $
           'CONSTANT VALUE IS '+STRTRIM(MIN(vals),2)

             END

 'depth-y' : BEGIN
     If my_device eq 'ascii' then Begin
        write_ascii, zmolev, y, depth   &   return
     EndIf
     If colorSW then Begin
        month_lev_view, zmolev, /fill, c_color=mycolors
        month_lev_view, zmolev, /overplot, c_color=1
     EndIf else month_lev_view, zmolev
             END

 'x-y'     : BEGIN

     If my_device eq 'ascii' then Begin
        write_ascii, zl, x, y   &   return
     EndIf
     vals = zl.matrix(ip0:ip1,jp0:jp1)
     cstval = MAX(vals) eq MIN(vals)
     if (MIN(vals) ne 0. ) then                                             $
        cstval = ( ABS(( MAX(vals) - MIN(vals))/MIN(vals)) lt 1.e-6 )
     if reset_titles then set_titles, zl
     my_col = white
     if (!p.background eq white or !D.NAME eq 'CGM') then my_col = black
     zprov = zl   &   if cstval then zprov.matrix(*,*) = 0.
     If all_lat then viewer, zprov, my_yrange=my_yrange,  /nodata,          $
                             xticks=6, color=my_col, background=black,      $
                              xtickname=my_tick, xminor=3, xrange=my_range  $
      Else viewer, zprov, my_yrange=my_yrange, /nodata, xrange=my_range,    $
                   color=my_col, background=black
     If not cstval then Begin
        if ColorSW then viewer, zl, /fill, c_color=mycolors, /overplot, /noerase
        my_col2 = my_col   &   if colorSW then my_col2 = colorSW
        viewer, zl, /overplot, c_color=my_col2, /noerase
;        if zl.badvals then show_badvals
      EndIf Else  xyouts, .2, .5, /normal, CHARSIZE=3,                      $
           'CONSTANT VALUE IS '+STRTRIM(MIN(vals),2)
     show.yp = show.yp and (y.name eq 'log-p altitude')
     If show.yp then Begin
        pos_yp = CONVERT_COORD( .92, 0., /NORMAL, /TO_DATA)
        if (!p.background eq 255) then pos_yp = CONVERT_COORD( .9, 0., /NORMAL, /TO_DATA)
        axis, pos_yp(0), y.coord(jp0), yaxis=1, /ylog,ticklen=-0.015,       $
             ystyle=1, ytitle=yp.name+' ('+yp.units+')', color=my_col,      $
             yrange = [yp.coord(jp0),yp.coord(jp1)]
     EndIf
     If show.pdn and arch.latpn(0) ne 0. then Begin
        lpn = arch.latpn(zl.did)
        If (x.coord(ip0) lt lpn) and (x.coord(ip1) gt lpn) then      $
           plots, [lpn,lpn], [my_yrange(0),my_yrange(1)], linestyle=2 ; polar night limit
        If (x.coord(ip0) lt -lpn) and (x.coord(ip1) gt -lpn) then    $
           plots, [-lpn,-lpn], [my_yrange(0),my_yrange(1)], linestyle=1 ; polar day limit
     EndIf
     If show.mesolev and (zl.name eq 'temperature') then Begin
        mesoalt = FLTARR( x.dim)   &   my_symsize = 2
        if !p.background eq white then my_symsize = 1.5
        For l = 0, x.dim-1 do Begin
           mesoval = MIN( zl.matrix(l,jp0:jp1), mesolev )
           If show.ygeo then mesoalt(l) = ygeo.matrix(l,jp0+mesolev) $
            Else mesoalt(l) = y.coord(jp0+mesolev)
           plots, x.coord(l), mesoalt(l), psym=4, symsize=my_symsize, color=my_col
        EndFor
;        window,4    ; temp mesopause level plotting
;        plot, x.coord(0:x.dim-1), mesoalt, xrange=[-90.,90.],yrange=[75.,105.]
;        If z2.compOK then Begin
;           mesoalt2 = FLTARR( x2.dim)
;           For l = 0, x2.dim-1 do Begin
;              mesoval2 = MIN( z2.matrix(l,jp0:jp1), mesolev2 )
;              If show.ygeo then mesoalt2(l) = ygeo2.matrix(l,jp0+mesolev2) $
;               Else mesoalt2(l) = y2.coord(jp0+mesolev2)
;           EndFor
;           oplot, x2.coord(0:x.dim-1), mesoalt2, color=l2color
;        EndIf
     EndIf

             END

 ELSE : MESSAGE, 'Last 2d plot unknown', /TRACEBACK

ENDCASE

If !D.name eq screen then Begin
;   if screen_size(0) eq 1024 then !P.CHARSIZE = 1.0
   if pointed then draw_point
EndIf

return
end
