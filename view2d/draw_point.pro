pro draw_point

common select_id	; in : ip, jp, idp ; out : pointed
common dataranges	; in : ip0, jp0, ip1, jp1, idp0, idp1
common axes		; in : x.coord, y.coord, depth.coord, ygeo
common plots_info	; in : ncols_avail, show.ygeo
common last_com		; in : last.plot2d

If (last.plot2d eq 'x-y') then Begin
   i_low = ( ip-1 > ip0 )   &    i_up = ( ip+1 < ip1 )
   j_low = ( jp-1 > jp0 )   &    j_up = ( jp+1 < jp1 )
   If i_low lt 0 or i_low gt x.dim-1 or ip lt 0 or ip gt x.dim-1    $
           or i_up lt 0 or i_up gt x.dim-1 then Begin
      pointed = 0   &   print, 'DRAW_POINT: X error'
      return
   EndIf
   x_low = .5 * (x.coord(i_low)+x.coord(ip))
   x_up = .5 * (x.coord(ip)+x.coord(i_up))
   If j_low lt 0 or j_low gt y.dim-1 or jp lt 0 or jp gt y.dim-1    $
           or j_up lt 0 or j_up gt y.dim-1 then Begin
      pointed = 0   &   print, 'DRAW_POINT: Y error'
      return
   EndIf
   If show.ygeo then Begin
      y_low = .5 * (ygeo.matrix(ip,j_low)+ygeo.matrix(ip,jp))
      y_up = .5 * (ygeo.matrix(ip,jp)+ygeo.matrix(ip,j_up))
    EndIf Else Begin
      y_low = .5 * (y.coord(j_low)+y.coord(jp))
      y_up = .5 * (y.coord(jp)+y.coord(j_up))
   EndElse
 EndIf Else If (last.plot2d eq 'depth-x') then Begin
   d_low = ( idp-1 > idp0 )   &   d_up = ( idp+1 < idp1 )
   If d_low lt 0 or d_low gt depth.dim-1 or idp lt 0 or idp gt depth.dim-1    $
           or d_up lt 0 or d_up gt depth.dim-1 then Begin
      pointed = 0   &   print, 'DRAW_POINT: Depth error'
      return
   EndIf
   j_low = ( ip-1 > ip0 )   &    j_up = ( ip+1 < ip1 )
   x_low = .5 * (depth.coord(d_low)+depth.coord(idp))
   x_up = .5 * (depth.coord(idp)+depth.coord(d_up))
   y_low = .5 * (x.coord(j_low)+x.coord(ip))
   y_up = .5 * (x.coord(ip)+x.coord(j_up))
 EndIf Else If (last.plot2d eq 'depth-y') then Begin
   d_low = ( idp-1 > idp0 )   &   d_up = ( idp+1 < idp1 )
   If d_low lt 0 or d_low gt depth.dim-1 or idp lt 0 or idp gt depth.dim-1    $
           or d_up lt 0 or d_up gt depth.dim-1 then Begin
      pointed = 0   &   print, 'DRAW_POINT: Depth error'
      return
   EndIf
   i_low = ( jp-1 > ip0 )   &    i_up = ( jp+1 < jp1 )
   x_low = .5 * (depth.coord(d_low)+depth.coord(idp))
   x_up = .5 * (depth.coord(idp)+depth.coord(d_up))
   y_low = .5 * (y.coord(i_low)+y.coord(jp))
   y_up = .5 * (y.coord(jp)+y.coord(i_up))
Endif

px = [ x_low, x_up, x_up, x_low, x_low ]
py = [ y_low, y_low, y_up, y_up, y_low ]

;Set xor graphics fct so that colors of line plots will change w/backgd color
device, GET_GRAPHICS_FUNCTION=old_graph_fct, SET_GRAPHICS_FUNCTION=2

plots, px, py, col=ncols_avail-1, /data, thick=1, lines=0	; Draw box

device, SET_GRAPHICS_FUNCTION=old_graph_fct

return
end

