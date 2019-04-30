pro viewer, z, my_yrange=my_yrange, nodata=nodata, _EXTRA = e 

common axes		; in : x, y, ygeo
common comscale		; in : scale
common plots_info	; in : titles, show.ygeo

WIDGET_CONTROL, /HOURGLASS

set_xymargins, my_xmargin, my_ymargin
x.i0 = MAX( [ x.i0, 0 ] )   &   x.i1 = MIN( [ x.i1, x.dim-1] )
y.i0 = MAX( [ y.i0, 0 ] )   &   y.i1 = MIN( [ y.i1, y.dim-1] )

If show.ygeo then Begin
   alt = { name:ygeo.name, units:ygeo.units, $
           coord:ygeo.matrix(x.i0:x.i1,0:y.dim-1) } 
   my_yrange = [ MIN(alt.coord(*,y.i0)), MAX(alt.coord(*,y.i1)) ]
 EndIf Else Begin
   alt = { name:y.name, units:y.units, coord:y.coord(0:y.dim-1) }
   my_yrange = [ alt.coord(y.i0), alt.coord(y.i1) ]
EndElse

nodata = KEYWORD_SET( nodata )
my_irregular = 0   &   vals = z.matrix(x.i0:x.i1,0:y.dim-1)
xvals = x.coord(x.i0:x.i1)   &   yvals = alt.coord

;-------------------------------------------------------------------------
;   Code commented here was for contouring of irregularly-gridded data.
;  Can be useful when there are "holes", but looks worse when the badvals
;  are at the border. Notice: in most cases the "hole" is caused simply by
;  another (var-specific) grid than the unique archive grid...
;-------------------------------------------------------------------------
;If z.badvals and not nodata then Begin
;   my_irregular = 1   &   nvals = x.dim*y.dim   &   i = 0
;   irvals = fltarr(nvals)
;   xirvals = fltarr(nvals)   &   yirvals = fltarr(nvals) 
;   For ip = 0, x.dim-1 Do Begin
;      For jp = 0, y.dim-1 Do Begin
;         If z.valOK(ip,jp) then Begin
;            irvals(i) = z.matrix(ip,jp)   &   xirvals(i) = x.coord(ip)
;            if show.ygeo then yirvals(i) = alt.coord(ip,jp) $
;              else yirvals(i) = alt.coord(jp)
;            i = i + 1
;         EndIf
;      EndFor
;   EndFor
;   vals = fltarr(i)    &   vals(0:i-1)  = irvals(0:i-1)
;   xvals = fltarr(i)   &   xvals(0:i-1) = xirvals(0:i-1)
;   yvals = fltarr(i)   &   yvals(0:i-1) = yirvals(0:i-1)
;EndIf

contour, vals, xvals, yvals, irregular=my_irregular,                       $
   ystyle=1, ymargin=my_ymargin, yrange=my_yrange,                         $
   xstyle=1, xmargin=my_xmargin, ticklen=-0.015, /follow,                  $
   level=scale.vector,                         $
;   c_linestyle = 2*(scale.vector lt 0.0),     $
   title=titles(0)+'!C'+titles(1),                                         $
   subtitle=titles(2)+'!C!C'+titles(3),                                    $
   xtitle=x.name+' ('+x.units+')',                                         $
   ytitle=alt.name+' ('+alt.units+')', nodata=nodata, _EXTRA = e
          
return

end
