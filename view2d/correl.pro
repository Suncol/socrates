pro correl, xname, noy=noy, cly=cly, bry=bry, bry_src=bry_src, _EXTRA=e

common axes             ; in : x
common arch_info        ; in : arch, arch2.flnm, ...
common last_fields      ; in : zl
common dataranges       ; in : ip0, ip1, jp0, jp1
common plots_info       ; in : screen  ; inout : titles
common formats          ; in : Xfmt, Yfmt, Dfmt

WIDGET_CONTROL, /HOURGLASS
If (!D.NAME eq screen) then window, 1, xsize=800, ysize=600, $
       title='Correlation plot'

xvals = 1.e9 * ncdf_valsget( xname, ok=readok )
If not readok then Begin
   error = DIALOG_MESSAGE( xname+' not found. Aborting.', /ERROR )
   return
EndIf

nm = 0
If KEYWORD_SET( noy ) then Begin
   valsname = 'NOy'   &   nm = 9
   members = [ 'n', 'no', 'no2', 'hno3', 'n2o5', 'ho2no2', 'clono2', 'brono2', 'no3' ]
   coeffs  = [  1.,   1.,    1.,     1.,     2.,      1.,       1.,      1.,      1. ]
 EndIf Else If KEYWORD_SET( cly ) then Begin
   valsname = 'Cly'   &   nm = 9
   members = [ 'cl', 'clo', 'hocl', 'oclo', 'cl2o2', 'cl2', 'hcl', 'clono2', 'brcl' ]
   coeffs  = [  1.,   1.,    1.,     1.,     2.,      2.,     1.,      1.,      1. ]
 EndIf Else If KEYWORD_SET( bry ) then Begin
   valsname = 'Bry'   &   nm = 6
   members = [ 'br', 'bro', 'hobr', 'hbr', 'brono2', 'brcl' ]
   coeffs  = [  1.,   1.,    1.,     1.,       1.,      1. ]
 EndIf Else If KEYWORD_SET( bry_src ) then Begin
   valsname = 'Bry sources'   &   nm = 4
   members = [ 'ha1211', 'ha1301', 'ch3br', 'chbr3' ]
   coeffs  = [  1.,   1.,    1.,     3. ]
 EndIf Else Begin
   valsname = STRTRIM(zl.name,2)
   vals = 1.e9 * zl.matrix
EndElse

titles(0) = xname+' - '+valsname+' correlation'
xc0 = x.coord(ip0)   &   xc1 = x.coord(ip1)
titles(0) = titles(0) + ' ; lat from '+string(xc0,FORMAT=Xfmt)   $
                +'° to '+string(xc1,FORMAT=Xfmt)+'°' 
yc0 = y.coord(jp0)   &   yc1 = y.coord(jp1)
titles(0) = titles(0) + ' ; alt from '+string(yc0,FORMAT=Yfmt)   $
                   +'km to '+string(yc1,FORMAT=Yfmt)+'km'

msg = strarr(2)
If nm gt 0 then Begin
   msg(0) = valsname + ' = '   &   vals = fltarr(x.dim,y.dim)
   For i = 0, nm-1 Do Begin
      mval = ncdf_valsget( members(i), ok=readok )
      If readok then Begin
         vals = vals + coeffs(i)*mval
         if coeffs(i) eq 2. then cc = '2*' else cc = ''
         if coeffs(i) eq 3. then cc = '3*' else cc = ''
         msg(0) = msg(0) + cc + members(i) + ' + '
       EndIf Else Begin
         msg(1) = msg(1) + members(i) + ' ; '
      EndElse
   EndFor
   vals = 1.e9 * vals
EndIf else msg(0) = valsname + ' directly read on file'

titles(1) = msg(0)
if msg(1) ne '' then titles(1) = titles(1) + ' (missing species: '+ msg(1) +')'

my_xrange = [ 0., 1.1*MAX(xvals(ip0:ip1,jp0:jp1)) ]
my_yrange = [ 0., 1.1*MAX(vals(ip0:ip1,jp0:jp1)) ]

set_xymargins, junk, my_ymargin, /plot1d

plot, xvals(ip0,jp0:jp1), vals(ip0,jp0:jp1), lines=0, psym=1,            $
            xrange=my_xrange, yrange=my_yrange, /xstyle, /ystyle,      $
            xmargin=[16,4], ymargin=my_ymargin, title=titles(0)+'!C',  $
            xtitle=xname+' (ppbv)', ytitle=valsname+' (ppbv)', _EXTRA=e
            
For lat = ip0+1, ip1 do oplot, xvals(lat,jp0:jp1), vals(lat,jp0:jp1), $
                              lines=0, psym=1

titles(4) = string(zl.date.month,zl.date.day,zl.date.year,FORMAT=Dfmt)   $
          + ' ; '+arch.attr(0,1)+' '+arch.attr(1,1)
plots, [.05,.1], [.09,.09], /normal, lines=0, psym=1
xyouts, .12, .08, titles(4), /normal
xyouts, .05, .03, titles(1), /normal

If xname ne 'n2o' or valsname ne 'NOy' then return

xvec = xvals(ip0,jp0:jp1)    &   yvec = [ vals(ip0,jp0:jp1) ]
For lat = ip0+1,ip1 do Begin
   xvec = [ xvec, xvals(lat,jp0:jp1) ]   &   yvec = [ yvec, vals(lat,jp0:jp1) ]
EndFor
min_xvals = 200.   &   idx = where(xvec gt min_xvals)
If idx(0) ne -1 then Begin
   xvec = xvec( idx )   &   yvec = yvec( idx )
EndIf
linpar = LINFIT( xvec, yvec )
plots, [min_xvals,-linpar(0)/linpar(1)], [linpar(0)+min_xvals*linpar(1),0.], thick=5
xyouts, .75, .75, 'slope = '+STRTRIM(linpar(1),2), /normal

end
