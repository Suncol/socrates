pro lifetimes

common last_com         ; out: last.plot1d
common arch_info        ; in : arch.attr
common axes             ; in : x, y, yp, depth
common select_id        ; in : ip
common dataranges       ; in : jp0, jp1
common last_fields      ; in : zl
common plots_info       ; in : screen, valsrange, valSW
common formats          ; in : Xfmt, Dfmt

;---------------------------------------------------
;    Lifetime of vertical advection ( H/w )
;---------------------------------------------------
w = ncdf_valsget( 'w', ok=ok )
If not ok then Begin
    junk = DIALOG_MESSAGE( ['LIFETIMES: w not in Fg archive','plot cancelled'],$
                           /ERROR )
    return
EndIf

WIDGET_CONTROL, /HOURGLASS
If (!D.NAME eq screen) then Begin
   window, 1, xsize=1000, ysize=750, title='Lifetimes'
   my_xmargin = [8,38]
EndIf Else my_xmargin = [4,25]
Hs = 7.e3  ; conventional scale height for SOCRATES, 7km
v = REPLICATE( { name:'', vals:fltarr(y.dim) }, 5 )

nv = 1   &   v(0).name = 'advection (H/w)'   &   v(0).vals = Hs / ABS(w(ip,*))

;---------------------------------------------------
;    Lifetime of vertical eddy diffusion ( H*H/Kzz )
;---------------------------------------------------
kzz = ncdf_valsget( 'kzz', ok=ok )
If ok then Begin
   nv = nv+1   &   v(nv-1).name = 'eddydiff (H*H/Kzz)'
   v(nv-1).vals = Hs*Hs / kzz(ip,*)
EndIf

;---------------------------------------------------
;    Photochemical lifetime ( present in archive? )
;---------------------------------------------------
lt12 = ncdf_valsget( zl.name+'-lt12', ok=ok )
If ok then Begin
   nv = nv+1   &   v(nv-1).name = 'photochem (1/Li) at noon'
   v(nv-1).vals = lt12(ip,*)
EndIf
lt24 = ncdf_valsget( zl.name+'-lt24', ok=ok )
If ok then Begin
   nv = nv+1   &   v(nv-1).name = 'photochem (1/Li) at night'
   v(nv-1).vals = lt24(ip,*)
EndIf

;---------------------------------------------------
;    Lifetime of molecular diffusion ( Di/Hi/Hi )
;---------------------------------------------------
CASE zl.name OF
    'o2': spec = { mass:  32.    , A:-9.e29  , s:-9.e29 }
  'o2dg': spec = { mass:  32.    , A:-9.e29  , s:-9.e29 }
   'o2s': spec = { mass:  32.    , A:-9.e29  , s:-9.e29 }
   'o1d': spec = { mass:  16.    , A: 9.69e16, s: 0.774 }
    'oh': spec = { mass:  17.    , A:-9.e29  , s:-9.e29 }
    'cl': spec = { mass:  35.45  , A:-9.e29  , s:-9.e29 }
   'o3p': spec = { mass:  16.    , A: 9.69e16, s: 0.774 }
    'o3': spec = { mass:  48.    , A:-9.e29  , s:-9.e29 }
   'ho2': spec = { mass:  33.    , A:-9.e29  , s:-9.e29 }
   'no2': spec = { mass:  46.    , A:-9.e29  , s:-9.e29 }
    'no': spec = { mass:  30.    , A:-9.e29  , s:-9.e29 }
    'br': spec = { mass:  79.9   , A:-9.e29  , s:-9.e29 }
     'n': spec = { mass:  14.    , A:-9.e29  , s:-9.e29 }
   'clo': spec = { mass:  51.45  , A:-9.e29  , s:-9.e29 }
   'bro': spec = { mass:  95.9   , A:-9.e29  , s:-9.e29 }
   'no3': spec = { mass:  62.    , A:-9.e29  , s:-9.e29 }
     'h': spec = { mass:   1.    , A: 4.90e17, s: 0.708 }
   'n2o': spec = { mass:  44.    , A:-9.e29  , s:-9.e29 }
   'ch4': spec = { mass:  16.    , A:-9.e29  , s:-9.e29 }
   'h2o': spec = { mass:  18.    , A:-9.e29  , s:-9.e29 }
  'hno3': spec = { mass:  63.    , A:-9.e29  , s:-9.e29 }
  'n2o5': spec = { mass: 108.    , A:-9.e29  , s:-9.e29 }
    'co': spec = { mass:  28.    , A:-9.e29  , s:-9.e29 }
  'oclo': spec = { mass:  67.45  , A:-9.e29  , s:-9.e29 }
   'hcl': spec = { mass:  36.45  , A:-9.e29  , s:-9.e29 }
'clono2': spec = { mass:  97.45  , A:-9.e29  , s:-9.e29 }
  'hocl': spec = { mass:  52.45  , A:-9.e29  , s:-9.e29 }
   'cl2': spec = { mass:  70.9   , A:-9.e29  , s:-9.e29 }
  'h2o2': spec = { mass:  34.    , A:-9.e29  , s:-9.e29 }
 'clno2': spec = { mass:  81.45  , A:-9.e29  , s:-9.e29 }
   'hbr': spec = { mass:  80.9   , A:-9.e29  , s:-9.e29 }
'brono2': spec = { mass: 141.9   , A:-9.e29  , s:-9.e29 }
'ho2no2': spec = { mass:  79.    , A:-9.e29  , s:-9.e29 }
 'cl2o2': spec = { mass: 106.9   , A:-9.e29  , s:-9.e29 }
  'hobr': spec = { mass:  96.9   , A:-9.e29  , s:-9.e29 }
   'co2': spec = { mass:  44.    , A:-9.e29  , s:-9.e29 }
  'ch2o': spec = { mass:  30.    , A:-9.e29  , s:-9.e29 }
    'h2': spec = { mass:   2.    , A:-9.e29  , s:-9.e29 }
 'cfc10': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
 'cfc11': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
 'cfc12': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
'cfc113': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
'cfc114': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
'cfc115': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
'hcfc22': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
 'ch3cl': spec = { mass:  50.45  , A:-9.e29  , s:-9.e29 }
'ha1211': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
'ha1301': spec = { mass: 100.0   , A:-9.e29  , s:-9.e29 }
 'ch3br': spec = { mass:  94.9   , A:-9.e29  , s:-9.e29 }
 'chbr3': spec = { mass: 252.7   , A:-9.e29  , s:-9.e29 }
 'ch3o2': spec = { mass:  47.    , A:-9.e29  , s:-9.e29 }
'ch3ooh': spec = { mass:  48.    , A:-9.e29  , s:-9.e29 }
  'brcl': spec = { mass: 115.35  , A:-9.e29  , s:-9.e29 }
    'hf': spec = { mass:  20.    , A:-9.e29  , s:-9.e29 }
    ELSE: spec = { mass: -9.e29 }
ENDCASE
   
If spec.mass gt 0. then Begin
   nv = nv+1   &   v(nv-1).name = 'moldiff (Hi*Hi/Di)'
   k = 1.38065e-23   &   Nav = 6.022142e23   &   R = k*Nav   &   g0 = 9.80665
   T = ncdf_valsget( 'temperature' )
   Hi = 1.e3 * R * T(ip,*) / spec.mass / g0
   airmass = calc_wmole( )   &   ntot = arch.totdens(ip,*,zl.did)
   D12cst = 1.52e18 
   if spec.A gt 0. then Di = 1.e-4 * spec.A * T(ip,*)^spec.s / ntot  else      $
      Di = 1.e-4 * D12cst * SQRT( T(ip,*) * (1./spec.mass + 1./airmass(ip,*)) )$
         / ntot
   print, 'at level 100, Hi= ',Hi(100),' ; Di= ',Di(100)
   v(nv-1).vals = Hi*Hi / Di
EndIf

;---------------------------------------------------
;      Prepare the plot
;---------------------------------------------------
If valSW then Begin
   my_xrange = valsrange 
 EndIf Else Begin
   my_xrange = [ MIN(v(0).vals(jp0:jp1)), MAX(v(0).vals(jp0:jp1)) ]
   If nv gt 1 then For i = 1, nv-1 Do Begin
                      my_xrange(0) = MIN(v(i).vals(jp0:jp1)) < my_xrange(0)
                      my_xrange(1) = MAX(v(i).vals(jp0:jp1)) > my_xrange(1)
                   EndFor
   if my_xrange(0) lt 1. then my_xrange(0) = 1.  ; limit to 1 second
   if my_xrange(1) gt 3.15e8 then my_xrange(1) = 3.15e8  ; limit to 10years
   print,'my_xrange= ',my_xrange
EndElse
my_yrange = [ y.coord(jp0), y.coord(jp1) ]
thistring = up_subscript( zl )
my_title = 'Characteristic Times for '+thistring(0)+ ' ; '                   $
         + string(zl.date.month,zl.date.day,zl.date.year,FORMAT=Dfmt)+' ; '  $
         + arch.attr(1,1)+' ; '+x.name+' : '                                 $
         + string(x.coord(ip),FORMAT=Xfmt)+' '+x.units+' !C!C'
                
;---------------------------------------------------
;      Do the plot
;---------------------------------------------------
plot, v(0).vals, y.coord, title=my_title, thick=2,                           $
   /xstyle, xrange=my_xrange, xmargin=my_xmargin, /xlog, xtitle='Time (s)',  $
   /ystyle, yrange=my_yrange, ymargin=[4,8], ytitle=y.name+' ('+y.units+')'
plots, [.75,.8], [.8,.8], thick=2, /normal
xyouts, .81, .795, v(0).name, /normal

If nv gt 1 then For i = 1, nv-1 Do Begin
   mycolor = ncols_avail * i / nv
   oplot, v(i).vals, y.coord, color=mycolor, thick=2
   plots, [.75,.8], [.8-i*.05,.8-i*.05], color=mycolor, thick=2, /normal
   xyouts, .81, .795-i*.05, v(i).name, /normal
                EndFor
   
axis, /xaxis, /xlog, ticklen=-0.015,                               $
         /xstyle, xrange=my_xrange, xticks=5,                      $
         xtickv=[60.,3600.,86400.,7*86400.,30*86400.,365*86400.],  $
         xtickname=['1 min','1hour','1 day','1 week','1 month','1 year']

last.plot1d = 'lifetimes'

end
