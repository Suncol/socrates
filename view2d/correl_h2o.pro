pro correl_h2o, _EXTRA=e

common axes             ; in : x
common select_id        ; in: pointed, jp
common arch_info        ; in : arch, arch2.flnm, ...
common last_fields      ; in : zmolat
common dataranges       ; in : ip0, ip1, jp0, jp1
common plots_info       ; in : screen  ; inout : titles
common formats          ; in : Xfmt, Yfmt, Dfmt

If not pointed then Begin
   msg = DIALOG_MESSAGE( 'No altitude selected', /ERROR )
   return
EndIf

WIDGET_CONTROL, /HOURGLASS
If (!D.NAME eq screen) then window, 1, xsize=800, ysize=600, $
       title='Correlation plot'

minmissval = -1.e20   &   maxmissval = 1.e22
my_xrange = fltarr(2)   &   my_yrange = fltarr(2)

month_lat_calc, /altfix
vals = zmolat.matrix   &   nidx = 0
my_yrange(1) = 1.1*MAX(vals(idp0:idp1,ip0:ip1))
idx = where ( vals lt minmissval, nidx )
if nidx gt 0 then vals( idx ) = maxmissval
my_yrange(0) = 0.9*MIN(vals(idp0:idp1,ip0:ip1))

month_lat_calc, /altfix, varname='h2o'
h2o = zmolat.matrix
my_xrange(1) = 1.1*MAX(h2o(idp0:idp1,ip0:ip1))
idx = where ( h2o lt minmissval, nidx )
if nidx gt 0 then h2o( idx ) = maxmissval
my_xrange(0) = 0.9*MIN(h2o(idp0:idp1,ip0:ip1))

titles(0) = 'h2o - '+zl.name+' correlation'
xc0 = x.coord(ip0)   &   xc1 = x.coord(ip1)
titles(0) = titles(0) + ' ; lat from '+string(xc0,FORMAT=Xfmt)   $
                +'° to '+string(xc1,FORMAT=Xfmt)+'°' 
titles(0) = titles(0) + ' ; at z*= '+string(y.coord(jp),FORMAT=Yfmt)+'km'

set_xymargins, junk, my_ymargin, /plot1d

plot, h2o(idp0,ip0:ip1), vals(idp0,ip0:ip1), lines=0, psym=1,            $
            xrange=my_xrange, yrange=my_yrange, /xstyle, /ystyle,      $
            xmargin=[16,4], ymargin=my_ymargin, title=titles(0)+'!C',  $
            xtitle='h2o', ytitle=zl.name, _EXTRA=e
            
For time = idp0+1, idp1 do oplot, h2o(time,ip0:ip1), vals(time,ip0:ip1), $
                              lines=0, psym=1

titles(4) = + ' ; '+arch.attr(0,1)+' '+arch.attr(1,1)
days0date, depth.coord(idp0), date
titles(4) = titles(4) + ' ;  from  '                              $
          + string(date.month,date.day,date.year,FORMAT=Dfmt)
days0date, depth.coord(idp1), date
titles(4) = titles(4) + '  to  '                                  $
          + string(date.month,date.day,date.year,FORMAT=Dfmt)
plots, [.05,.1], [.09,.09], /normal, lines=0, psym=1
xyouts, .12, .08, titles(4), /normal
xyouts, .05, .03, titles(1), /normal

end
