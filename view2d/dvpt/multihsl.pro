pro horslicer, get_jp=get_jp, multi=multi, _EXTRA=e

common axes
common last_fields
common dataranges
common select_id
common com_sun_hours

if (keyword_set(get_jp) and keyword_set(multi)) then   $
   MESSAGE, '/multi and /get_jp conflict !', /TRACEBACK

If (keyword_set(get_jp)) then Begin
   print,'Click along the desired horizontal line on the contour plot'
   pointer, ip, jp
   if (!D.NAME eq 'X') or (!D.NAME eq 'WIN') then   $
      window, 1, title='Fixed '+y.name+' plot'
Endif
If (keyword_set(multi)) then Begin
   If (n_elements(jstep) le 0) then Begin
      print,'Plots from '+y.name+' = '+STRING(y.coord(jp0))+' '+y.units+  $
            'to '+y.name+' = '+STRING(y.coord(jp1))+' '+y.units
      print, 'What vertical index increase between 2 consecutive plots ? (maximum '+  $
                STRING(jp1-jp0)+')'
      read, jstep   &   jstep = MIN( [jstep, jp1-jp0 ] )
   EndIf
   jmulti = (jp1-jp0)/jstep 
   label = strarr(jmulti+1)   &   xlabel=fltarr(jmulti+1)   &   ylabel=fltarr(jmulti+1)
   xlabel2=fltarr(jmulti+1)   &   ylabel2=fltarr(jmulti+1)
   iplabel = ip0+(ip1-ip0)/3   &   xlabel = x.coord(iplabel)
   iplabel2 = ip0+2*(ip1-ip0)/3   &   xlabel2 = x.coord(iplabel2)
   if (!D.NAME eq 'X') or (!D.NAME eq 'WIN') then   $
      window, 1, title='multi-'+y.name+' plot'
EndIf

WIDGET_CONTROL, /HOURGLASS

FmtStr = '(A3,"/",I2,"/",I4)'
mytitle = string(zl.date.month,zl.date.day,zl.date.year,FORMAT=FmtStr)+' ; '+      $
    zl.loc(0)+' ; '+zl.loc(1)+' ; '+zl.loc(2)

If (NOT keyword_set(multi)) then Begin

   yc = y.coord(jp)
   mytitle = mytitle+' ; '+ y.name+' : '+string(yc,FORMAT='(F4.0)')+' '+y.units
   If (N_ELEMENTS(yp) ne 0) then Begin
      !y.margin = [4,4]
      mytitle = mytitle+'!C'+ yp.name+' : '+string(yp.coord(jp))+' '+yp.units
   EndIf
   plot, x.coord(ip0:ip1), zl.matrix(ip0:ip1,jp), title=mytitle,		$
        xrange=[x.coord(ip0),x.coord(ip1)], xstyle=1, xmargin=[12,4],			$
	xtitle=x.name+' ('+x.units+')', ytitle=zl.name+' ('+zl.units+')', $
	psym=-1, _EXTRA=e

 EndIf Else Begin

   minz = MIN(zl.matrix(ip0:ip1,jp0))   &   maxz = MAX(zl.matrix(ip0:ip1,jp0))
   For iplot = 1, jmulti Do Begin
       jprov = jp0 + iplot*jstep
       provmin = MIN(zl.matrix(ip0:ip1,jprov))
       provmax = MAX(zl.matrix(ip0:ip1,jprov))
       minz = MIN( [ minz, provmin ] )   &    maxz = MAX( [ maxz, provmax ] )
   EndFor
   plot, x.coord(ip0:ip1), zl.matrix(ip0:ip1,jp0), title=mytitle,	$
        xrange=[x.coord(ip0),x.coord(ip1)], xstyle=1,			$
	xtitle=x.name+' ('+x.units+')', 				$
	ytitle=zl.name+' ('+zl.units+')', yrange=[minz,maxz],		$
	psym=-1, _EXTRA=e
   label(0) = STRING(y.coord(jp0),FORMAT='(F3.0)')+y.units
   ylabel(0) = zl.matrix(iplabel,jp0)   &   ylabel2(0) = zl.matrix(iplabel2,jp0)
   For iplot = 1, jmulti Do Begin
       myls = iplot MOD 6   &   mypsym = iplot MOD 8
       jprov = jp0 + iplot*jstep
       oplot, x.coord(ip0:ip1), zl.matrix(ip0:ip1,jprov), linestyle=myls, psym=-mypsym
       label(iplot) = STRING(y.coord(jprov),FORMAT='(F4.0)')+y.units
       ylabel(iplot) = zl.matrix(iplabel,jprov)
       ylabel2(iplot) = zl.matrix(iplabel2,jprov)
   EndFor
   xyouts, xlabel, ylabel, label
   xyouts, xlabel2, ylabel2, label
   
EndElse
          
If diuvarchsw then Begin
   realmin = 10^(!y.crange(0))   &   realmax = 10^(!y.crange(1))
   plots, [sr, sr], [realmin,realmax], linestyle=2, /data
   plots, [ss, ss], [realmin,realmax], linestyle=2, /data
EndIf


return
end
