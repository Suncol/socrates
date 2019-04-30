pro velofield

common arch_info, arch_flnm, cdfid, glob, glob_attr, diuvar, 		$
       depthlist, depth_is_date, nbfields, varlist, t_id, hm2d_id, hm2d
common axes, x, y, depth, yp
common select_id, var_id, last_type, depth_id, ip, jp, colorsw, logsw, dvzoom

arch_flnm = '/unicos_tmp/simonc/archive22.6+2.5yr.nc'
archopen, nopick=-1, /nowin			; now open the diur avg archive
x.units = 'degrees'
ip0 = 0   &   ip1 = x.dim-1   &   jp0 = 60   &   jp1 = y.dim-1

depth_id = 0					; plot first archived date
var_id = 2					; read v (m/s)
archread, varlist(var_id), depth_id, v
var_id = 3					; read w (m/s)
archread, varlist(var_id), depth_id, w
w.matrix = 100.*w.matrix   &   w.units = 'cm/s'

x_sub = fltarr(17)   &   y_sub = fltarr(21)
v_sub = fltarr(17,21)   &   w_sub = fltarr(17,21)	; extract subsets of u & v
For j = 0, 20 Do y_sub(j) = y.coord(60+3*j)
For i = 0, 16 Do Begin
    x_sub(i) = x.coord(1+2*i)
    For j = 0, 20 Do Begin
        v_sub(i,j) = v.matrix(1+2*i,60+3*j)
        w_sub(i,j) = w.matrix(1+2*i,60+3*j)
    EndFor
EndFor

v_sub(8:11,0:3) = 2e10   &   w_sub(8:11,0:3) = 2e10	; leave room for legend
v_sub(9,2) = 10.   &   w_sub(9,2) = 2.			; arrow for legend

xtick = ['90S','75S','60S','45S','30S','15S','Eq','15N','30N','45N','60N','75N','90N']

FmtStr = '(A3,"/",I2,"/",I4)'
mytitle = 'Velocity Field '+						$
    string(v.date.month,v.date.day,v.date.year,FORMAT=FmtStr)+' '+	$
    v.loc(0)+' '+v.loc(1)+' '+v.loc(2)+'!C'

if (!d.name eq 'X') then window, 0, xsize=1000, ysize=800	; or will not annotate axes, why??

velovect2, v_sub, w_sub, x_sub, y_sub, length=1.3, missing=1e10, 	$
	xmargin=[10,4], ymargin=[4,5],					$
	xrange=[-90,90], yrange=[60,120],				$
	xstyle=1, ystyle=9, ticklen=-0.015, title=mytitle,		$
        xtitle=x.name+' ('+x.units+')', ytitle=y.name+' ('+y.units+')',	$
        xticks=12, xtickname=xtick

If (N_ELEMENTS(yp) ne 0) then						$
   axis, /yaxis, /ylog, ystyle=1, ytitle=yp.name+' ('+yp.units+')',	$
         yrange = [yp.coord(jp0),yp.coord(jp1)], ticklen=-0.015, charsize=.5

plots,-10,60   &   plots, -10, 72, thick=2., /cont	; draw box for legend
plots, 40, 72, thick=2., /cont   &   plots, 40, 60, thick=2., /cont

plots, [10,19], [64,64]
plots,[10,10],[63.5,64.5]   &   plots,[19,19],[63.5,64.5]
xyouts, 15, 62, '10 m/s', align=.5			; annotate legend
xyouts, 15, 65, 'v', align=.5
plots, [24,24], [66,67.75]
plots,[23,25],[66,66]   &   plots,[23,25],[67.75,67.75]
xyouts, 26, 66.75, '2 cm/s'
xyouts, 21, 66.75, 'w'

        
return
end
