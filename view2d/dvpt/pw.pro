pro pw

common arch_info, arch_flnm, cdfid, glob, glob_attr, diuvar, 		$
       depthlist, depth_is_date, nbfields, varlist, t_id, hm2d_id, hm2d
common axes, x, y, depth, yp
common select_id, var_id, last_type, depth_id, ip, jp, colorsw, logsw, dvzoom
common last_fields, zl, zstack1, zstack2, zmolat
common dataranges, idp0, idp1, ip0, jp0, ip1, jp1, jstep, jplat, nbmonth_used, $
       month_days0, month_str, jplay
common comscale, scale
common com_sun_hours, latpn

loadct, 39  					; "Rainbow+white" color table
ncols_avail = !D.N_COLORS			; to do BEFORE setting 'ps' !!
set_plot, 'ps'
device, FILENAME='ps/pw.ps', /LANDSCAPE, bits=8,	$
	 /COLOR, xsize=26.6, ysize=19., xoffset=1., yoffset=28.
!p.multi = [0,2,2,0,0]   &   !p.charsize = .65

arch_flnm = '/unicos_tmp/simonc/diuvarch22.10dr.nc'
archopen, nopick=-1, /nowin			; first open the diuvarch
x.units = 'degrees'   &   y.name = 'altitude'   &   yp.units = 'hPa'

var_id = 20					; 20 for o3 in diuvarch
depth_id = 0					; plot first archived date
archread, varlist(var_id), depth_id, zl	
zl.name = 'Ozone and OH'   &   zl.date.year = 1995
ip = 26						; latitude index -> latitude = 45N
jp = 80						; altitude index -> alt = 80km
logsw = 1   &   dvzoom = 1
diuvarplot, xmargin=[1,13], ymargin=[7,2], $
	yrange=[1.e-9,1.e-6], ytitle='volume mixing ratio'
xyouts, 12., 2.e-7, 'Ozone'
var_id = 17					; 17 for oh in diuvarch
archread, varlist(var_id), depth_id, zl
diuvarplot, /again
xyouts, 12., 4.e-9, 'OH'
sun_hours, sr, ss
xyouts, sr+.25, 1.3e-9, 'sunrise', charsize=.5
xyouts, ss+.25, 1.3e-9, 'sunset', charsize=.5

arch_flnm = '/unicos_tmp/simonc/archive22.6+2.5yr.nc'
archopen, nopick=-1, /nowin			; now open the diur avg archive
x.units = 'degrees'   &   yp.units = 'hPa'

var_id = 65					; ozone contour
archread, varlist(var_id), depth_id, zl	
zl.date.year = 1995
ip0 = 0   &   ip1 = x.dim-1   &   jp0 = 60   &   jp1 = y.dim-1
zl.name = 'Ozone'
xtick = ['90S','75S','60S','45S','30S','15S','Eq','15N','30N','45N','60N','75N','90N']
polardn
scale = {scaletype, mins:1., maxs:1., interv:'', nbmax:20, vector:FINDGEN(20) }
scale.vector = 2.5e-7*indgen(20)   &   scale.interv='2.5e-7'
scale.mins = scale.vector(0)   &   scale.maxs = scale.vector(19)
mycolors = 30 + indgen(20) * ncols_avail / 20
viewer, zl, /fill, c_color= mycolors, xmargin=[10,4], ymargin=[7,2], $
	yrange=[60,120], xticks=12, xtickname=xtick, xrange=[-90,90]
viewer, zl, /overplot, c_color=1
plots, [latpn,latpn], [y.coord(jp0),y.coord(jp1)], linestyle=2
plots, [-latpn,-latpn], [y.coord(jp0),y.coord(jp1)], linestyle=1

var_id = 0					; Temperature contour
archread, varlist(var_id), depth_id, zl	
zl.date.year = 1995
scale.vector = 130. + 15.*indgen(20)   &   scale.interv='15'+' '+zl.units
scale.mins = scale.vector(0)   &   scale.maxs = scale.vector(19)
mycolors = 15 + indgen(20) * ncols_avail / 20
viewer, zl, /fill, c_color= mycolors, xmargin=[1,13], ymargin=[4,5], $
	yrange=[60,120], xticks=12, xtickname=xtick, xrange=[-90,90]
viewer, zl, /overplot, c_color=1
plots, [latpn,latpn], [y.coord(jp0),y.coord(jp1)], linestyle=2
plots, [-latpn,-latpn], [y.coord(jp0),y.coord(jp1)], linestyle=1

velofieldpw, xmargin=[10,4], ymargin=[4,5], xticks=12, xtickname=xtick

;viewer, zl, c_color=1, xmargin=[10,4], xrange=[-90,90],$
;	ymargin=[4,5], yrange=[60,120], xticks=12, xtickname=xtick

device, /close
set_plot, 'X'
!p.multi =  0   &   !p.charsize = 1.

return
end
