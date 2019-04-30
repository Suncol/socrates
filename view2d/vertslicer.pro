pro vertslicer, lat_avg=lat_avg, time_avg=time_avg, _EXTRA = e

common arch_info        ; in : arch, arch2, arch3
common axes             ; in : x, y, yp, depth
common axes2            ; in : y2
common axes3            ; in : y3
common last_fields      ; in : zl, z2, z3
common last_com         ; in : last.plot2d
common dataranges       ; in : ip0, ip1, idp0, idp1, jp0, jp1
common plots_info       ; in : logsw, show, reset_titles, valsrange, valSW 
                        ; inout : titles
common select_id        ; in : ip, idp
common hardware         ; in : colorSW, l2color
common formats          ; in : Xfmt
common path_com         ; in : write_ascii_path

;-------------------------------------------------------------------
;         Return if error with optional args &/or switches
;-------------------------------------------------------------------

if KEYWORD_SET( lat_avg ) and zl.badvals then Begin
   msg = DIALOG_MESSAGE( 'Missing values - lat average not possible', /ERROR )  
   return
EndIf
if KEYWORD_SET( time_avg ) and not KEYWORD_SET( lat_avg ) then Begin
   msg = DIALOG_MESSAGE( 'Time average possible only with lat average', /ERROR )  
   return
EndIf
if KEYWORD_SET( time_avg ) and show.ygeo then Begin
   msg = DIALOG_MESSAGE( 'Geo altitude selected - time average not possible', /ERROR )
   return
EndIf
if KEYWORD_SET( lat_avg ) and z2.compOK then if z2.badvals then Begin
   msg = DIALOG_MESSAGE( 'Missing vals in background field '               $
                            + '- lat average not possible', /ERROR )  
   return
EndIf

my_thick = 3
If (!D.NAME eq screen) then Begin
   window, 1, xsize=800, ysize=600, title='Vertical plot'
   my_thick = 2
EndIf

WIDGET_CONTROL, /HOURGLASS

;-------------------------------------------------------------------
;         Prepare titles
;-------------------------------------------------------------------

set_xymargins, my_xmargin, my_ymargin, /plot1d
If reset_titles then set_titles, zl, /plot1d, time_avg=time_avg

If (last.plot2d eq 'x-y') and reset_titles then Begin
   If show.title then Begin
      If not KEYWORD_SET( lat_avg ) then Begin
         xc = x.coord(ip)
         titles(0) = titles(0) + ' ; '+ x.name+' : '+string(xc,FORMAT=Xfmt)+' '+x.units
      EndIf Else Begin
         xc0 = x.coord(ip0)   &   xc1 = x.coord(ip1)
         titles(0) = titles(0) + ' ; lat avg from '+string(xc0,FORMAT=Xfmt)   $
                   +' to '+string(xc1,FORMAT=Xfmt) 
         If KEYWORD_SET( time_avg ) then titles(0) = titles(0) + ' ; time average'
      EndElse
   EndIf
EndIf

;-------------------------------------------------------------------
;         Calc vert axis and range & data values from Fg file
;-------------------------------------------------------------------

If show.ygeo then alt = { name:ygeo.name, units:ygeo.units, coord:ygeo.matrix(ip,*) } $
 Else alt = { name:y.name, units:y.units, coord:y.coord(*) }

minmissval = -1.e20   &   maxmissval = 1.e22

If not KEYWORD_SET( lat_avg ) then Begin
   value = zl.matrix(ip,*)
 EndIf Else Begin
   value = FLTARR( y.dim )
   If show.ygeo then alt.coord = ygeo.matrix(x.dim/4,*)
   idp = zl.did   &   my_idp0 = idp   &   my_idp1 = idp   &   mymatrix = zl.matrix
   If KEYWORD_SET( time_avg ) then Begin
      my_idp0 = idp0   &   my_idp1 = idp1
   EndIf
   For idp = my_idp0, my_idp1 Do Begin
      If KEYWORD_SET( time_avg ) then mymatrix = NCDF_VALSGET( zl.name )
      For ja = 0, y.dim-1 Do                                                           $
         value(ja) = value(ja)                                                         $
                   + TOTAL( COS(!pi*x.coord(ip0:ip1)/180.) * mymatrix(ip0:ip1,ja) )    $
                    / TOTAL( COS(!pi*x.coord(ip0:ip1)/180.) )
   EndFor
   If KEYWORD_SET( time_avg ) then value = value / (my_idp1-my_idp0+1)
   idp = zl.did
EndElse

my_yrange = [ alt.coord(jp0), alt.coord(jp1) ]

;-------------------------------------------------------------------
;         Calc data values from Bg file
;-------------------------------------------------------------------

If z2.compOK then Begin
   If show.ygeo then  $
      alt2 = { name:ygeo2.name, units:ygeo2.units, coord:ygeo2.matrix(ip,*) }  $
    Else alt2 = { name:y2.name, units:y2.units, coord:y2.coord(*) }
   my_yrange = [ my_yrange(0) < alt2.coord(jp0),                        $
                 my_yrange(1) > alt2.coord(jp1) ]
   If not KEYWORD_SET( lat_avg ) then Begin
      z2_vals = z2.matrix(ip,*) 
    EndIf Else Begin
      If show.ygeo then alt2.coord = ygeo2.matrix(x.dim/4,*)
      z2_vals = FLTARR( y.dim )
      idp2 = z2.did   &   my_idp2_0 = idp2   &   my_idp2_1 = idp2   &   mymatrix = z2.matrix
      If KEYWORD_SET( time_avg ) then Begin
         my_idp2_0 = idp2_0   &   my_idp2_1 = idp2_1
      EndIf
      For idp2 = my_idp2_0, my_idp2_1 Do Begin
         If KEYWORD_SET( time_avg ) then mymatrix = NCDF_VALSGET( z2.name, /bg )
         For ja = 0, y.dim-1 Do                                                           $
            z2_vals(ja) = z2_vals(ja)                                                     $
                        + TOTAL( COS(!pi*x.coord(ip0:ip1)/180.) * mymatrix(ip0:ip1,ja) )  $
                               / TOTAL( COS(!pi*x.coord(ip0:ip1)/180.) )
      EndFor
      If KEYWORD_SET( time_avg ) then z2_vals = z2_vals / (my_idp2_1-my_idp2_0+1)
      idp2 = z2.did
   EndElse
EndIf

;-------------------------------------------------------------------
;         Calc data values from Ref file
;-------------------------------------------------------------------

refOK = z3.compOK
if refOK then if z3.badvals and KEYWORD_SET( lat_avg ) and KEYWORD_SET( time_avg ) then refOK = 0
If refOK then Begin
   If show.ygeo then $
      alt3 = { name:ygeo3.name, units:ygeo3.units, coord:ygeo3.matrix(ip,*) }  $
    Else alt3 = { name:y3.name, units:y3.units, coord:y3.coord(*) }
   my_yrange = [ my_yrange(0) < alt3.coord(jp0),                        $
                 my_yrange(1) > alt3.coord(jp1) ]
   If not KEYWORD_SET( lat_avg ) then Begin
      z3_vals = z3.matrix(ip,*)   &   z3_valOK = z3.valOK(ip,0:y3.dim-1)
   EndIf Else Begin
      If show.ygeo then alt3.coord = ygeo3.matrix(x.dim/4,*)
      z3_vals = FLTARR( y3.dim )
      z3_valOK = BYTARR( y3.dim )   &   if not z3.badvals then z3_valOK(*) = 1B
      idp3 = z3.did   &   my_idp3_0 = idp3   &   my_idp3_1 = idp3   &   mymatrix = z3.matrix
      If KEYWORD_SET( time_avg ) then Begin
         my_idp3_0 = idp3_0   &   my_idp3_1 = idp3_1
      EndIf
      For idp3 = my_idp3_0, my_idp3_1 Do Begin
         If KEYWORD_SET( time_avg ) then mymatrix = NCDF_VALSGET( z3.name, /ref )
         For ja = 0, y3.dim-1 Do Begin
            If not z3.badvals then  z3_vals(ja) = z3_vals(ja)                             $
                        + TOTAL( COS(!pi*x3.coord(ip0:ip1)/180.) * mymatrix(ip0:ip1,ja) )  $
                               / TOTAL( COS(!pi*x3.coord(ip0:ip1)/180.) )                  $
             Else Begin
                totcoslat = 0.
                For ia = 0, x3.dim-1 Do Begin
                   If z3.valOK(ia,ja) then Begin
                      z3_valOK(ja) = 1B
                      coslat = COS(!pi*x3.coord(ia)/180.)
                      totcoslat = totcoslat + coslat
                      z3_vals(ja) = z3_vals(ja) + coslat * mymatrix(ia,ja)
                   EndIf
                EndFor
                if z3_valOK(ja) then z3_vals(ja) = z3_vals(ja) / totcoslat
            EndElse
         EndFor
      EndFor
      If KEYWORD_SET( time_avg ) then z3_vals = z3_vals / (my_idp3_1-my_idp3_0+1)
      idp3 = z3.did
   EndElse
EndIf

;-------------------------------------------------------------------
;         Calculate X-axis range
;-------------------------------------------------------------------

my_xrange = fltarr(2)   &   nidx = 0
If zl.badvals then Begin
   idx = where ( zl.valOK(ip,*) eq 0B, nidx )
   if nidx gt 0 then value( idx ) = maxmissval
EndIf
my_xrange(0) = MIN(value(jp0:jp1))
if nidx gt 0 then value( idx ) = minmissval
my_xrange(1) = MAX(value(jp0:jp1)) 
If z2.compOK then Begin
   nidx = 0
   If zl.badvals then Begin
      idx = where ( z2.valOK(ip,*) eq 0B, nidx )
      if nidx gt 0 then z2_vals( idx ) = maxmissval
   EndIf
   my_xrange(0) = MIN(z2_vals(jp0:jp1)) < my_xrange(0)
   if nidx gt 0 then z2_vals( idx ) = minmissval
   my_xrange(1) = MAX(z2_vals(jp0:jp1)) > my_xrange(1)
EndIf
If refOK then Begin
   nidx = 0
   If z3.badvals then Begin
      idx = where ( z3.valOK(ip,*) eq 0B, nidx )
      if nidx gt 0 then z3_vals( idx ) = maxmissval
   EndIf
   my_xrange(0) = MIN(z3_vals(jp0:jp1)) < my_xrange(0)
   if nidx gt 0 then z3_vals( idx ) = minmissval
   my_xrange(1) = MAX(z3_vals(jp0:jp1)) > my_xrange(1)
EndIf

junk = WHERE( value(jp0:jp1) le 0. and value(jp0:jp1) ne minmissval, count )
mylogsw = logsw   &   If logsw and (count gt 0) then mylogsw = 0

If valSW then Begin
   my_xrange = valsrange
   if valsrange(0) gt 0. then mylogsw = logsw 
EndIf
  
thistring = up_subscript( zl )

;-------------------------------------------------------------------
;         Vertical plot
;-------------------------------------------------------------------

If not KEYWORD_SET( lat_avg ) then index = where(zl.valOK(ip,0:y.dim-1) eq 1B)   $
 Else index = indgen(y.dim)

plot, value(index), alt.coord(index), title=titles(0)+'!C'+titles(1),   $
     yrange=my_yrange, ystyle=1, ymargin = my_ymargin,                  $
     xrange=my_xrange, xstyle=1, xmargin = my_xmargin, xlog=mylogsw,    $
     xtitle=thistring(0)+' ('+thistring(1)+')',                         $
     ytitle=alt.name+' ('+alt.units+')', thick=2,                       $
     ticklen=.02+.98*show.grid, min_value=minmissval+1.,                $
     xgridstyle=show.grid, ygridstyle=show.grid, _EXTRA = e

If z2.compOK  then Begin
   If not KEYWORD_SET( lat_avg ) then index = where(z2.valOK(ip,0:y2.dim-1) eq 1)
   If index(0) ne -1 then Begin
      If colorsw then                                                        $
         oplot, z2_vals(index), alt2.coord(index), color=l2color,            $
                       thick=my_thick                                        $
       Else oplot, z2_vals(index), alt2.coord(index), linestyle=2,           $
                         thick=my_thick
   EndIf
EndIf
If refOK  then Begin
   index = where(z3_valOK(0:y3.dim-1) eq 1B)
   if index(0) ne -1 then If colorsw then $
            oplot, z3_vals(index), alt3.coord(index), color=l3color,         $
                                         thick=my_thick                      $
                       Else oplot, z3_vals, alt3.coord, linestyle=3,         $
                                         thick=my_thick
EndIf

If show.yp then Begin
   pos_yp = CONVERT_COORD( .9, 0., /NORMAL, /TO_DATA)
   axis, pos_yp(0), y.coord(jp0), yaxis=1, /ylog,ticklen=-0.015,        $
         ystyle=1, ytitle=yp.name+' ('+yp.units+')',                    $
         yrange = [yp.coord(jp0),yp.coord(jp1)]
EndIf

If show.legends then print_legends, titles

;-----------------------------------------------------------------------------
;         Save time-averaged data (should be done elsewhere but data is local)
;-----------------------------------------------------------------------------

If KEYWORD_SET( time_avg ) then Begin
   msg = DIALOG_MESSAGE( 'Save averaged data to an ASCII file?', /Question )
   If msg eq 'Yes' then Begin
      def_fname = write_ascii_path+zl.name+'.dat'
      fname = DIALOG_PICKFILE( file=def_fname, filter='*.dat',  $
              title='Provide name for ASCII file to write' )
      if (fname eq '') then return
      junk = FINDFILE( fname, COUNT=c )
      If (c ne 0) then Begin
         junk = DIALOG_MESSAGE('This file already exists. OK to overwrite ?',/QUESTION)
         if (junk eq 'No') then return
      EndIf
      openw, 1, fname
      printf, 1, '! ASCII file written by the IDL/NetCDF Viewer for NCAR/ACD 2D model NetCDF output files'
      printf, 1, '! This data was written the '+SYSTIME()+' from NetCDF file '+arch.flnm
      printf, 1, '! ' + titles(0)
      printf, 1, '! ' + titles(4)
      printf, 1, '! log-p alt |  pressure | ' + zl.name
      printf, 1, '!     (km)  |      (mb) |     (' + zl.units + ')'
      printf, 1, '!-----------|-----------|-----------------------'
      For j = 0, y.dim-1 Do Begin
         line = string(y.coord(j),FORMAT='(f12.0)') + string(yp.coord(j),FORMAT='(e12.4)') 
         line = line + string( value(j), FORMAT='(e12.4)')
         printf, 1, line
      EndFor
      close, 1     
   EndIf
EndIf

end
