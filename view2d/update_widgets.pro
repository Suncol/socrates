PRO update_widgets

common arch_info        ; in : arch, arch2.flnm, arch_to_compOK
common axes             ; in : x, y, depth, ygeo
common select_id        ; in : pointed, ip, jp, idp
common last_fields      ; in : zl, zmolat, z2.compOK, z3.compOK
common last_com         ; in : last.plot2d
common label_widgets    ; out : pointedX_id, pointedY_id, msg, msg_id, Fgfname_id, Bgfname_id
common widgets_info     ; in : most widgets ids
common formats          ; in : Xfmt, Yfmt
common plots_info       ; in : autobutton, fixbutton
common plots_info       ; in : show.mesolev
common dataranges       ; in : ip0, jp0, ip1, jp1, idp0, idp1

pointedcoord = strarr(2)   &   idp = zl.did
var_is_chem =  (zl.units eq 'vmr') or (zl.units eq 'molec/cm3') 
CASE last.plot2d OF
   'depth-x' : BEGIN
                 pointedX = depth.name + ' : '
                 pointedY = x.name + ' : '
                 if (x.name eq 'latitudes') then pointedY = 'lat : '
                 WIDGET_CONTROL, plot1dvert_id, SENSITIVE=0
                 WIDGET_CONTROL, plot1dvertglob_id, SENSITIVE=0
                 WIDGET_CONTROL, plot1dvertavg_id, SENSITIVE=0
                 WIDGET_CONTROL, molatfixlev_id, SENSITIVE=0
                 if molattotal_id ne -999 then   $
                   WIDGET_CONTROL, molattotal_id, SENSITIVE=0
                 WIDGET_CONTROL, cmpbase_id, SENSITIVE=0
                 WIDGET_CONTROL, Bfollow_id, SENSITIVE=0
                 WIDGET_CONTROL, Rfollow_id, SENSITIVE=0
                 WIDGET_CONTROL, switchFR_id, SENSITIVE=0
               END
   'depth-y' : BEGIN
                 pointedX = depth.name + ' : '
                 pointedY = y.name + ' : '
                 if (y.name eq 'log-p altitude') then pointedY = 'alt : '
                 WIDGET_CONTROL, plot1dvert_id, SENSITIVE=0
                 WIDGET_CONTROL, plot1dvertglob_id, SENSITIVE=0
                 WIDGET_CONTROL, plot1dvertavg_id, SENSITIVE=0
                 WIDGET_CONTROL, molevfixlat_id, SENSITIVE=0
                 WIDGET_CONTROL, cmpbase_id, SENSITIVE=0
                 WIDGET_CONTROL, Bfollow_id, SENSITIVE=0
                 WIDGET_CONTROL, Rfollow_id, SENSITIVE=0
                 WIDGET_CONTROL, switchFR_id, SENSITIVE=0
               END
   'x-y' :     BEGIN
                 pointedX = x.name + ' : '
                 pointedY = y.name + ' : '
                 if (x.name eq 'latitudes') then pointedX = 'lat : '
                 if (y.name eq 'log-p altitude') then pointedY = 'alt : '
                 WIDGET_CONTROL, plot1dvert_id, /SENSITIVE
                 WIDGET_CONTROL, plot1dvertglob_id, /SENSITIVE
                 WIDGET_CONTROL, plot1dvertavg_id, /SENSITIVE
                 if molattotal_id gt 0 then   $
                    WIDGET_CONTROL, molattotal_id, $
                       SENSITIVE=(var_is_chem and arch.OK.molat and arch.OK.totdens and zl.tdep)
                 WIDGET_CONTROL, molevtotal_id, SENSITIVE=( arch.OK.molev and zl.tdep )
                 WIDGET_CONTROL, cmpbase_id, SENSITIVE=z2.compOK
                 WIDGET_CONTROL, Bfollow_id, SENSITIVE=arch_to_compOK
                 WIDGET_CONTROL, Rfollow_id, SENSITIVE=arch_refOK
                 WIDGET_CONTROL, switchFR_id, SENSITIVE=z3.compOK
               END
ENDCASE

If last.action eq 'zoom' then Begin
   If (last.plot2d eq 'x-y') then Begin
      msg = 'Zoomed from ('+STRING(x.coord(ip0),format=Xfmt)+','
      msg = msg + STRING(y.coord(jp0),format=Yfmt) + ') to ('
      msg = msg + STRING(x.coord(ip1),format=Xfmt)+','
      msg = msg + STRING(y.coord(jp1),format=Yfmt)+').'
    EndIf Else If (last.plot2d eq 'depth-x') then Begin
      msg = 'Zoomed from (' + arch.list.depth(depth.i0) + ','
      msg = msg + STRING(x.coord(ip0),format=Xfmt) + ') to ('
      msg = msg + arch.list.depth(depth.i1) + ','
      msg = msg + STRING(x.coord(ip1),format=Xfmt)+').'
    EndIf Else If (last.plot2d eq 'depth-y') then Begin
      msg = 'Zoomed from (' + arch.list.depth(depth.i0) + ','
      msg = msg + STRING(y.coord(jp0),format=Yfmt) + ') to ('
      msg = msg + arch.list.depth(depth.i1) + ','
      msg = msg + STRING(y.coord(jp1),format=Yfmt)+').'
   EndIf
 EndIf Else If pointed then Begin
   msg = 'At pointed coordinates, ' + zl.name + ' = '
   WIDGET_CONTROL, plot1dhor_id, /SENSITIVE
   WIDGET_CONTROL, molatfixlev_id, SENSITIVE=(arch.OK.molat and zl.tdep)
   WIDGET_CONTROL, molevfixlat_id, SENSITIVE=(arch.OK.molev and zl.tdep)
   CASE last.plot2d OF
      'depth-x' : BEGIN
                    pointedX = pointedX + arch.list.depth(idp) +' '+ depth.units
                    pointedY = pointedY + STRING(x.coord(ip),FORMAT=Xfmt) +' '+ x.units
                    msg = msg + STRING(zmolat.matrix(idp,ip)) + zmolat.units
                  END
      'depth-y' : BEGIN
                    pointedX = pointedX + arch.list.depth(idp) +' '+ depth.units
                    pointedY = pointedY + STRING(y.coord(jp),FORMAT=Xfmt) +' '+ y.units
                    msg = msg + STRING(zmolev.matrix(idp,jp)) + zmolev.units
                  END
      'x-y' :     BEGIN
                    WIDGET_CONTROL, plot1dvert_id, /SENSITIVE
                    pointedX = pointedX +       $
                         STRING(x.coord(ip),FORMAT=Xfmt) +' '+ x.units
                    If show.ygeo then pointedY = pointedY +                            $
                         STRING(ygeo.matrix(ip,jp),FORMAT='(f6.2)') +' '+ ygeo.units       $
                     Else pointedY = pointedY +                                        $
                         STRING(y.coord(jp),FORMAT=Yfmt) +' '+ y.units
                    If zl.valOK(ip,jp) then                               $
                       msg = msg + STRING(zl.matrix(ip,jp))+' '+zl.units  $
                     Else msg = msg + 'MISSING value'
                  END
   ENDCASE
 EndIf Else Begin
   pointedX = pointedX + '----'   &   pointedY = pointedY + '----'
   msg = 'Ready. To get a 1D-plot, first select a point on the 2D-plot'
   WIDGET_CONTROL, plot1dvert_id, SENSITIVE=0
   WIDGET_CONTROL, plot1dhor_id,  SENSITIVE=0
   if arch.OK.molat then WIDGET_CONTROL, molatfixlev_id, SENSITIVE=0
   if arch.OK.molev then WIDGET_CONTROL, molevfixlat_id, SENSITIVE=0
EndElse

WIDGET_CONTROL, depthlistwidid, SENSITIVE=zl.tdep
WIDGET_CONTROL, pointedX_id, SET_VALUE=pointedX
WIDGET_CONTROL, pointedY_id, SET_VALUE=pointedY
WIDGET_CONTROL, zgrid_id, SENSITIVE=arch.OK.ygeo
WIDGET_CONTROL, msg_id, SET_VALUE=msg

WIDGET_CONTROL, Fgfname_id, SET_VALUE=arch.flnm
If arch_to_compOK then Begin
  WIDGET_CONTROL, Bgfname_id, SET_VALUE=arch2.flnm
 EndIf Else WIDGET_CONTROL, Bgfname_id, SET_VALUE='---none---'
If arch_refOK then Begin
  WIDGET_CONTROL, Refname_id, SET_VALUE=arch3.flnm
 EndIf Else WIDGET_CONTROL, Refname_id, SET_VALUE='---none---'
WIDGET_CONTROL, desc_fgfile_id, SENSITIVE=arch.ok.desc
WIDGET_CONTROL, closebgfile_id, SENSITIVE=arch_to_compOK
WIDGET_CONTROL, closerefile_id, SENSITIVE=arch_refOK
WIDGET_CONTROL, plot1dmesoval_id, SENSITIVE=show.mesolev and zl.name eq 'temperature'
WIDGET_CONTROL, plot1dtimes_id, SENSITIVE=pointed and var_is_chem

If autolevSW then WIDGET_CONTROL, autobutton, /SET_BUTTON               $
 Else WIDGET_CONTROL, fixbutton, /SET_BUTTON

return
end
