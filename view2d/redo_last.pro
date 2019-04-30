pro redo_last, only1d=only1d

common last_com         ; in : last.plot1d, last.plot2d
common last_fields      ; in : zl, zmolat, zmolev, z2, zmolat2, zmolev2, z3, zmolat3, zmolev3
common arch_info        ; in : arch, arch2, arch3, arch_to_compOK, arch_refOK
common dataranges       ; in : ip0, ip1, jp0, jp1
common last_fields      ; in : z2.compOK, z3.compOK
common plots_info       ; in : screen

CASE last.plot2d OF
   'x-y': BEGIN
     if zl.badvals then zl.matrix( where ( zl.valOK eq 0 ) ) = arch.missval
     if arch_to_compOK and z2.compOK then if z2.badvals then      $
        z2.matrix( where ( z2.valOK eq 0 ) ) = arch2.missval
     if arch_refOK and z3.compOK then if z3.badvals then          $
        z3.matrix( where ( z3.valOK eq 0 ) ) = arch3.missval
          END
   'depth-x': BEGIN
     if zmolat.badvals then $
        zmolat.matrix( where ( zmolat.valOK eq 0 ) ) = arch.missval
     if arch_to_compOK and z2.compOK then if zmolat2.badvals then      $
        zmolat2.matrix( where ( zmolat2.valOK eq 0 ) ) = arch2.missval
     if arch_refOK and z3.compOK then if zmolat3.badvals then          $
        zmolat3.matrix( where ( zmolat3.valOK eq 0 ) ) = arch3.missval
          END
   'depth-y': BEGIN
     if zmolev.badvals then $
        zmolev.matrix( where ( zmolev.valOK eq 0 ) ) = arch.missval
     if arch_to_compOK and z2.compOK then if zmolev2.badvals then      $
        zmolev2.matrix( where ( zmolev2.valOK eq 0 ) ) = arch2.missval
     if arch_refOK and z3.compOK then if zmolev3.badvals then          $
        zmolev3.matrix( where ( zmolev3.valOK eq 0 ) ) = arch3.missval
          END
ENDCASE

win_exist = bytarr(33)   &   win_exist(1) = 0B
If (!D.NAME eq screen) then device, window_state = win_exist

If (last.plot1d ne '') and (KEYWORD_SET(only1d) or win_exist(1)) then Begin

   CASE last.plot1d OF

      'f(y)'      : vertslicer, lat_avg=(last.action eq 'vertglobslice')
      'f(x)'      : horslicer
      'f(depth)'  : if last.plot2d eq 'depth-x' then month_val  $
                     else if last.plot2d eq 'depth-y' then month_val2
      'mesotvals' : mesotvals
      'lifetimes' : lifetimes
      'correl_n2o_var' : correl, 'n2o'
      'correl_n2o_noy' : correl, 'n2o', /noy
      'correl_n2o_bry' : correl, 'n2o', /bry
      'correl_n2o_cly' : correl, 'n2o', /cly
      'correl_ch4_var' : correl, 'ch4'
      'correl_ch4_noy' : correl, 'ch4', /noy
      'correl_ch4_bry' : correl, 'ch4', /bry
      'correl_ch4_bry_src' : correl, 'ch4', /bry_src
      'correl_ch4_cly' : correl, 'ch4', /cly
      'correl_h2o' : correl_h2o

    ELSE : message, 'Last 1d plot unknown', /TRACEBACK

   ENDCASE

EndIf

if not KEYWORD_SET(only1d) then viewpreproc

end
