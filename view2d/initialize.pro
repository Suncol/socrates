pro axes_arch_null, x, y, yp, ygeo, depth, arch

common maxcom           ; in: all max dims

; NOTICE: new i0 and i1 will replace ip0,ip1,jp0,jp1,idp0,idp1 in common dataranges

label = {axislabels, name:'', nb : 0, txt : strarr(30), coord : fltarr(30) }

x = { name:'', units:'', dim:-1L, i0:-1L, i1: -1L, coord:fltarr(maxdimx)  }
y = { name:'', units:'', dim:-1L, i0:-1L, i1: -1L, coord:fltarr(maxdimy)  }
if N_ELEMENTS(yp) eq 0 then yp = y

ygeo = { name:'geometric altitude', units:'km',                               $
         matrix:FLTARR(maxdimx,maxdimy) }

depth = { name:'', units:'', type: '', dim:-1L, i0:-1L, i1: -1L,              $
          coord:fltarr(maxdimdepth), label:label }

maxldesc = 60
ok0 = { molev:0B, molat:0B, totdens:0B, ygeo:0B, missval:0B, desc:0B }
list0 = { depth:STRARR(maxdimdepth), nbvars:-1L, vars:STRARR(maxvars) }

arch = { flnm:'', cdfid:-1L, attr:STRARR(6,2), ok:ok0, list:list0,             $
               totdens:FLTARR(maxdimx,maxdimy,maxdimdepth),                    $
               latpn:FLTARR(maxdimdepth), missval:-9.e29,                      $
               desc:STRARR(maxldesc) }

end

;============================================================================

pro fields_null, zl, zmolat, zmolev

common maxcom           ; in: all max dims

date0 = { datetype, month:'', day:-1L, year:-1L }
   zl = { vid:0, did:0, fix:0, tdep: 0B, compOK:0B,                           $
             name:'', units:'not found',                                      $
             date:date0, loc:STRARR(3), matrix:FLTARR(maxdimx,maxdimy),       $
             badvals:0B, valOK:bytarr(maxdimx,maxdimy) }

zmolat = { name:zl.name, units:zl.units, loc:zl.loc,                          $
           matrix:fltarr(maxdimdepth,maxdimx),                                $
           badvals:0B, valOK:bytarr(maxdimdepth,maxdimx) }

zmolev = { name:zl.name, units:zl.units, loc:zl.loc,                          $
           matrix:fltarr(maxdimdepth,maxdimy),                                $
           badvals:0B, valOK:bytarr(maxdimdepth,maxdimy) }

end

;============================================================================

pro initialize

common hardware         ; in : colorSW, ncolors ; out : take_colors, white, black, screen*
common axes             ; out : x, y, yp, ygeo, depth
common arch_info        ; out : arch, arch_to_compOK, arch_refOK
common moving_box       ; out : pressed
common plots_info       ; out : densSW, logsw, autolevSW, dvzoom, screen, ncols_avail,
                        ;       show, reset_titles, titles, valsrange, valSW
common select_id        ; out : vapointed
common last_fields      ; out : zl, z2, z3, zmolat, zmolat2, zmolat3, zmolev, zmolev2, zmolev3
common dataranges       ; out : ip0, ip1, jp0, jp1
common widgets_info     ; out : calcSW
common comscale         ; out : scale


; Determine nb of available colors BEFORE setting 'ps' and no more than 256 cols
; to account for decomposed color representationon 24-bit devices
ncols_avail = (!D.N_COLORS < 256 )
device, DECOMPOSED=0
take_colors = 2   &   white = 255B   &   black = 0B
if colorSW then loadct, 39              ; "Rainbow+white" color table

arch_to_compOK = 0B   &   arch_refOK = 0B
logsw = 0B   &   densSW = 0B   &   autolevSW = 1B   &   calcSW = 0B
pointed = 0B   &   pressed = 0B
screen = !D.NAME   &   device, get_screen_size=screen_size
my_device = screen

titles = strarr(7)
reset_titles = 1
show = { title:1B, subtitle:1B, legends:1B, pdn:1B, grid:1B, yp:1B,           $
            mesolev:1B, ygeo:0B, linfit:0B }
valsrange = [0.,1.]   &   valSW = 0

n = 1+15
scale = {scaletype, mins:'', maxs:'', interv:'', n:n, vector:1.e30+FINDGEN(n)}

axes_arch_null, x, y, yp, ygeo, depth, arch   ; code above

fields_null, zl, zmolat, zmolev               ; code above

z2 = zl   &   z3 = zl
zprov = zl   &   zstack1 = zl   &   zstack2 = zl
zmolat2 = zmolat   &   zmolat3 = zmolat
zmolev2 = zmolev   &   zmolev3 = zmolev

end
