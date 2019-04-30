PRO plot_budget, vars, l, _EXTRA = e

common dataranges       ; in : jp0, jp1
common axes             ; in : y
common plots_info       ; in : logsw, show.grid

my_xrange = [ MIN(vars.vals(l,jp0:jp1)), MAX(vars.vals(l,jp0:jp1)) ]
my_yrange = [ y.coord(jp0), y.coord(jp1) ]
junk = WHERE(vars.vals(jp0:jp1) le 0., count)
mylogsw = logsw   &   If logsw and (count gt 0) then mylogsw = 0

; Default !x.margin=[10,3]. Make room for legend
If (!P.MULTI(0) gt 0) and (!P.MULTI(0) lt 5) then Begin     ; right column
   my_xmargin = [4,22]
   my_ytitle = ''   &   my_ystyle=5
 EndIf Else Begin
   my_xmargin = [28,-4]                                      ; left column
   my_ytitle = y.name+' ('+y.units+')'   &    my_ystyle=1
EndElse

plot, vars(0).vals(l,*), y.coord(*),  xmargin=my_xmargin,                     $
     yrange=my_yrange, ystyle=my_ystyle, ytitle=my_ytitle, ymargin=[2,3],     $
     xrange=my_xrange, xstyle=1, xlog=mylogsw, xtitle=vars(0).units,          $
     color=vars(0).color, linest = vars(0).linest, _EXTRA = e

nv = N_ELEMENTS(vars)
For iv = 1, nv-1 Do Begin
    oplot, vars(iv).vals(l,*), y.coord(*),                                    $
           color=vars(iv).color, linest=vars(iv).linest, thick=2
EndFor

;===================== Draw box and legends ========================
;         To adjust norm coords, try changing 1st ul and uc

ul = 0.030   &   uc = 0.003

c = CONVERT_COORD( my_xrange, my_yrange, /TO_NORMAL )
If my_ytitle eq '' then Begin                             ; right column
   x0 = c(0,1)   &   y0 = c(1,0)   &   x1 = 1.   &   y1 = c(1,1)
   plots, [x0,x0], [y0,y1], /normal                       ; fake right axis
   x0 = x0 + 3*uc
EndIf Else Begin                                          ; left column
   x0 = 0.   &   y0 = c(1,0)   &   x1 = c(0,0)-2*ul   &   y1 = c(1,1)
EndElse
if my_xrange(0) lt 0. and my_xrange(1) gt 0. then $
   plots, [0.,0.], my_yrange, thick=0.5         ; center line
; plots, [x0,x1,x1,x0,x0], [y0,y0,y1,y1,y0], linest=2, /normal

For iv = 0, nv-1 Do Begin
   y1 = y1 - ul + uc
   plots, [x0,x0+ul], [y1,y1], /normal,                                       $
          color=vars(iv).color, linest=vars(iv).linest, thick=2
   xyouts, x0+ul+2*uc, y1-uc, vars(iv).name, /normal,                           $
          color=vars(iv).color, charsize=0.7
EndFor

END

;==================================================================

PRO calc_terms, vars, mapl

common axes             ; in : x
common last_fields      ; in : zl.did, zprov

nl = N_ELEMENTS(mapl)   &   nv = N_ELEMENTS(vars)
For iv = 0, nv-1 Do Begin
   archread, vars(iv).name, zl.did, zprov
   vars(iv).units = zprov.units   &   vars(iv).date = zprov.date
   For l = 0, nl-1 Do Begin
       vars(iv).lats(l) = x.coord(mapl(l))
       vars(iv).vals(l,*) = zprov.matrix(mapl(l),*)
   EndFor
;   if iv gt 0 then if vars(iv).units ne vars(iv-1).units then stop, 'Bad Units'
EndFor

END

;==================================================================

PRO heat_budget

common arch_info        ; in : arch, arch2
common axes             ; in : x, y, depth
common axes2            ; in : y2
common last_fields      ; in : zl.did, z2.did, zprov
common select_id        ; in : pointed, ip
common dataranges       ; in : jp0, jp1
common formats          ; in : Xfmt
common plots_info       ; in : screen, ncols_avail


nl = 1 ; 3 ; 4
If pointed then mapl = [ ip ]  $ ;6-1 , 20-1, 30-1 ] ; , -99 ]
   Else Begin
      error = DIALOG_MESSAGE( 'no latitude pointed', /ERROR )
      return
EndElse

WIDGET_CONTROL, /HOURGLASS
fname = 'heat_budget.ps'   &   set_plot,'ps'   &   !p.font = 0

device, FILENAME=fname, /color, bits=8, /TIMES,        $ ;, Font_size=22, /helvetica, $
        xsize=20., ysize=27., yoffset=1.5, xoffset=0.5
!P.MULTI = [0,2,4,0,1]  ; 2 columns, 4 rows per page; top/bottom then left/right

days0date, depth.coord(zl.did), date
var = { name:'', units:'', date:date, labels:strarr(2),                    $
        lats:fltarr(nl), vals:fltarr(nl,y.dim), color:0, linest:0 }

loadct, 39               ; "Rainbow+white" color table
; mycolors = 15 + INDGEN(8) * !D.N_COLORS / 8   &   mycolors(0) = 0
black = 0   &   blue = 90   &   green = 150   &   red = 240
mycolors = [ black, red, green, blue, black, red, green, blue  ]
mylinest = [   0,    0,     0,    0,   2,     2,     2,   2    ]

;=================== Main terms breakdown ================================

q = REPLICATE( var, 5)
q(*).color = mycolors(0:4)   &   q(*).linest = mylinest(0:4)
q(*).name  = [ 'q', 'SolHeat', 'qcool', 'ChemHeat', 'q' ]
calc_terms, q, mapl

prov = calc_q_adiab(ok=ok)
If not ok then Begin
   q(4).name = 'FAILED' 
 EndIf Else Begin
   q(4).name = 'q_adiab'
   For l = 0, nl-1 Do q(4).vals(l,*) = prov(mapl(l),*)
EndElse
   

;=================== Temperature comparison ==============================

nv = 2   &   t = REPLICATE( var, nv)
t(*).color = mycolors(0:nv-1)   &   t(*).linest = mylinest(0:nv-1)
t(*).name  = [ 'temperature', 'temperature' ]
calc_terms, t, mapl
If arch_to_compOK then Begin
   archread, t(1).name, z2.did, zprov, /bg_arch
   t(1).name = 'T from '+arch2.attr(0,1)
   t(1).units = zprov.units   &   t(1).date = zprov.date
   For l = 0, nl-1 Do Begin
       t(1).lats(l) = x.coord(mapl(l))
       t(1).vals(l,*) = zprov.matrix(mapl(l),*)
   EndFor
EndIf

;=================== Chemical heating breakdown ==========================

nv = 8   &   qc = REPLICATE( var, nv )
qc(*).color = mycolors(0:nv-1)   &   qc(*).linest = mylinest(0:nv-1)
qc(*).name  = [ 'ChemHeat','O-O-M','O-O2-M','O-O3','H-O3','O-OH','O-HO2','H-O2-M' ]
calc_terms, qc, mapl

;======================= Chemical species ================================

nv = 8   &   nic = REPLICATE( var, nv)
nic(*).color = mycolors(0:nv-1)   &   nic(*).linest = mylinest(0:nv-1)
nic(*).name  = [ 'o3-noon','o3p-noon','h-noon','oh-noon',                      $
                 'o3-night','o3p-night','h-night','oh-night' ]
calc_terms, nic, mapl

;=================== Infrared cooling breakdown ==========================

nv = 5   &   qir = REPLICATE( var, nv )
qir(*).color = mycolors(0:nv-1)   &   qir(*).linest = mylinest(0:nv-1)
qir(*).name  = [ 'qcool', 'IRcool_CO2', 'IRcool_O3', 'IRcool_NO', 'IRcool_H2O' ]
calc_terms, qir, mapl

;======================= IR Chemical species ==============================

nv = 5   &   nir = REPLICATE( var, nv)
nir(*).color = mycolors(0:nv-1)   &   nir(*).linest = mylinest(0:nv-1)
nir(*).name  = [ 'o3p', 'co2', 'o3', 'no', 'h2o' ]
calc_terms, nir, mapl

;======================= Solar heating breakdown ==========================

nv = 3   &   qs = REPLICATE( var, nv )
qs(*).color = mycolors(0:nv-1)   &   qs(*).linest = mylinest(0:nv-1)
qs(*).name  = [ 'SolHeat', 'UVSol_O3', 'UVSol_O2' ]
calc_terms, qs, mapl

;=========================== Diffusion breakdown ==========================

nv = 4   &   diff = REPLICATE( var, 4 )
diff(*).color = mycolors(0:nv-1)   &   diff(*).linest = mylinest(0:nv-1)
diff(*).name  = [ 'w', 'kzz', 'therm_diff', 'tide_diff' ]
calc_terms, diff, mapl

If diff(0).units eq 'm/s' then Begin
   diff(0).name = 'w*7e3'   &   diff(0).vals = 7.e3*diff(0).vals
   diff(0).units = 'm2/s'
EndIf

;=============================== Plot everything ==========================

mytitle0 = arch.attr(0,1)+' '+arch.attr(1,1)+' ; '+arch.list.depth(zl.did)  $
         +' ; '+x.name
For l = 0, nl-1 Do Begin
   plot_budget, t, l
   plot_budget, nic, l, /XLOG
   plot_budget, nir, l, /XLOG
   plot_budget, diff, l
   plot_budget, q, l
   plot_budget, qc, l
   plot_budget, qir, l
   plot_budget, qs, l
   mytitle = mytitle0 +' '+ STRING(q(0).lats(l),FORMAT=Xfmt) +' '+ x.units
   xyouts, 0.5, 1., mytitle, /NORMAL, ALIGN=0.5, CHARSIZE=1.5
;   plots, [0,1,1,0,0], [0,0,1,1,0], color=35, /normal
EndFor

device, /close   &   !P.MULTI = [0,0,0,0,0] 
set_plot, screen   &   !p.font = -1
; spawn, 'mgv '+fname+' &'  ; 'xv '+fname+' &' ; 

END

