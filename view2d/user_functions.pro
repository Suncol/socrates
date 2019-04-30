FUNCTION calc_n2, bg=bg
; Try to read N2. If not existent, set it as 
; constant profile from imprecise MSIS results (see module BACKTAM)

common axes           ; in : x.dim, y.dim
ok = 0

n2 = ncdf_valsget( 'n2' , bg=bg, ok=readok )
if readok then return, n2

n2 = FLTARR(x.dim,y.dim)
n2(*,0:84) = 0.78   &   n2(*,85:98) = 0.79   &   n2(*,99:107) = 0.78
n2(*,108:111) = 0.77   &   n2(*,112:116) = 0.76   &   n2(*,117) = 0.75
n2(*,118:119) = 0.74   &   n2(*,120) = 0.73

return, n2
END

;==================================================================

FUNCTION set_no_cira96, bg=bg, ref=ref, ok=ok
; From CIRA96, i.e. Barth-1996, Fig. 2, solmin, equinox - uses GEO alt!

common axes          ;  in: x.dim, y.dim
common arch_info     ;  out: arch.ok.missval; in: arch.missval

ok = 0   &   no = FLTARR(x.dim,y.dim)
arch.ok.missval = 1B   &   no(*,*) = arch.missval

;                 -85 -80 -75 -70 -65 -60 -55 -50 -45 -40 -35 -30 -25 -20 -15 -10  -5   0   5  10  15  20  25  30  35  40  45  50  55  60  65  70  75  80  85
no(*,120) = 1.e6*[ 25, 21, 23, 26, 26, 24, 23, 21, 18, 15, 13, 12, 11, 10, 10, 10,  9,  9,  9,  9,  9,  8,  8,  8,  8,  9, 10, 10, 12, 16, 19, 22, 26, 28, 26 ]
no(*,117) = 1.e6*[ 29, 25, 28, 31, 31, 28, 26, 23, 19, 17, 14, 13, 12, 11, 11, 11, 10,  9, 10,  9,  9,  9,  9,  9, 10, 11, 11, 12, 14, 18, 22, 27, 32, 33, 30 ]
no(*,113) = 1.e6*[ 33, 30, 31, 35, 37, 34, 29, 24, 22, 18, 15, 14, 13, 12, 11, 11, 10,  9, 10,  9, 10, 10, 10, 10, 11, 12, 13, 14, 16, 21, 27, 32, 37, 38, 33 ]
no(*,110) = 1.e6*[ 38, 34, 34, 40, 43, 39, 33, 26, 22, 17, 14, 16, 12, 11, 11, 10,  9,  9, 10,  9,  9,  9, 10, 10, 11, 12, 13, 15, 19, 24, 31, 37, 43, 42, 35 ]
no(*,107) = 1.e6*[ 45, 37, 36, 42, 43, 42, 34, 25, 19, 14, 12, 11, 10,  9,  9,  8,  8,  8,  8,  7,  8,  8,  9,  9, 10, 11, 13, 15, 19, 25, 31, 39, 47, 47, 36 ]
no(*,103) = 1.e6*[ 44, 36, 34, 37, 37, 39, 33, 22, 15, 11,  9,  8,  8,  8,  7,  7,  7,  7,  7,  6,  7,  7,  7,  7,  8,  9, 11, 13, 16, 22, 28, 36, 44, 45, 31 ]
no(*,100) = 1.e6*[ 35, 28, 26, 28, 29, 33, 30, 19, 12,  8,  7,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  5,  6,  7,  7,  8, 10, 13, 18, 23, 30, 36, 35, 22 ]

ok = 1   &   return, no
END

;==================================================================

FUNCTION calc_wmole, bg=bg, ref=ref, ok=ok
; Calculation of air molar mass (g/mole)

common axes          ;  in: x.dim, y.dim

ok = 0

n2 = calc_n2( bg=bg )
o2 = ncdf_valsget( 'o2' , bg=bg, ref=ref, ok=readok )

If not readok then Begin
   o2 = FLTARR(x.dim,y.dim)
   o2(*,0:84) = 0.21   &   o2(*,85:94) = 0.20   &   o2(*,95:98) = 0.19
   o2(*,99:102) = 0.18   &   o2(*,103:104) = 0.17   &   o2(*,105:107) = 0.16
   o2(*,108:109) = 0.15   &   o2(*,110:111) = 0.14   &   o2(*,112:114) = 0.12
   o2(*,116:116) = 0.11   &   o2(*,117) = 0.10   &   o2(*,118:119) = 0.09
   o2(*,120) = 0.08
EndIf

o  = ncdf_valsget( 'o3p', bg=bg, ref=ref, ok=readok )
if not readok then o = ncdf_valsget( 'o3p-noon', bg=bg, ref=ref, ok=readok )
if not readok then o = 1. - n2 - o2
   
wmole = 28.*n2 + 32.*o2 + 16.*o

ok = 1   &   return, wmole
END

;==================================================================

FUNCTION calc_ygeo, bg=bg, ref=ref, ok=ok
; calculate geometric altitude from y.coord, log-p altitude of 7km scale height

common axes		; in: x,y

ok = 0   &   ygeovals = FLTARR(x.dim,y.dim)

t2d = ncdf_valsget( 'temperature', bg=bg, ref=ref, ok=readok )
if not readok then return, ygeovals
wmole = calc_wmole( bg=bg, ref=ref, ok=fctok )
if not fctok then return, ygeovals

Hair = 8.314 * t2d / (wmole*9.806)		; km

For iz = 1, y.dim-1 Do ygeovals(*,iz) = ygeovals(*,iz-1) +   $    ; trapeze integration
      (Hair(*,iz)+Hair(*,iz-1)) * .5 * (y.coord(iz)-y.coord(iz-1)) / 7.  

ok = 1   &   return, ygeovals
END

;==================================================================

FUNCTION calc_ro, ok=ok, bg=bg
; Calculation of air mass density (g/m3)

ok = 0

wmole = calc_wmole( ok=fctok, bg=bg )
if not fctok then return, 'FAILED'
hm2d = ncdf_valsget( 'totdens', ok=readok, bg=bg )
if not readok then return, 'FAILED'
Nav = 6.022142e23
ro = 1.e6 * hm2d * wmole / Nav             ; g/m3

ok = 1   &   return, ro
END

;==================================================================

FUNCTION calc_tdiff_new, ok=ok, bg=bg
; Calculation of Thermal diffusivity as in force1.f beginning v4s002

common axes           ; in : x.dim, y.dim
ok = 0

Cp = FLTARR(y.dim)
Cp(0:74)=1.007   &   Cp(75:87)=1.008   &   Cp(88:89)=1.009   &   Cp(90:92)=1.01
Cp(93:120) = [ 1.011, 1.012, 1.013, 1.013, 1.014, 1.016, 1.017, 1.018, 1.019, $
               1.021, 1.022, 1.024, 1.026, 1.028, 1.029, 1.032,               $
               1.034, 1.036, 1.036, 1.038, 1.041, 1.044, 1.047,               $
               1.05, 1.053, 1.056, 1.06, 1.064 ]
               
ro = calc_ro( ok=fctok, bg=bg )         &   if not fctok then return, 'FAILED'
n2 = calc_n2( bg=bg )
o2 = ncdf_valsget( 'o2', bg=bg, ok=readok )
if not readok then return, 'FAILED'
o  = ncdf_valsget( 'o3p', bg=bg, ok=readok )
if not readok then return, 'FAILED'
t  = ncdf_valsget( 'temperature', bg=bg, ok=readok ) 
if not readok then return, 'FAILED'

xtz = 1.e-5*((n2 + o2)*56. + o*75.9) * (t^.69)  ; thermal conductivity, J/m/K/s
For l = 0, x.dim-1 Do xtz(l,*) = xtz(l,*) / ( ro(l,*) * Cp(*) )       ; m2/s

ok = 1   &   return, xtz
END

;==================================================================

FUNCTION calc_mvisc_new, ok=ok, bg=bg
; kinematic coeff of molecular viscosity (m2/s) from B/K:1973, vol.2 p.10

ok = 0

n2 = calc_n2( bg=bg )
o2 = ncdf_valsget( 'o2' , ok=readok, bg=bg )
if not readok then return, 'FAILED'
o  = ncdf_valsget( 'o3p', ok=readok, bg=bg )
if not readok then return, 'FAILED'
t  = ncdf_valsget( 'temperature', ok=readok, bg=bg ) 
if not readok then return, 'FAILED'

vmd = 1.e-4 * ( 4.03*o2 + 3.43*n2 + 3.9*o ) * t^0.69    ; dynamic coeff (g/m/s)
ro = calc_ro( ok=fctok, bg=bg )   &   if not fctok then return, 'FAILED'
vmd = vmd / ro                                          ; kinematic coeff (m2/s)

ok = 1   &   return, vmd
END

;==================================================================

FUNCTION calc_q_adiab, ok=ok, bg=bg
; Adiabatic heat (K/day) from vert mvt of air, see SOCRATES notebook 9 dec 1999

ok = 0

kappa = 0.285
w = ncdf_valsget( 'w' , ok=readok, bg=bg )
if not readok then return, 'FAILED'
w = 1.e-3 * w * 86400.                           ; from m/s to km/day
t  = ncdf_valsget( 'temperature', ok=readok, bg=bg ) 
if not readok then return, 'FAILED'

q_adiab = - w * kappa * t / 7.              ; K/day

ok = 1   &   return, q_adiab
END

;==================================================================

FUNCTION calc_qteta, ok=ok, bg=bg
; Total heating rate expressed in potential temperature system

common axes           ; in : x, y, yp

ok = 0

q = ncdf_valsget( 'q' , ok=readok, bg=bg )
if not readok then return, 'FAILED'
q_teta = FLTARR( x.dim, y.dim )
kappa = 0.285   &   t2pt = ( yp.coord(0) / yp.coord(*) ) ^ kappa
For l = 0, x.dim-1 Do q_teta(l,*) = q(l,*) * t2pt(*)

ok = 1   &   return, q_teta
END

;==================================================================

FUNCTION calc_dudz, ok=ok, bg=bg
; u(z+1)-u(z-1), see SOCRATES notebook 30 jan 2000

common axes           ; in : x, y, yp

ok = 0
u = ncdf_valsget( 'u' , ok=readok, bg=bg )
if not readok then return, 'FAILED'
dudz = FLTARR( x.dim, y.dim )
For l = 0, x.dim-1 Do Begin
   dudz(l,0) = ( u(l,1) - u(l,0) ) / 2.e3
   dudz(l,1:y.dim-2) = ( u(l,2:y.dim-1) - u(l,0:y.dim-3) ) / 2.e3
   dudz(l,y.dim-1) = ( u(l,y.dim-1) - u(l,y.dim-2) ) / 2.e3
EndFor
ok = 1   &   return, dudz
END

;==================================================================

FUNCTION calc_au1, ok=ok, bg=bg
; A * u * (u(z+1) - u(z-1)), see SOCRATES notebook 30 jan 2000

common axes           ; in : x, y, yp

ok = 0
u = ncdf_valsget( 'u' , ok=readok, bg=bg )
if not readok then return, 'FAILED'
ff = 1.4584e-4 * SIN( x.coord(*)*!DTOR )
a = 2. * TAN(x.coord(*)*!DTOR) / ( ff(*) * 6.3567e6 )
For l=0,x.dim-1 Do print, x.coord(l),' ; ulim= ',-1./a(l)
a(18-1) = 0.      ; avoid 0/0 at equator
au1 = FLTARR( x.dim, y.dim )
For l = 0, x.dim-1 Do au1(l,*) = 1. + a(l) * u(l,*)
ok = 1   &   return, au1
END

;==================================================================

FUNCTION calc_dco2rs, ok=ok, bg=bg

common axes             ; in : x.dim, y.dim
common arch_info        ; in: arch, arch2
common last_fields      ; in : zl.did, z2.did
ok = 0B

t = ncdf_valsget( 'temperature' , bg=bg, ok=readok )
if not readok then return, 'FAILED'

if KEYWORD_SET(bg) then arch0=arch2 else arch0=arch
if not arch0.ok.totdens then return, 'FAILED'
if KEYWORD_SET(bg) then did=z2.did else did=zl.did
ntot = arch0.totdens(0:x.dim-1,0:y.dim-1,did)

wm = calc_wmole( bg=bg, ok=calcok)
if not calcok then return, 'calc_dco2rs: FAILED getting wmole'

dco2rs = 1.e-4 * 1.52e18 * SQRT( (1./44. + 1./wm)*t ) / ntot   ; 1.e-4 to get from cm2/s to m2/s
ok = 1B   &   return, dco2rs
END

;==================================================================
FUNCTION calc_dco2, ok=ok, bg=bg

common axes             ; in : x.dim, y.dim
common arch_info        ; in: arch, arch2
common last_fields      ; in : zl.did, z2.did
ok = 0

t = ncdf_valsget( 'temperature' , bg=bg, ok=readok )
if not readok then return, 'FAILED'

if KEYWORD_SET(bg) then arch0=arch2 else arch0=arch
if not arch0.ok.totdens then return, 'FAILED'
if KEYWORD_SET(bg) then did=z2.did else did=zl.did
ntot = arch0.totdens(0:x.dim-1,0:y.dim-1,did)

dco2 = 1.e-4 * 6.42e16 * t^0.735 / ntot   ; 1.e-4 to get from cm2/s to m2/s
ok = 1B   &   return, dco2
END

;==================================================================
PRO user_functions

common arch_info      ; in: arch.missval
common axes           ; in: x.dim, y.dim
common last_com       ; out: last.plot2d
common last_fields    ; inout : zl ;out : z2

n_fcts = 12   &   fct = STRARR(n_fcts,4)

fct(0,*) = [ 'zgeo',   'geometric altitude',  'km',     'calc_ygeo'      ]
fct(1,*) = [ 'wmole',  'air molar mass',      'g/mole', 'calc_wmole'     ]
fct(2,*) = [ 'ro',     'air mass density',    'g/m3',   'calc_ro'        ]
fct(3,*) = [ 'xtz',    'thermal diffusivity', 'm2/s',   'calc_tdiff_new' ]
fct(4,*) = [ 'mvisc',  'molecular viscosity', 'm2/s',   'calc_mvisc_new' ]
fct(5,*) = [ '-wkT/7', 'adiabatic heating',   'K/day',  'calc_q_adiab'   ]
fct(6,*) = [ 'qteta',  'q *(ps/p)**.285',     'K/day',  'calc_qteta'     ]
fct(7,*) = [ '1+a*u',  '2d factor of NWIND',  'm/s',    'calc_au1'       ]
fct(8,*) = [ 'du/dz',  '1st factor of NWIND', 'm/s',    'calc_dudz'      ]
fct(9,*) = [ 'no',     'CIRA96-solmin-equinox','molec/cm3',   'set_no_cira96'  ]
fct(10,*)= [ 'D(CO2)', 'CO2 moldiff coeff',    'm2 s-1', 'calc_dco2'  ]
fct(11,*)= [ 'D(CO2)rs', 'CO2 moldiff coeff by rigid spheres',    'm2 s-1', 'calc_dco2rs'  ]

fct_list = fct(0,0)+' ('+fct(0,2)+')'+' - the '+fct(0,1)
For i = 1, n_fcts-1 Do $
   fct_list = fct_list + '|' + fct(i,0)+' ('+fct(i,2)+')'+' - the '+fct(i,1)

desc = strarr(7)
desc(0) = '1, BASE,, COLUMN, FRAME'
desc(1) = '2, BUTTON, '+fct_list+', EXCLUSIVE, TAG=l'
desc(2) = '1, BASE,, COLUMN, FRAME'
desc(3) = '2, BUTTON, Just look|Choose another fct to compare, EXCLUSIVE, TAG=cmp'
desc(4) = '1, BASE,, ROW, CENTER'
desc(5) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(6) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'

form = CW_FORM(desc, /COLUMN, title='Pick user function')
if form.cancel then return

fct_vals = CALL_FUNCTION( fct(form.l,3), ok=ok, bg=0 )

If not ok then Begin
   msg = [ 'Execution of function '+fct(form.l,3)+' failed',          $
           'Please correct it in file user_functions.pro' ]
   error = DIALOG_MESSAGE( msg, /ERROR )
   return
EndIf

zl.matrix(0:x.dim-1,0:y.dim-1) = fct_vals
zl.name = fct(form.l,0)   &   zl.units = fct(form.l,2)
zl.valOK(*,*) = zl.matrix ne arch.missval
zl.badvals = TOTAL( zl.valOK ) lt x.dim*y.dim

If form.cmp then Begin
   desc(3) = '0, LABEL, This fct replaces the field from Background file'
   form2 = CW_FORM(desc, /COLUMN, title='Pick 2d user function')
   if form2.cancel then return
   fct_vals = CALL_FUNCTION( fct(form2.l,3), ok=ok, bg=0 )
   If not ok then Begin
      msg = [ 'Execution of function '+fct(form.l,3)+' failed',          $
              'Please correct it in file user_functions.pro',                  $
              'No second function in memory' ]
      error = DIALOG_MESSAGE( msg, /ERROR )
   EndIf Else Begin
      z2 = zl   &   z2.matrix = fct_vals
      z2.name = fct(form2.l,0)   &   z2.units = fct(form2.l,2)
      z2.compOK = 1B
   EndElse
 EndIf; Else If z2.compOK then Begin
;   fct_vals = CALL_FUNCTION( fct(form.l,3), ok=ok, /bg )
;   If not ok then Begin
;      msg = [ 'Execution of function '+fct(form.l,3)+' on the background', $              
;              'file failed. Showing only foreground file results.' ]
;      error = DIALOG_MESSAGE( msg, /ERROR )
;      z2.compOK = 0B
;   EndIf Else Begin
;      z2.matrix = fct_vals
;      z2.name = fct(form.l,0)   &   z2.units = fct(form.l,2)
;   EndElse
;EndIf


reset_titles = 1   &   last.plot2d = 'x-y'   &   viewpreproc

END
