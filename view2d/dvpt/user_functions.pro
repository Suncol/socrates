FUNCTION calc_n2, ok=ok
; Setting N2 constant proile from imprecise MSIS results (see module BACKTAM)

common axes           ; in : x.dim, y.dim
ok = 0

n2 = FLTARR(x.dim,y.dim)
n2(*,0:84) = 0.78   &   n2(*,85:98) = 0.79   &   n2(*,99:107) = 0.78
n2(*,108:111) = 0.77   &   n2(*,112:116) = 0.76   &   n2(*,117) = 0.75
n2(*,118:119) = 0.74   &   n2(*,120) = 0.73

ok = 1   &   return, n2
END

;==================================================================

FUNCTION calc_wmole, bg_arch=bg_arch, ok=ok
; Calculation of air molar mass (g/mole)

ok = 0

n2 = ncdf_valsget( 'n2' , bg_arch=bg_arch, ok=readok )
if not readok then return, 'FAILED'
o2 = ncdf_valsget( 'o2' , bg_arch=bg_arch, ok=readok )
if not readok then return, 'FAILED'
o  = ncdf_valsget( 'o3p', bg_arch=bg_arch, ok=readok )
if not readok then return, 'FAILED'

wmole = 28.*n2 + 32.*o2 + 16.*o

ok = 1   &   return, wmole
END

;==================================================================

FUNCTION calc_ygeo, bg_arch=bg_arch, ok=ok
; calculate geometric altitude from y.coord, log-p altitude of 7km scale height

common axes		; in: x,y

ok = 0   &   ygeovals = FLTARR(x.dim,y.dim)

t2d = ncdf_valsget( 'temperature', bg_arch=bg_arch, ok=readok )
if not readok then return, ygeovals
wmole = calc_wmole( bg_arch=bg_arch, ok=fctok )
if not fctok then return, ygeovals

Hair = 8.314 * t2d / (wmole*9.806)		; km

For iz = 1, y.dim-1 Do ygeovals(*,iz) = ygeovals(*,iz-1) +   $    ; trapeze integration
      (Hair(*,iz)+Hair(*,iz-1)) * .5 * (y.coord(iz)-y.coord(iz-1)) / 7.  

ok = 1   &   return, ygeovals
END

;==================================================================

FUNCTION calc_ro, ok=ok
; Calculation of air mass density (g/m3)

ok = 0

wmole = calc_wmole( ok=fctok )                &   if not fctok then return, 'FAILED'
hm2d = ncdf_valsget( 'totdens', ok=readok )   &   if not readok then return, 'FAILED'
Nav = 6.022142e23
ro = 1.e6 * hm2d * wmole / Nav             ; g/m3

ok = 1   &   return, ro
END

;==================================================================

FUNCTION calc_tdiff_old, ok=ok
; Calculation of Thermal diffusivity as in force1.f up to v3s75

common axes           ; in : x.dim, y.dim
ok = 0

fix_n2 = calc_n2( ok=fctok )   &   if not fctok then return, 'FAILED'

fix_o2 = FLTARR(y.dim)
fix_o2(0:84) = 0.21   &   fix_o2(85:94) = 0.20   &   fix_o2(95:98) = 0.19
fix_o2(99:102) = 0.18   &   fix_o2(103:104) = 0.17
fix_o2(105:107) = 0.16   &   fix_o2(108:109) = 0.15
fix_o2(110:111) = 0.14   &   fix_o2(112:114) = 0.12
fix_o2(115:116) = 0.11   &   fix_o2(117) = 0.1   &   fix_o2(118:119) = 0.09
fix_o2(120) = 0.08

fix_o = 1. - n2(0,*) - fix_o2(*)
wm = 1.e-3 * (28.*fix_n2(0,*) + 32.*fix_o2(*) + 16.*fix_o(*))
po = 1.013e05   &   cp = 1005.   &   r = 8.314   &   g = 9.80665   &   h = 7.e3
const = wm(*)*(g*h)^2 / (po*cp*EXP(-FINDGEN(121)/7.)*r)

o2 = ncdf_valsget( 'o2' , ok=readok )         &   if not readok then return, 'FAILED'
o  = ncdf_valsget( 'o3p', ok=readok )         &   if not readok then return, 'FAILED'
t  = ncdf_valsget( 'temperature', ok=readok ) &   if not readok then return, 'FAILED'

xtz = FLTARR( x.dim, y.dim )
For l = 0, x.dim-1 Do Begin
   xtz(l,*) = ((fix_n2(l,*) + o2(l,*))*56. + o(l,*)*75.9) * (t(l,*)^.69) * const(*)  $
            / t(l,*) * 1.e-5
EndFor

ok = 1   &   return, xtz
END

;==================================================================

FUNCTION calc_tdiff_new, ok=ok
; Calculation of Thermal diffusivity as in force1.f beginning v4s002

common axes           ; in : x.dim, y.dim
ok = 0

Cp = FLTARR(y.dim)
Cp(0:74)=1.007   &   Cp(75:87)=1.008   &   Cp(88:89)=1.009   &   Cp(90:92)=1.01
Cp(93:120) = [ 1.011, 1.012, 1.013, 1.013, 1.014, 1.016, 1.017, 1.018, 1.019, $
               1.021, 1.022, 1.024, 1.026, 1.028, 1.029, 1.032,               $
               1.034, 1.036, 1.036, 1.038, 1.041, 1.044, 1.047,               $
               1.05, 1.053, 1.056, 1.06, 1.064 ]
               
ro = calc_ro( ok=fctok )                &   if not fctok then return, 'FAILED'
n2 = ncdf_valsget( 'n2' , ok=readok )   &   if not readok then return, 'FAILED'
o2 = ncdf_valsget( 'o2' , ok=readok )   &   if not readok then return, 'FAILED'
o  = ncdf_valsget( 'o3p', ok=readok )   &   if not readok then return, 'FAILED'
t  = ncdf_valsget( 'temperature', ok=readok ) 
if not readok then return, 'FAILED'

xtz = 1.e-5*((n2 + o2)*56. + o*75.9) * (t^.69)  ; thermal conductivity, J/m/K/s
For l = 0, x.dim-1 Do xtz(l,*) = xtz(l,*) / ( ro(l,*) * Cp(*) )       ; m2/s

ok = 1   &   return, xtz
END

;==================================================================

FUNCTION calc_mvisc_old, ok=ok
; Molecular viscosity (m2/s) (cf Matsuno:1982), see vmd in force1.f up to v4s003

common axes           ; in : x.dim, y.dim, y.coord
ok = 0

vmd = FLTARR(x.dim,y.dim)
For l = 0, x.dim-1 Do vmd(l,*) =  400. * EXP( (y.coord(*)-110.)/7. )    ; m2/s

ok = 1   &   return, vmd
END

;==================================================================

FUNCTION calc_mvisc_new, ok=ok
; kinematic coeff of molecular viscosity (m2/s) from B/K:1973, vol.2 p.10

ok = 0

n2 = ncdf_valsget( 'n2' , ok=readok )   &   if not readok then return, 'FAILED'
o2 = ncdf_valsget( 'o2' , ok=readok )   &   if not readok then return, 'FAILED'
o  = ncdf_valsget( 'o3p', ok=readok )   &   if not readok then return, 'FAILED'
t  = ncdf_valsget( 'temperature', ok=readok ) 
if not readok then return, 'FAILED'

vmd = 1.e-4 * ( 4.03*o2 + 3.43*n2 + 3.9*o ) * t^0.69    ; dynamic coeff (g/m/s)
ro = calc_ro( ok=fctok )                &   if not fctok then return, 'FAILED'
vmd = vmd / ro                                          ; kinematic coeff (m2/s)

ok = 1   &   return, vmd
END

;==================================================================

PRO user_functions

common last_com       ; out: last.plot2d
common last_fields    ; out : zl
common last_fields2   ; out : z2

n_fcts = 8   &   fct = STRARR(n_fcts,3)

fct(0,*) = [ 'geometric altitude',            'km',     'calc_ygeo' ]
fct(1,*) = [ 'latitude-independent N2',       'vmr',    'calc_n2' ]
fct(2,*) = [ 'air molar mass',                'g/mole', 'calc_wmole' ]
fct(3,*) = [ 'air mass density',              'g/m3',   'calc_ro' ]
fct(4,*) = [ 'Thermal diffusivity: old calc', 'm2/s',   'calc_tdiff_old' ]
fct(5,*) = [ 'Thermal diffusivity: new calc', 'm2/s',   'calc_tdiff_new' ]
fct(6,*) = [ 'Molecular viscosity: old calc', 'm2/s',   'calc_mvisc_old' ]
fct(7,*) = [ 'Molecular viscosity: new calc', 'm2/s',   'calc_mvisc_new' ]

fct_list = fct(0,0)+' ('+fct(0,1)+')'
For i = 1, n_fcts-1 Do fct_list = fct_list + '|' + fct(i,0)+' ('+fct(i,1)+')'

desc = strarr(5)
desc(0) = '0, LIST, '+fct_list+', TAG=l'
desc(1) = '0, BUTTON, Just look|Choose another fct to compare, EXCLUSIVE, TAG=cmp'
desc(2) = '1, BASE,, ROW, CENTER'
desc(3) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(4) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'

form = CW_FORM(desc, /COLUMN, title='Pick user function')
if form.cancel then return

fct_vals = CALL_FUNCTION( fct(form.l,2), ok=ok )

If not ok then Begin
   msg = [ 'Execution of function '+fct(form.l,2)+' failed',          $
           'Please correct it in file user_functions.pro' ]
   error = DIALOG_MESSAGE( msg, /ERROR )
   return
EndIf

zl.matrix = fct_vals
zl.name = fct(form.l,0)   &   zl.units = fct(form.l,1)
field_to_compOK = 0

If form.cmp then Begin
   desc(1) = '0, LABEL, This fct replaces the field from Background file'
   form2 = CW_FORM(desc, /COLUMN, title='Pick 2d user function')
   if form2.cancel then return
   fct_vals = CALL_FUNCTION( fct(form2.l,2), ok=ok )
   If not ok then Begin
      msg = [ 'Execution of function '+fct(form.l,2)+' failed',          $
              'Please correct it in file user_functions.pro',                  $
              'No second function in memory' ]
      error = DIALOG_MESSAGE( msg, /ERROR )
   EndIf Else Begin
      z2 = zl   &   z2.matrix = fct_vals
      z2.name = fct(form2.l,0)   &   z2.units = fct(form2.l,1)
      field_to_compOK = 1
   EndElse
EndIf

reset_titles = 1   &   last.plot2d = 'x-y'   &   viewpreproc

END
