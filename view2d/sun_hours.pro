pro sun_hours, sr, ss, plots=plots

common axes		; in : x, depth.coord
common select_id	; in : depth_id, ip
common plots_info	; in : logsw
common arch_info	; in : arch.attr

pi = 3.141592654
convrd = pi / 180

If (x.name eq 'latitudes') then 				$
   lat = x.coord(ip)						$
 Else If (x.name eq 'solar_time') then 				$
   lat = FLOAT(STRMID(arch.attr(0), STRLEN(arch.attr(0))-4, 4))	$
 Else Begin
  msg = 'routine SUN_HOURS : can not set latitude. Failed to set sunrise/sunset times'
  return
EndElse

daynum = ((depth.coord(depth_id) - 1) MOD 365) + 1 - 172   ; Julian day - 172
declind = 23.45 * COS( 2.*pi/365.*daynum )
declin = declind * convrd   &   phi = lat * convrd
if( lat lt 0. ) then hautmx = 90. - declind + lat  $
 else if( lat ge 0. ) then hautmx = 90. + declind - lat

If (hautmx le 0.)  then Begin
   sr = 999.   &   ss = -999.
   return
 EndIf Else Begin
   latpol = 90. - ABS(declind)
   If (ABS(lat) ge latpol)  then Begin
      sr = -999.   &   ss = 999.
      return
    EndIf Else Begin

;--------------------------------------------------------------------
;        a : sunrise/sunset zenith angle (90 at ground, 102.44deg at 120km)
;        h0 : corresponding solar hour angle
;--------------------------------------------------------------------
	 alt = 0.
         a = ( 90. + 1.76459 * alt^0.40795 ) * convrd    ; alt : 120km max !
         cosh0 = (COS(a)-SIN(phi)*SIN(declin)) / (COS(phi)*COS(declin))
         h0 = ACOS(cosh0) / convrd
         sr = 12. - h0/15.    &     ss = 12. + h0/15.
   EndElse
EndElse

If KEYWORD_SET(plots) then Begin
   If (sr ge 0.) and (sr le 24.) then Begin
      If (logsw) then Begin
         plots, [sr,sr], [10^!y.crange(0),10^!y.crange(1)], linestyle=1, /data
         plots, [ss,ss], [10^!y.crange(0),10^!y.crange(1)], linestyle=1, /data
       EndIf Else Begin
         plots, [sr,sr], [!y.crange(0),!y.crange(1)], linestyle=1, /data
         plots, [ss,ss], [!y.crange(0),!y.crange(1)], linestyle=1, /data
      EndElse
   EndIf
EndIf
return
end

