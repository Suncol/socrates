pro calc_flxub, zmonthlat

common select_id	; out : jp
common axes		; in : depth, x, y

zmonthlat = { name:'upper boundary O(3P) flux', units:'molec/cm2/s', loc:strarr(3), $
                                                      matrix:fltarr(depth.dim,x.dim) }

jp = y.dim -1

WIDGET_CONTROL, /HOURGLASS

For date_id = 0, depth.dim-1 Do Begin
      daynum = depth.coord(date_id) + 193. MOD 365.
      For ilat = 0, x.dim-1 Do Begin
          secjou, daynum, x.coord(ilat), h2pi
          dun = -1. + 2.*h2pi
          dud = -2.*h2pi
          zmonthlat.matrix(date_id,ilat) = dun*1.5e8 + dud*2.5e11
      EndFor
EndFor
 
return
end
