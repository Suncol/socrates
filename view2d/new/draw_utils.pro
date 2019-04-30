PRO unzoom                ; OOP: could become a method for class of "pos"

common i_com             ; in : narch ; out : pos

For i = 0, narch-1 Do Begin
   pos(i).ixr = [ 0, arch(i).x.dim-1 ]
   pos(i).iyr = [ 0, arch(i).y.dim-1 ]
   pos(i).idr = [ 0, arch(i).d.dim-1 ]
   if arch(i).ok.d_as_xplot then time_labels
EndFor

redo_plot

END

;======================================================================

PRO draw_utils
print, 'Draw_utils routines compiled'
END