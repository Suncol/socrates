pro make_colors, ncolors, mycolors    ; in: ncolors, out: mycolors

loadct, 39  ; "rainbow+white" color table

ncols_avail = (!D.N_COLORS < 256 )

mycolors = INDGEN( ncolors+1 )
mycolors(0) = 2
mycolors(1:ncolors) = 8 + indgen(ncolors) * (ncols_avail+100) / ncolors

end

;=====================================

pro test_colors, ncolors         ; in : ncolors

If ncolors gt 74 then Begin
   print, 'TEST_COLORS: error, can not display more than 74+1 colors'
   print, '     Input arg ncolors must be <= 74'
   return
EndIf

make_colors, ncolors, mycolors

window, 1, xsize=1000, ysize=700

For i = 0, ( ncolors < 37 ) do Begin
   ypos = .96-i/40.
   xyouts,.01,ypos-5.e-3, 'mycolors('+strtrim(i,2)+')', charsize=1.5, /normal 
   plots,[.12,.4],[ypos,ypos],thick=12,color=mycolors(i), /normal
   xyouts,.41,ypos-5.e-3, strtrim(mycolors(i),2), charsize=1.5, /normal
EndFor 

If ncolors gt 36 then Begin
   plots,[.5,.5],[0.,1.], /normal
   For i = 37, ncolors do Begin
      ypos = .96 - (i-37) / 40.
      xyouts,.51,ypos-5.e-3, 'mycolors('+strtrim(i,2)+')', charsize=1.5, /normal 
      plots,[.62,.9],[ypos,ypos],thick=12,color=mycolors(i), /normal
      xyouts,.91,ypos-5.e-3, strtrim(mycolors(i),2), charsize=1.5, /normal
   EndFor
EndIf

end

