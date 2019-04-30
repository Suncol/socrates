;+
;  function pseudo8_to_true24
;  This function converts an 8 bits image in a 24 bits image.
;  Important for colors mpeg.  Written by Dave Fanning
;+

function pseudo8_to_true24, image8
   s = SIZE(image8)
   width = s(1)
   height = s(2)
   TVLCT, red, green, blue, /GET
   image24 = BYTARR(3,width, height)
   image24(0,*,*) = red(image8(*,*))
   image24(1,*,*) = green(image8(*,*))
   image24(2,*,*) = blue(image8(*,*))
   RETURN, image24
end

;==============================================================================

pro make_mpeg, savesw=savesw
;+
;  pro make_mpeg
;  The make_mpeg make you a mpeg file of the contour plot.
;  A widget ask you the starting time, ending time and the time step as well as
;  the file name where you will save the mpeg.
;  Author: Q. Errera 	18 sept 1998 
;  Adapted to view2d by S.C. 22 sept 1998 - set /savesw to really save a mpeg
;+

common arch_info        ; in : arch
common axes             ; in : depth
common dataranges	; in : idp0, idp1
common plots_info       ; in : draw_id
common last_com         ; out : last
common last_fields      ; out : zl
common formats          ; in : Dfmt
common comscale		; in : scale
common plots_info	; in : ncols_avail, show.pdn

if KEYWORD_SET(savesw) then savesw = 1 else savesw = 0
If savesw then Begin
   mpeg_file = DIALOG_PICKFILE( filter='*.mpg',                             $
       get_path=mpeg_path, title='Provide name for mpeg file to write' )
   junk = FINDFILE( mpeg_file, count=c )
   If (c ne 0) then Begin
      junk = dialog_message( 'this file already exists. ok to overwrite ?', $
                          /question )
      if (junk eq 'no') then return
   EndIf
EndIf

WIDGET_CONTROL, /HOURGLASS
archread, zl.name, idp0, zl
mpeg_size = [ 600, 480 ]   &   i_frame = 0
WINDOW, 7, xsize=mpeg_size(0), ysize=mpeg_size(1), title=zl.name
mycolors = indgen(30) * (ncols_avail+100) / 30 + 10
days0date, depth.coord(idp0), date   &   days0date, depth.coord(idp1), date1
datestrg = 'from  '+ string(date.month,date.day,date.year,FORMAT=Dfmt)       $
         + '  to  '+ string(date1.month,date1.day,date1.year,FORMAT=Dfmt)

If savesw then Begin                   ; Prepare title frame
   mpeg_id = MPEG_OPEN( mpeg_size )
   thistring = up_subscript( zl )
   xyouts, .35, .80, /normal, CHARSIZE=3, thistring(0) + ' (' + thistring(1) + ')'
   xyouts, .30, .70, /normal, CHARSIZE=1.5, datestrg
   xyouts, .45, .60, /normal, CHARSIZE=1.5, 'by model '
   xyouts, .40, .50, /normal, CHARSIZE=3, arch.attr(0,1)
   xyouts, .45, .45, /normal, CHARSIZE=1.5, 'version '+arch.attr(1,1)
   xyouts, .30, .40, /normal, CHARSIZE=1.5, 'output from file ' + extract_flnm(arch.flnm)
   xyouts, .30, .35, /normal, CHARSIZE=1.5, 'Animation created by view2d'
   xyouts, .20, .30, /normal, CHARSIZE=1.5, 'an IDL application written by S. Chabrillat (IASB-BIRA)'
   If arch.attr(0,1) eq 'SOCRATES' then Begin
      xyouts, .20, .20, /normal, CHARSIZE=2, 'Developed at IASB-BIRA and NCAR/ACD'
      xyouts, .10, .15, /normal, CHARSIZE=1.5, $
        'http://wwww.acd.ucar.edu         http://www.oma.be/BIRA-IASB/'
   EndIf
   image8 = TVRD()   &   image24 = pseudo8_to_true24( image8 )
   for i_frame = 0, 6 do MPEG_PUT, mpeg_id, frame=i_frame, image=image24, order=1
EndIf   

contour, zl.matrix, x.coord, y.coord, xstyle=1, xrange=[ -90., 90. ],   $
   xticks=6, xtickname=['90S','60S','30S','Eq','30N','60N','90N'],      $
   xtitle=x.name+' ('+x.units+')', ytitle=y.name+' ('+y.units+')',      $
   ymargin = [9,1], ticklen=-0.015, xminor=3, /nodata 
   
tbar0 = .15   &   tbar1 = 1. - tbar0
px = [tbar0, tbar1, tbar1, tbar0, tbar0]   &   py = [.04, .04, .07, .07, .04]
plots, px, py, /normal
xyouts, .02, .05, string(date.month, date.day, date.year, FORMAT=Dfmt), /normal
xyouts, .87, .05, string(date1.month,date1.day,date1.year,FORMAT=Dfmt), /normal
device, GET_GRAPHICS_FUNCTION=old_graph_fct

For d_id = idp0, idp1 do Begin
   i_frame = i_frame+1
   archread, zl.name, d_id, zl
   device, window_state = win_exist
   if not win_exist(7) then GOTO, FINISH
   contour, zl.matrix, x.coord, y.coord, xstyle=1, xrange=[ -90., 90. ], $
            level=scale.vector, /fill, c_color=mycolors, /overplot
   lpn = arch.latpn(d_id)
   If show.pdn and (ABS(x.coord(0)) gt ABS(lpn)) then Begin             ; draw new instance
      plots, [lpn,lpn], [y.coord(0)+1.,y.coord(y.dim-1)-1.],col=ncols_avail-1, lines=2
      plots, [-lpn,-lpn],[y.coord(0)+1.,y.coord(y.dim-1)-1.],col=ncols_avail-1,lines=1
   EndIf
   xtbar = tbar0 + (tbar1-tbar0) * (d_id-idp0) / (idp1-idp0)
   plots, [xtbar,xtbar], [.04, .07 ], col=ncols_avail-1, /normal
   If savesw then Begin
      image8 = TVRD()   &   image24 = pseudo8_to_true24( image8 )
      MPEG_PUT, mpeg_id, frame=i_frame, image=image24, order=1
   EndIf
   If show.pdn and (ABS(x.coord(0)) gt ABS(lpn)) then Begin
      device, SET_GRAPHICS_FUNCTION=6               ; Set xor graphics fct
                                                    ; erase previous instance
      plots, [lpn,lpn], [y.coord(0)+1.,y.coord(y.dim-1)-1.],col=ncols_avail-1, lines=2
      plots, [-lpn,-lpn],[y.coord(0)+1.,y.coord(y.dim-1)-1.],col=ncols_avail-1,lines=1
      device, SET_GRAPHICS_FUNCTION=old_graph_fct   ; Set normal graphics fct
   EndIf
EndFor

If savesw then Begin
   MPEG_SAVE, mpeg_id, filename=mpeg_file
   MPEG_CLOSE, mpeg_id
   msg = DIALOG_MESSAGE( mpeg_file+' successfully written' )
EndIf

FINISH:
if win_exist(7) then WDELETE, 7
WIDGET_CONTROL, draw_id, GET_VALUE=draw_win
WSET, draw_win

return
end
