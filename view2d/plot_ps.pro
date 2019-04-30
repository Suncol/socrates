pro plot_ps, to_print=to_print, encaps=encaps

;-----------------------------------------------------------------------
; This routine prepares the device and the plot settings to re-draw
; the last executed plot in a file instead than on the screen.
; Here the user can program modifications to the appearance of printed
; or exported plots. DO NOT FORGET to re-set the settings to their
; initial value at the end of this subroutine, or you're in for big trouble !
;
; At this time we handle only the PostScript (*.ps) format. Other formats are
; possible. Everything here is written for IDL version 4.01.
; Look at the online help for the 'set_plot' and 'device' commands.
; Note that 'device' settings like the '/TIMES' font setting are device-
; dependent, e.g. they work fine with *.ps or *.eps but maybe not with *.gif
; Other interesting formats are :
; + EPS, Encapsulated PostScript (*.eps) : like PostScript, but can be imported
;	 in a word processor, where the size of the graphics can be changed,
;	 still keeping the good quality of PostScript fonts !
; + CGM, Computer Graphics Metafile (*.cgm) : can be imported in a word
;	 processor, and adapted to local drawing tools, e.g. Draw for Word
; + GIF, (*.gif), interesting for universality and best for internet
;	 distribution.
;-----------------------------------------------------------------------

common hardware		; in : colorSW, print_command
common plots_info	; in : screen
common path_com		; in : write_plots_path
common last_fields	; in : zl

encaps = KEYWORD_SET( encaps )
If KEYWORD_SET( to_print ) then Begin
   fname = 'idl.ps'   &   desc = strarr(6)
   desc(0) = '1, BASE,, ROW, CENTER'
   desc(1) = '0, TEXT,'+print_command+', WIDTH=40, TAG=command'
   desc(2) = '2, LABEL, '+fname+', RIGHT'
   desc(3) = '1, BASE,, ROW, CENTER'
   desc(4) = '0, BUTTON, OK, QUIT, TAG=OK'
   desc(5) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
   form = CW_FORM(desc, /COLUMN, title='Shell command for printing')
   if form.cancel then return
   this_print_command = form.command
   spawn, 'rm -f '+fname
 EndIf Else Begin      ; Ask for the file name
   def_fname = zl.name+'.ps'   &   myfilter = write_plots_path+'*.ps'
   If KEYWORD_SET( encaps ) then Begin
      def_fname = zl.name+'.eps'
      myfilter = write_plots_path + '*.eps'
   EndIf
   fname = DIALOG_PICKFILE( file=def_fname, filter=myfilter,  $
          /fix_filter, title='Provide name for PostScript file to write' )
   if (fname eq '') then return
   junk = FINDFILE( fname, COUNT=c )
   If (c ne 0) then Begin
      junk = DIALOG_MESSAGE( 'This file already exists. OK to overwrite ?', $
                             /QUESTION )
      if (junk eq 'No') then return
   EndIf
EndElse

; find out if the 1d-plot window exists at present time

device, window_state = win_exist

; set the plotting device to PostScript

set_plot, 'ps'

; Switch the fonts from IDL-generated, vector-drawn (!p.font=-1) to
; device-dependent fonts. This the time to set other system variables
; beginning with '!' (e.g. '!p' ). Do NOT forget to re-set them to their
; initial value at the end of this subroutine !!

!p.font = 0

fs = 12 ;  &   if encaps then fs = 10
; Set the current device (now PostScript) to get the appropriate plot.
; Lots of settings can be modified through device-dependent settings to the
; 'device' coomand. Look at the Online Help. This command opens the (PostScript)
; file and put the appropriate headers in.

device, FILENAME=fname, COLOR=colorSW,                                         $
        LANDSCAPE=not encaps, PORTRAIT=encaps, encaps=encaps, preview=encaps,  $
        font_size = fs, /TIMES

; Re-load the "Rainbow+white" color table. It will be identical for all
; PostScript output but can be very different from colors on screen

loadct, 39

; Re-do the last plot, it will go to the opened file.
; Note that this program has color scale for contours specifically set for the
; 'PostScript' device and different than the color scale used on the screen.
; Look at the beginning of 'viewpreproc.pro'

redo_last, only1d=win_exist(1)

; Close the file

device, /close

; Print the plot if this was asked

If KEYWORD_SET( to_print ) then spawn, this_print_command + ' ' + fname

; Go back to the initial settings, appropriate for plotting on the screen.
; do not forget these steps is important or everything is screwed afterwards !

set_plot, screen   &   !p.font = -1   &   loadct, 39

end
