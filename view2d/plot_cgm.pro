pro plot_cgm

;-----------------------------------------------------------------------
; This routine prepares the device and the plot settings to re-draw
; the last executed plot in a file instead than on the screen.
; Here the user can program modifications to the appearance of printed 
; or exported plots. DO NOT FORGET to re-set the settings to their 
; initial value at the end of this subroutine, or you're in for big trouble !
;-----------------------------------------------------------------------

common hardware		; in : colorSW
common plots_info	; in : screen
common path_com		; in : write_plots_path
common last_fields	; in : zl

; Ask for the file name

def_fname = write_plots_path+zl.name+'.cgm'
fname = DIALOG_PICKFILE( file=def_fname, filter='*.cgm', /fix_filter,  $
       title='Provide name for GGM file to write' )
if (fname eq '') then return
junk = FINDFILE( fname, COUNT=c )
If (c ne 0) then Begin
   junk = DIALOG_MESSAGE( 'This file already exists. OK to overwrite ?', $
                          /QUESTION )
   if (junk eq 'No') then return
EndIf

; find out if the 1d-plot window exists at present time

device, window_state = win_exist

; set the plotting device to GGM

set_plot, 'cgm' 

; Switch the fonts from IDL-generated, vector-drawn (!p.font=-1) to
; device-dependent fonts. This the time to set other system variables
; beginning with '!' (e.g. '!p' ). Do NOT forget to re-set them to their 
; initial value at the end of this subroutine !!

!p.font = 0

; Set the current device (now GGM) to get the appropriate plot.
; Lots of settings can be modified through device-dependent settings to the
; 'device' coomand. Look at the Online Help. This command opens the (GGM)
; file and put the appropriate headers in.

device, FILENAME=fname

; Re-load the color table in this new device
if colorSW then loadct, 39              ; "Rainbow+white" color table

; An ERASE command sets the background color to the value in the color map 
; at index 0
ERASE

; Re-do the last plot, it will go to the opened file.
; Note that this program has color scale for contours specifically set for the
; 'GGM' device and different than the color scale used on the screen.
; Look at the beginning of 'viewpreproc.pro'

redo_last, only1d=win_exist(1)

; Close the file 

device, /close

; Go back to the initial settings, appropriate for plotting on the screen.
; do not forget these steps is important or everything is screwed afterwards !

set_plot, screen   &   !p.font = -1

return
end
