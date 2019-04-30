pro plot_gif

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

def_fname = write_plots_path+zl.name+'.gif'
fname = DIALOG_PICKFILE( file=def_fname, filter='*.gif', /fix_filter,  $
       title='Provide name for GIF file to write' )
if (fname eq '') then return
junk = FINDFILE( fname, COUNT=c )
If (c ne 0) then Begin
   junk = DIALOG_MESSAGE( 'This file already exists. OK to overwrite ?', $
                          /QUESTION )
   if (junk eq 'No') then return
EndIf

device, window_state = win_exist

old_p = !p
;   !p.charsize=2   ;   &   !p.background=255
device, set_font='Times', /TT_FONT

redo_last, only1d=win_exist(1)

junk = DIALOG_MESSAGE( 'This image is OK for capture?', /QUESTION )

if (junk eq 'Yes') then write_gif, fname, TVRD()

!p = old_p
redo_last, only1d=win_exist(1)

return
end
