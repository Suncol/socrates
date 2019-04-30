PRO preferences

;============================================================================
;== Change the variables set here to adapt the IDL program view2d to your  ==
;== special case                                                           ==
;============================================================================

;============================================================================
;== 1. Hardware-dependent settings                                         ==
;============================================================================

common hardware

; set colorSW to 0 for B/W plotting, 1 for color/grayscale plotting
colorSW = 1

; take_colors specifies the number of colors that IDL should take. If there are
; not enough available colors, a private colormap will be created ("flashing"
; plot windows). If you want *all* 256 colors in a private color map, set to 256.
; A negative number tells IDL to use all available colors but the specified
; colors. See the COLORS keyword to the WIDGET_DRAW routine.
take_colors = 50

; main_xoffset and main_yoffset set the original position of the application
; window. Set to -9999 to let the Window manager take care of it.
; Set to 0 to place upper-left corner of window in upper-left corner of screen.
; If result is bad (e.g. menu bar not accessible) find correct value manually.
main_xoffset = -9999
main_yoffset = -9999

; draw_xsize and draw_ysize set the size in pixels of the 2D-plots (contour).
; If set to zero, these variables will be automatically recalculated to fit the
; size of the application window to the screen (recommended).
draw_xsize = 0
draw_ysize = 600

;============================================================================
;== 2. PATH setting                                                        ==
;============================================================================

common path_com

; Set this string to the LOCAL path of your input NetCDF (*.nc) files
local_path = 'd:/Projects/socrates/results'

; Set this string to the REMOTE path of your input NetCDF (*.nc) files
remote_path = 'e:/hal/socrates/nc_files'

; Set this string to the path of your output plot files
write_plots_path = 'ps/'

; Set this string to the path of your output ASCII data files
write_ascii_path = 'data/'


;============================================================================
;== 3. Data-dependent settings                                             ==
;============================================================================

common formats  ; Formats are string variables with same meaning than in Fortran
Xfmt = '(F4.0)' ; To print the coordinates of the X axis
Yfmt = '(F4.0)' ; To print the coordinates of the Y axis
Dfmt = '(A3,"/",I2,"/",I4)' ; To print the dates (Jan/01/1995) extracted from coordinates of the Depth axis

return
end
