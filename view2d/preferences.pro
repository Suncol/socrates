PRO preferences

;============================================================================
;== Change the variables set here to adapt the IDL program view2d to your  ==
;== special case							   ==
;============================================================================

;============================================================================
;== 0. Maximum dimensions: higher they are, more memory view2d uses        ==
;============================================================================

common maxcom
maxdimx = 36   &   maxdimy = 121
maxdimdepth = 760   &   maxvars = 140   ; max nb of archived dates & vars

;============================================================================
;== 1. PATH settings       					   	   ==
;============================================================================

common path_com

; Set this string to the LOCAL (i.e. most usual) path of your input NetCDF 
; (*.nc) files
local_path = '/bira-iasb/projects/socrates/'

; OPTIONAL: Set this string to another, less ususal path to your input NetCDF 
; (*.nc) files. If useless to you, set this to '' (null string)
other_path = '/bira-iasb/projects/socrates/climato/'

; OPTIONAL: Set this string to yet another path to your input NetCDF (*.nc) 
; files. If useless to you, set this to '' (null string)
remote_path = ''

; Set this string to the path of your output plot files
write_plots_path = '/home/simonc/socrates/view2d/ps/'

; Set this string to the path of your output ASCII data files
write_ascii_path = '/home/simonc/socrates/view2d/data/bascoe_init/'

;============================================================================
;== 2. PRINTER setting        					   	   ==
;============================================================================

common hardware

; Set this string to your *default* command to print a PostScript file
; This is IGNORED ON NON-UNIX SYSTEMS. The filename 'idl.ps' will be added 
; in the program. For exemple, on my HP-unix with nljaero4 as main printer, 
; I set print_command = 'lpr -dnljaero4'
print_command = 'lp -dcolorljet3'

;============================================================================
;== 3. Hardware-dependent settings					   ==
;============================================================================

; set colorSW to 0 for B/W plotting, 1 for color/grayscale plotting
colorSW = 1

; ncolors specifies the number of colors to use for color contour plots.
; Set to an integer BELOW 50. I recommend ncolors=23 for 24-bits displays and 
; ncolors=40 for 8-bits displays (run view2d before other apps take many colors)
ncolors = 23

; l2color is the color index for the 2d line (from Background file) in 1D-plots.
; Set it to an integer value between 0 and 255 - try until you get a pleasant
; color on your display
l2color = 76   ;   BYTE( 0.66* !D.N_COLORS )

; l3color is the color index for the 3rd line (from Reference file) in 1D-plots.
; Set it to an integer value between 0 and 255 - try until you get a pleasant
; color on your display
l3color = 152   ;   BYTE( 0.33* !D.N_COLORS )

; main_xoffset and main_yoffset set the original position of the view2d
; window. Set to -9999 to let the Window manager take care of it.
; Set to 0 to place upper-left corner of window in upper-left corner of screen.
; If result is bad (e.g. menu bar not accessible) find correct value manually.
main_xoffset = -9999
main_yoffset = -9999

; draw_xsize and draw_ysize set the size in pixels of the 2D-plots (contour).
; If set to zero, these variables will be automatically recalculated to fit the 
; size of the view2d window to the screen (recommended).
draw_xsize = 0
draw_ysize = 0
		
;============================================================================
;== 4. Data-dependent settings						   ==
;============================================================================

common formats	; Formats are string variables with same meaning than in Fortran
Xfmt = '(F4.0)' ; To print the coordinates of the X axis
Yfmt = '(F4.0)' ; To print the coordinates of the Y axis
Dfmt = '(A3,"/",I2,"/",I4)' ; To print the dates (Jan/01/1995) extracted from coordinates of the Depth axis 

end
