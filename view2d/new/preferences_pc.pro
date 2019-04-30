PRO preferences

;============================================================================
;== Change the variables set here to adapt the IDL program view2d to your  ==
;== special case                                                           ==
;============================================================================

;============================================================================
;== 1. PATH settings                                                       ==
;============================================================================

common path_com

; Set this string to the most usual path to your input NetCDF (*.nc) files
arch_path1 = 'D:/projects/socrates/arch/'

; OPTIONAL: Set this string to a less ususal (often NFS-mounted drives) path
; to your input NetCDF (*.nc) files.
; If useless to you, set this to '' (null string)
remote_path = 'E:/hercules/socrates/archives/'

; Set this string to the path of your output plot files
write_plots_path = 'D:/projects/socrates/view2d/ps/'

; Set this string to the path of your output ASCII data files
write_ascii_path = 'D:/projects/socrates/view2d/data/'

;============================================================================
;== 2. PRINTER setting                                                     ==
;============================================================================

common hardware

; Set this string to your *default* command to print a PostScript file
; The filename 'idl.ps' will be added in the program. For exemple, on
; my HP-unix with nljaero4 as main printer, print_command = 'lpr -dnljaero4'
; NOTE: USED BY UNIX SYSTEMS ONLY. On other OS, a screen dump will be sent
; to the printer.
print_command = 'lpr -dnljaero4'

;============================================================================
;== 3. Memory requirements settings. Set below the maximal dimension for
;==    data arrays. Obviously the bigger you set, the more memory you'll need
;==    to run this application. All arrays are dimensioned to maxdim along
;==    all their dimensions. Sorry, I don't master OOP yet  !-(
;============================================================================

common max_com

; Maximum dimension for all axes in input NetCDF (*.nc) file and for number
; of variables that it contains. For SOCRATES, the biggest difficulty is with
; axis 3, the nb of simulation dates saved. maxdim=300 seems to use 11 Mb .
maxdim = 300

; Maximum number of NetCDF (*.nc) files that you can open and compare
narchmax = 8

;============================================================================
;== 4. Color settings                                                      ==
;============================================================================

; set colorSW to 0 for B/W plotting, 1 for color/grayscale plotting
colorSW = 1

; ncolors specifies the number of colors to use for color contour plots.
; Set to an integer BELOW 50. I recommend ncolors=23 for 24-bits displays and
; ncolrs=30 for 8-bits displays.
ncolors = 23

; lcolors is a vector of narchmax color indexes for the lines from
; non-foreground files in 1D-plots. Set them to evenly spaced integer values
; between 0 and 255 - try until you get a colors on your display
lcolors = [ 15, 35, 55, 75, 95, 115, 135, 155 ]

;============================================================================
;== 5. Screen settings                                                     ==
;============================================================================

; draw_xsize and draw_ysize set the size in pixels of the plot area.
; If set to zero, these variables will be automatically recalculated to fit the
; size of the view2d window to the screen (recommended).
draw_xsize = 0
draw_ysize = 650

; main_xoffset and main_yoffset set the original position of the view2d
; window. Set to -9999 to let the Window manager take care of it.
; Set to 0 to place upper-left corner of window in upper-left corner of screen.
; If result is bad (e.g. menu bar not accessible) find correct value manually.
main_xoffset = -9999
main_yoffset = -9999

;============================================================================
;== 6. Data-dependent settings                                             ==
;============================================================================

common formats  ; Formats are string variables with same meaning than in Fortran
Xfmt = '(F4.0)' ; To print the coordinates of the X axis
Yfmt = '(F4.0)' ; To print the coordinates of the Y axis
Dfmt = '(A3,"/",I2,"/",I4)' ; To print the dates (Jan/01/1995) extracted from coordinates of the Depth axis

return
end
