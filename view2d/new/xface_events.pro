;------------------------------------------------------------------------------
;     procedure xface_events - processes the events being sent by the XManager.
;------------------------------------------------------------------------------
PRO xface_events, event

common max_com             ; in : maxdim, narchmax
common path_com            ; in : arch_path1
common i_com               ; inout : narch, ifore, iback, pos
common hardware            ; inout : colorSW
common arch_info           ; inout : arch
common plots_info          ; inout : show
common last_com            ; inout : last
common widgets_info        ; inout : wid

WIDGET_CONTROL, event.id, GET_UVALUE = action

CASE action OF

;*** here is where you would add the actions for your events.
;*** Each widget you add should have a unique string for its user value

'exit'      :   WIDGET_CONTROL, event.top, /DESTROY
'open_fore' :   open_arch
'set_fore'  :   Begin
   ifore = event.index   &   xface_update_chgfore
                End
'open_back' :   open_arch
'close_fore':   Begin
   arch(ifore) = {archstruc}   &   narch = narch-1
   for i = ifore, narch do arch(i) = arch(i+1)
   If narch le 1 then Begin
      ifore = narch-1   &   iback = -1
    EndIf Else Begin
      ifore = iback   &   iback = -1
      for i = 0, narch-1 do if i ne ifore then iback = i
   EndElse
   xface_update_chgfore
                End
'ncbrowse'    : ncbrowse, arch(ifore).flnm, PATH=arch_path1,GROUP=wid.main.id
'export_data' : export_data
'plot_to_file': plot_to_file
'print':        If !D.name ne 'X' then junk = DIALOG_PRINTJOB()                $
                 Else plot_to_file, /to_print
'printsetup'  : printsetup
'chgscale' :    Begin
   chgscale   &   show.autolev = 0   &   redo_plot
                End
'chgtitles':    chgtitles
'chgaxes' :     chgaxes
'autolevs':   Begin
    show.autolev = 1   &   last.plot.type='d2'   &   redo_plot
              End
'fixlevs':    show.autolev = 0
'smooth'    : smooth_field
'showsw'    : show_xface
'colorsw'   : Begin
   colorSW = NOT colorSW   &   redo_plot
              End
'colorch'   : Begin
   If (NOT colorSW) then Begin
      colorSW = 1   &   redo_plot
   EndIf
   xloadct
              End
'unzoom'    : unzoom
'about'     : xdisplayfile, 'VERSION.txt', TITLE='about view2d...'
'varlist'   : Begin
   pos.ivar = event.index
   read_nf, ifore   &   calc_plot   &   redo_plot
              End
'dlist':      Begin
   pos.id = event.index
   read_nf, ifore   &   calc_plot   &   redo_plot
              End
'linsw':      Begin
   show.log = 0
   if last.plot.type eq 'd1' then redo_plot
              End
'logsw':      Begin
   show.log = 1
   if last.plot.type eq 'd1' then redo_plot
              End
'idl_shell' : Begin
   print,'!!!!!----------------------- TYPE "return" TO RESUME EXECUTION'
   stop
              End

  ELSE: MESSAGE, 'Event User Value "'+action+'" Not Found'
ENDCASE

junk = WHERE( action eq ['exit','ncbrowse','cancelled','drawEvent'], count )
If count eq 0 then xface_update

if (action ne 'cancelled') then last.action = action

END
