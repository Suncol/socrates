PRO xface_update_chgfore

common i_com                       ; in : narch, ifore, action
common arch_info                   ; in : arch(ifore)
common widgets_info                ; inout : wid

wid.varlist.set = 1   &   wid.dlist.set = 1
wid.varlist.dim = arch(ifore).nbvars   &   wid.varlist.val = arch(ifore).var(*)
wid.dlist.dim = arch(ifore).d.dim   &   wid.dlist.val = arch(ifore).dstring(*)
wid.chem.ok = arch(ifore).ok.ntot
wid.d2xy.set = 1   &   wid.d2dx_fix.set = 1   &   wid.d2dx_col.set = 1
wid.d2xy.val = arch(ifore).x.name + '-' + arch(ifore).y.name
wid.d2dx_fix.val = arch(ifore).d.name+'-'+arch(ifore).x.name+' at fixed '      $
                 + arch(ifore).y.name
wid.d2dx_fix.ok = arch(ifore).ok.d_as_xplot
ivar = pos(ifore).ivar
wid.d2dx_col.val = arch(ifore).d.name+'-'+arch(ifore).x.name+' (column)'
wid.d2dx_col.ok = arch(ifore).ok.d_as_xplot                                    $
                  and ( (arch(ifore).units(ivar) eq 'vmr')                     $
                       or (arch(ifore).units(ivar) eq 'molec/cm3') )
wid.d2dy_fix.set = 1   &   wid.d2dy_glob.set = 1
wid.d2dy_fix.val = arch(ifore).d.name+'-'+arch(ifore).y.name+' at fixed '      $
                 + arch(ifore).x.name
wid.d2dy_fix.ok = arch(ifore).ok.d_as_xplot
wid.d2dy_glob.val = arch(ifore).d.name+'-'+arch(ifore).y.name+' (global avg)'
wid.d2dy_glob.ok = arch(ifore).ok.d_as_xplot

END

;=========================================================================

PRO xface_update     ; OOP: this could become a method for class of object "wid"

common i_com                       ; in : narch, ifore, action
common max_com                     ; in : narchmax
common arch_info                   ; in : arch(ifore)
common last_com                    ; in : last.action
common widgets_info                ; inout : wid

If narch le 0  then Begin
;=========================================================================
;=== Initialization: most event-generating widgets set to unsensitive ====
;=========================================================================
   wid.main.ok = 1
   wid.open_fore.ok = 1
   wid.list_fore.ok = 0
   wid.back.ok = 0
   wid.tools2.ok = 0
   wid.close_fore.ok = 0
   wid.close_back.ok = 0
   wid.xdata.ok = 0
   wid.xplot.ok = 0
   wid.print.ok = 0
   wid.disp.ok = 0
   wid.d2menu.ok = 0
   wid.d1menu.ok = 0
   wid.movie.ok = 0
   wid.msg.set = 1   &   wid.msg.ok = 1
   wid.msg.val(0) = 'Open a Foreground File'

 EndIf Else Begin 
;=========================================================================
;========================= Set widget properties =========================
;=========================================================================
   If narch eq 1 then Begin
      if (action eq 'open_fore') or (action eq 'open_back') then               $
         for tag = 0, N_TAGS(wid)-1 do wid.(tag).ok = 1
      wid.list_back.ok = 0
      wid.use_back.ok = 0
      wid.close_back.ok = 0
    EndIf Else If narch lt narchmax then Begin
      wid.list_back.ok = 1
      wid.use_back.ok = 1
      wid.close_back.ok = 1
    EndIf Else If narch eq narchmax then Begin
      wid.open_fore.ok = 0
      wid.open_back.ok = 0
      wid.msg.val = 'You MUST close a file before opening one'
      wid.msg.set = 1
   EndIf
   wid.list_fore.dim = narch   &   wid.list_fore.val = arch(*).flnm
   wid.list_back.dim = narch   &   wid.list_back.val = arch(*).flnm
   If action eq 'cancelled' then Begin
      wid.msg.set = 1   &   wid.msg.ok = 1
      wid.msg.val(0) = 'Action cancelled'
   EndIf
   If (last.action eq 'cancelled') and (action ne 'cancelled') then Begin
      wid.msg.set = 1   &   wid.msg.ok = 1
      wid.msg.val(0) = 'Ready'
   EndIf

EndElse

;=========================================================================
;============== Realize the widget properties set above ==================
;=========================================================================
For tag = 0, N_TAGS(wid)-1 Do Begin
   If wid.(tag).ok then WIDGET_CONTROL, wid.(tag).id, /SENSITIVE               $
    Else WIDGET_CONTROL, wid.(tag).id, SENSITIVE=0
   If wid.(tag).set then                                                       $
      WIDGET_CONTROL, wid.(tag).id, SET_VALUE=wid.(tag).val(0:wid.(tag).dim-1)
   wid.(tag).set = 0
EndFor

END
