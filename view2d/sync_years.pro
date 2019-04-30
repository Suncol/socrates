PRO sync_years

common arch_info    ; inout : arch ; in : arch2, arch_to_compOK
common axes         ; inout : depth
common axes2        ; in : depth2
common widgets_info ; out : depthlistwidid
common last_fields  ; out : zl.date, z2

If not arch_to_compOK then Begin
   error = DIALOG_MESSAGE( 'There is no Bagkground file to synchronize with',  $
                                             /ERROR )
   return
EndIf

desc = strarr(7)
desc(0) = '0, LABEL, Foreground file goes from '+arch.list.depth(0)+' to ' +   $
                                       arch.list.depth(depth.dim-1)+', LEFT'
desc(1) = '0, LABEL, Background file goes from '+arch2.list.depth(0)+' to ' +  $
                                       arch2.list.depth(depth2.dim-1)+', LEFT'
desc(2) = '0, LABEL, Change the dates of the Foreground file by, LEFT'

days0date, depth.coord(0), f0
days0date, depth2.coord(0), b0
nyears = STRING( b0.year - f0.year, FORMAT='(i4)')

desc(3) = '0, INTEGER, '+nyears+',LABEL_RIGHT=years (integer):,WIDTH=5,TAG=nyears'
desc(4) = '1, BASE,, ROW, CENTER'
desc(5) = '0, BUTTON, OK, QUIT, TAG=OK'
desc(6) = '2, BUTTON, Cancel, QUIT, TAG=CANCEL'
form = CW_FORM(desc, /COLUMN, title='Change years of Foreground File')
if form.cancel then return

depth.coord(*) = depth.coord(*) + 365.*form.nyears
if arch.OK.molev then time_labels, depth
make_depthlist, depth, depthlist
arch.list.depth = depthlist
                                           ; update depthlist widget
WIDGET_CONTROL, depthlistwidid, SET_VALUE=arch.list.depth, SET_LIST_SELECT=zl.did
days0date, depth.coord(zl.did), date
zl.date = date

find_ids, /read
redo_last
    
END
