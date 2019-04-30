pro switchfiles, ref=ref

common axes       
common axes2      
common axes3      
common arch_info 
common plots_info 
common select_id  
common dataranges 
common widgets_info
common last_com
common last_fields

WIDGET_CONTROL, /HOURGLASS

if KEYWORD_SET(ref) then Begin
   depth_id1 = z3.did   &   var_id1 = z3.vid   &   arch1 = arch3
   x1 = x3              &   y1 = y3            &   depth1 = depth3
 EndIf Else Begin
   depth_id1 = z2.did   &   var_id1 = z2.vid   &   arch1 = arch2
   x1 = x2              &   y1 = y2            &   depth1 = depth2
EndElse

find_ids, ref=ref   &   reset_titles = 1
                                           ; now switch all files parameters
depth_id0 = zl.did     &   zl.did = depth_id1     &   depth_id1 = depth_id0
var_id0 = zl.vid       &   zl.vid = var_id1       &   var_id1 = var_id0
arch0 = arch           &   arch = arch1           &   arch1 = arch0
x0 = x                 &   x = x1                 &   x1 = x0
y0 = y                 &   y = y1                 &   y1 = y0
depth0 = depth         &   depth = depth1         &   depth1 = depth0

if KEYWORD_SET(ref) then Begin
   z3.did = depth_id1   &   z3.vid = var_id1   &   arch3 = arch1
   x3 = x1              &   y3 = y1            &   depth3 = depth1
 EndIf Else Begin
   z2.did = depth_id1   &   z2.vid = var_id1   &   arch2 = arch1
   x2 = x1              &   y2 = y1            &   depth2 = depth1
EndElse

depth.i0 = 0     &   depth.i1 = depth.dim-1
idp0 = depth.i0   &   idp1 = depth.i1     ; to remove ASAP
If arch_to_compOK then Begin
   idp2_0 = 0   &   idp2_1 = depth2.dim-1
EndIf
If arch_refOK then Begin
   idp3_0 = 0   &   idp3_1 = depth3.dim-1
EndIf
if arch.OK.molat then time_labels, depth
if y.name eq 'log-p altitude' then yp.coord = 1013.25 * EXP( - y.coord/7. )
                                           ; update depthlist widget
WIDGET_CONTROL, depthlistwidid, SET_VALUE=arch.list.depth(0:depth.dim-1),   $
                SET_LIST_SELECT=zl.did
                                           ; update varlist widget
WIDGET_CONTROL, varlist_id, SET_VALUE=arch.list.vars(0:arch.list.nbvars-1), $
                SET_LIST_SELECT=zl.vid

                                           ; now re-read fields and re-draw all
archread, arch.list.vars(zl.vid), zl.did, zl
find_ids, /read   &   find_ids, /ref, /read

last.plot2d = 'x-y'   &   viewpreproc

end
