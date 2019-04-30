PRO wdelete1

common plots_info   ; in : screen

win_exist = bytarr(33)   &   win_exist(1) = 0
If (!D.NAME eq screen) then device, window_state = win_exist
if win_exist(1) then wdelete, 1

END ;==========================================================================
