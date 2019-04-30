pro animater, play=play, gif=gif, GROUP=GROUP

common arch_info
common axes
common select_id
common last_fields
common movies

if KEYWORD_SET(play) then GOTO, PLAYER

animtitle = zl.name+' ('+zl.units+') ; '+zl.loc(0)+' ; '+zl.loc(1)+' ; '+zl.loc(2)

If KEYWORD_SET(gif) then Begin
   get_flnm, 'gif', zl.name, flnm, GROUP=GROUP
   window, 0, xsize=500, ysize=400, TITLE=animtitle, RETAIN=2
 EndIf Else Begin
   xanimate, /close
   xanimate, SET=[500,400,depth.dim]
   window, 0, xsize=500, ysize=400, TITLE=animtitle
EndElse

mycolors = 12*INDGEN(30) + 10
print, 'Activate this text window and Type Control-C to cancel !'
print, 'Creating movie, % done : '
For iframe = 0, depth.dim - 1 Do Begin
    archread, varlist(var_id), iframe, zl
    erase
    viewer, zl, follow=0, /fill, c_color=mycolors  ;  , title='', subtitle=''
    If KEYWORD_SET(gif) then Begin
      gifflnm = '/scratch/simonc/gif/'+flnm+ STRTRIM(STRING(iframe),2)+'.gif'
       WRITE_GIF, gifflnm, TVRD()
     EndIf Else Begin
       xanimate, FRAME=iframe, WINDOW = 0
    EndElse
    print, 100*iframe/(depth.dim-1),'%  ', FORMAT='(i3,a,$)'
EndFor

If KEYWORD_SET(gif) then Begin
   print,'Files '+flnm+'xx.gif   successfully written in /scratch/simonc/gif'
   return
EndIf

PLAYER :

print
print,'Activate this text window and Type Any key to stop'
xanimate, 4 
print,'Type "return" to use the selecter again'
stop

return
end
