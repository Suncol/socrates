function up_subscript, z

; This function finds digits in incoming strings z.name and z.units, and makes 
; them subscript or upperscript (depending of z.units) unless they are 
; preceded by a hyphen'-'. This will work with the default vector-drawn faults
; (if !p.font=-1) and the PostScript fonts (if !p.font=0 and the current device 
; is 'ps'). I have no idea of the effect with other fonts.
; If z looks like a chemical species, it will turn the following letters 
; to capitals : A, B, C, F, H, I, N, O, P and will turn 'x' and 'y' in subscripts.

thistring = [z.name, z.units ]

If (z.units eq '%') then return, thistring
If (z.name eq 'o1d') then Begin
   thistring = [ 'O(!U1!ND)', '' ]
   GOTO, UNITS
 EndIf Else If (z.name eq 'o3p') then Begin
   thistring = [ 'O(!U3!NP)', '' ]
   GOTO, UNITS
EndIf

len = STRLEN(z.name)   &   char = strarr(len)
if (len eq 7) and (STRMID(z.name,0,6) eq 'tracer') then return, thistring
For i = 0, len-1 Do char(i) = STRMID( z.name, i, 1 )
thistring = STRARR(2)

If (z.units eq 'vmr') or (z.units eq 'molec/cm3') 			 $
    or (z.units eq 'molec/cm2') or (z.units eq 'Dobson Units') then Begin
   For i = 0, len-1 Do Begin
      mybyte = BYTE(char(i))
      If (z.name ne 'totdens') and ((char(i) eq 'a') or (char(i) eq 'b')  $
             or (char(i) eq 'c') or (char(i) eq 'f') or (char(i) eq 'h')  $
             or (char(i) eq 'i') or (char(i) eq 'n') or (char(i) eq 'o')  $
             or (char(i) eq 'p')) then char(i) = STRUPCASE(char(i))       $
       Else If (char(i) eq '-') or (char(i) eq '+') then char(i) = '!U' + char(i) + '!N' $
       Else If (char(i) eq ' ') then GOTO, CONTINUE   $
       Else If (i ge 1) and ((char(i) eq 'x') or (char(i) eq 'y')         $
                      or ((mybyte(0) ge 49) and (mybyte(0) le 57))) then  $
         char(i) = '!D' + char(i) + '!N'
  EndFor
 EndIf Else Begin
   For i = 0, len-1 Do Begin
      mybyte = BYTE(char(i))
      If (mybyte(0) ge 49) and (mybyte(0) le 57) then Begin
         char(i) = '!U' + char(i) + '!N'
      EndIf
   EndFor
EndElse

CONTINUE:

For i = 0, len-1 Do thistring(0) = thistring(0) + char(i)

UNITS:

If (z.units eq 'molec/cm3') then Begin
   thistring(1) = 'cm!U-3!N'
 EndIf Else Begin
   len = STRLEN(z.units)   
   If (len eq 0) then Begin
      thistring(1) = 'no_units'
    EndIf Else Begin
      char = strarr(len)
      For i = 0, len-1 Do char(i) = STRMID( z.units, i, 1 )
      For i = 1, len-1 Do Begin
         mybyte = BYTE(char(i))
         If (char(i) eq '-') or ((mybyte(0) ge 49) and (mybyte(0) le 57)) then Begin
               char(i) = '!U' + char(i) + '!N'
         EndIf
      EndFor
      For i = 0, len-1 Do thistring(1) = thistring(1) + char(i)
   EndElse
EndElse

return, thistring

end
