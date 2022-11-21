;+
;=========================================================================
; NAME:
;       PEYL_FOOTNOTE
;
; PURPOSE:
;       print a text at the bottom of a graph in small size plus the date
;	If the text is a vector then print separe lines.
;	if the length of a line is too long then cut it half.
;
; CALLING SEQUENCE:
;       peyl_footnote, text
;
; INPUTS:
;	text : text vector
;
; OUTPUTS:
;	none
;
; RESTRICTION:
;	no more than 5 lines can be printed..
;       
;=========================================================================
PRO peyl_footnote, text

time=systime(0)
aout=strarr(5)
pos=[-0.03,-.05,-.07,-.09,-.11]

nb_ligne=n_elements(text)
k=0
for n=0,nb_ligne-1 do begin

    nb_car = strlen(text(n))

;--- Determine if text not too long...
    xyouts,0.,-2.,text(n),charsize=0.5,charthick=1.5,width=ll,/normal
    if ll gt .75 then begin
       aout(k)=strmid(text(n),0,fix(nb_car/2))
       k=k+1
       aout(k)=strmid(text(n),fix(nb_car/2)+1,fix(nb_car/2)-1)
    endif else begin
       aout(k)=text(n)
    endelse
    k=k+1
endfor

if k gt 4 then begin
   print,'PEYL_FOOTNOTE error : too much line to plot...'
   stop
endif

xyouts,0.,pos(0),time,charsize=0.7,charthick=1.5,/normal
for n=0,k-1 do begin
    xyouts,0.,pos(n+1),aout(n),charsize=0.7,charthick=1.5,/normal
endfor

;print,ll,aout

END
;-



