;+
;=========================================================================
; NAME:
;       PEYL_DIVISION
;
; PURPOSE:
;       Divide two array but return max_value if division by 0
;
; CALLING SEQUENCE:
;       result = peyl_division(a,b,max_value)
;
; INPUTS:
;       a : divident
;	b : diviseur
;	max_value : value to return if b=0
;
; OPTIONAL INPUT PARAMETERS:
;	precis : the value for which if |b|<precis the result is max_value 
; 
; OUTPUTS:
;       result : a/b
;
;========================================================================
;
FUNCTION PEYL_DIVISION,a,b,max_value, precis=precis,$
                       mask=mask, maxval=maxval, minval=minval
if not keyword_set(mask) then mask = 1.e35
if not keyword_set(maxval) then maxval = 1.e35
if not keyword_set(minval) then minval = -1.e35
bb=b
if  (n_elements(bb) ne 1 and n_elements(bb) ne n_elements(a)) then begin
   print,'ERREUR PEYL_DIVISION : wrong size :',size(bb),size(a)
   stop
endif
zero = where (bb eq 0.,count)
if keyword_set(precis) then zero = where (abs(bb) lt abs(precis), count)
if count ne 0 then begin
   bb(zero)=1.
   c=a/bb
   c(zero)=max_value
endif else c=a/bb

foo = where (b eq mask or b lt minval or b gt maxval or $
             a eq mask or a lt minval or a gt maxval, nc)
if nc gt 0 then c(foo) = mask

return,c
end
;-

