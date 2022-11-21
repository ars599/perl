;+
;=======================================================================
; NAME:
;       PEYL_MIN
;
; PURPOSE:
;       Calculate the minimum of an array containing
;	masked data at a value > mask_value..
;
; CALLING SEQUENCE:
;       min = peyl_min (x, mask_value)
;
; INPUTS:
;	x : array of initial values
;	mask_value : the value of the data to mask
;
; OPTIONAL INPUT PARAMETERS:
;	none    
;
; OUTPUTS:
;	min : the minimum of x (without mask_value)
;       
; RESTRICTIONS:
;	none
; 
;=======================================================================
FUNCTION PEYL_MIN, x, mask=mask, maxval=maxval, minval=minval, noinfo=noinfo
if n_elements(mask) eq 0 then mask = 1.e35
if n_elements(maxval)eq 0 then maxval = 1.e35
if n_elements(minval)eq 0  then minval = -1.e35
if not keyword_set(noinfo) then noinfo = 0

ii = where (x ne mask and x gt minval and x lt maxval, nc)
if nc gt 0 then return,min(x(ii)) else begin
   if noinfo eq 0 then PRINT,'Warning PEYL_MIN : all values are masked'
   return,mask
endelse
end
;-






