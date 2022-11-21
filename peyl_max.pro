;+
;=======================================================================
; NAME:
;       PEYL_MAX
;
; PURPOSE:
;       Calculate the maximum of an array containing
;	masked data at a value > mask_value..
;
; CALLING SEQUENCE:
;       max = peyl_max (x, mask_value)
;
; INPUTS:
;	x : array of initial values
;	mask_value : the value of the data to mask
;
; OPTIONAL INPUT PARAMETERS:
;	none    
;
; OUTPUTS:
;	max : the maximum of x (without mask_value)
;       
; RESTRICTIONS:
;	none
; 
;=======================================================================
FUNCTION PEYL_MAX, x, mask=mask, maxval=maxval, minval=minval, noinfo=noinfo,imax=imax
if n_elements(mask) eq 0 then mask = 1.e19
if n_elements(maxval)eq 0 then maxval = 1.e19
if n_elements(minval)eq 0  then minval = -1.e19
if not keyword_set(noinfo) then noinfo = 0

imax=-1
ii = where (x ne mask and x gt minval and x lt maxval and finite(x) ne 0, nc)
if nc gt 0 then begin
    return,max(x(ii),imax) 
endif else begin
    if noinfo eq 0 then PRINT,'Warning PEYL_MAX : all values are masked'
    return,mask
endelse
end
;-

