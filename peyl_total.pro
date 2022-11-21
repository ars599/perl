;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; return the mean value of an array
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function peyl_total, arr, mask=mask, maxval=maxval, minval=minval, noinfo=noinfo
if not keyword_set(noinfo) then noinfo = 0
if n_elements(mask) eq 0 then mask = 1.e35
if n_elements(maxval)eq 0 then maxval = 1.e35
if n_elements(minval)eq 0  then minval = -1.e35
ii = where (arr ne mask and arr gt minval and arr lt maxval, nc)
if nc gt 0 then return,total(arr(ii)) else begin
    if noinfo eq 0 then PRINT,'Warning PEYL_TOTAL : all values are masked'
    return,mask
endelse

end
;-
