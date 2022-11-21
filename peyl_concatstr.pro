;+
;=========================================================================
; NAME:
;       PEYL_CONCATSTR
;
; PURPOSE:
;       Concatenate an array of string with a given separator.
;
; CALLING SEQUENCE:
;       result = peyl_concatstr(aa,separator)
;
; INPUTS:
;       aa : array of string or other type
;	separator : string for separation
;
; OPTIONAL INPUT PARAMETERS:
; 
; OUTPUTS:
;       result : string with all elements of aa.
;
;========================================================================
;
FUNCTION PEYL_CONCATSTR,aa,separator=separator

if not keyword_set(separator) then sep='' else sep=string(separator)
nn = n_elements(aa)
aout=string(aa(0))
for k=1,nn-1 do aout=aout+sep+string(aa(k))
return,aout
end
;-

