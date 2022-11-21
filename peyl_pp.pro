;+
;==================================================================
; NAME:
;       PEYL_pp
;
; PURPOSE:
;       print on sceen floating value(s) with a format
;
; CALLING SEQUENCE:
;       result = peyl_arrondi, val, format=format
;
; INPUTS:
;       val    : array of values 
;
; OPTIONAL INPUT PARAMETERS:
;       format : specified fortran format (like f12.5)
;                Default is f12.4
;
; OUTPUTS:
;
; RESTRICTIONS:
; 
;
;==================================================================
PRO PEYL_PP, val, format=format

if (not keyword_set(format)) then format='(f12.4)'

print,format=format,val

END
;-

