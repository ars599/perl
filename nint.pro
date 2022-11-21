;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; return the closest interger to a given real..
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function nint,floater
if floater ge 0. then if floater-fix(floater) gt .5 then floater=floater+1 
if floater le 0. then if abs(floater-fix(floater)) gt .5 then floater=floater-1 
return,fix(floater)
end
;-
