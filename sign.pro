;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inserted by AFM  2004
;
;  the sign function
;
function sign, x
if (x NE 0.0) then begin 
return, x/abs(x)
endif else begin
return, x
endelse
end
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
