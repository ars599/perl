;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inserted by AFM  2004
;
;  the sign function
;
function sign2, x
ii= where (x NE 0.0) 
y=x
y[*,*]=0
y(ii)=x(ii)/abs(x(ii))
return, y
end
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
