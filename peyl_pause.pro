;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Stop temporaly the programme : hit a key to continue
;		                 hit 'q' to stop 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION peyl_pause

print,'Press "q" to stop, any other key to continue ?'
rep=strlowcase(get_kbrd(1))
if rep eq 'q' then return,1 else return,0
end
;-
