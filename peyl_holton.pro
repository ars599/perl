;+
;========================================================================
; NAME:
;       PEYL_holton
;
; PURPOSE:
;       compute altitude given Psurf, P and Tsurf using Holton formulation
;
; CALLING SEQUENCE:
;       
;
; INPUTS:
;     
;
; OPTIONAL INPUT PARAMETERS:
; 
;
; OUTPUTS:
;
;       
; RESTRICTIONS:
; 
;
;========================================================================
FUNCTION PEYL_HOLTON,P,Psurf=Psurf,Tsurf=Tsurf,noinfo=noinfo,reverse=reverse

if not keyword_set(reverse) then reverse=0
if not keyword_set(noinfo) then noinfo=0
if n_elements(Psurf) eq 0 then Psurf=98400.
if n_elements(Tsurf) eq 0 then Tsurf=293.
g = 9.81
R = 287.
gama = 6.5e-3
if noinfo eq 0 then begin
    print,'Peyl_holton : Warning Pression doit etre en Pa !!'
    print,'Psurf = ',psurf(0)
    print,'Tsurf = ',tsurf(0)
endif

if reverse then return,float(Psurf) * (1.-P*gama/Tsurf)^(g/(R*gama)) else $
  return, Tsurf/gama * (1.-(float(Psurf)/p)^(-R*gama/g))
END
;-


