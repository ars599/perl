;+
; NAME:
;       PEYL_FIT
;
; PURPOSE:
;       function that fit an array of values to a polynome
;	of a given degre using poly_fit (idl routine) 
;	if degre = 0 then : fit = first value of data
;	if degre = -1 then : fit = line joining data(0) to data(end)
;
; CALLING SEQUENCE:
;       result = peyl_fit (y, max_number=max_number, deg_polyno=deg_polyno)
;
; INPUTS:
;       y : the array to fit
;
; OPTIONAL INPUT PARAMETERS:
;	max_number : values of the data to mask (not used for the fit)
;	deg_polyno : degre of the polynome (0 if not precised..)
;
; OUTPUTS:
;	result : structure with the elements :
;	       { yfit : polynome fitted values
;		 resid : y - yfit
;		 coef(ndegre+1) : coefficient ai..       
;	       }
;
; RESTRICTIONS:
;	value of y are supposed to be equaly spaced.
;
;=======================================================================

FUNCTION peyl_fit, y, max_number=max_number, deg_polyno=deg_polyno

if (not keyword_set(max_number)) then max_number=1.e35
if keyword_set(deg_polyno) then ndegre=deg_polyno else ndegre=0

;----- Define the output array
nb_data=n_elements(y)
aout = { yfit  :fltarr(nb_data),$
	 resid :fltarr(nb_data),$
	 coef  :fltarr(abs(ndegre)+1)}
aout.yfit(*) = 0.
aout.resid(*)= y
aout.coef(*) = 1.

;----------- Set the good data.
; i_deb: indice of first good data
; i_end: indice of last good data
;
indice_good_data = where(y ne max_number, count)
if (count gt 0) then nb_good_data = n_elements(indice_good_data) $
else begin
   print,'WARNING PEYL_FIT : All data = ',max_number
   aout.resid(*)=0.
   aout.coef(*)=0.
   return,aout
endelse
i_deb = indice_good_data(0)
i_end = indice_good_data(nb_good_data-1)

;----------- Case for only a shift of the data (ndegre=0)...
if (ndegre eq 0) then begin
   bb = y(i_deb)
   for i=i_deb,i_end do begin
       if (y(i) ne max_number) then begin
	  aout.yfit(i) = bb
	  aout.resid(i) = y(i) - bb
       endif
   endfor
   aout.coef(0) = bb
   return,aout
endif

;----------- Case for special fit to a line (ndegre=-1)...
if (ndegre eq -1) then begin
   b1 = y(i_deb)
   b2 = y(i_end)
   aa = (b2-b1)/(i_end-i_deb)
   for i=i_deb,i_end do begin
       if (y(i) ne max_number) then begin
	  aout.yfit(i) = aa * i + (b1-aa*i_deb) 
	  aout.resid(i) = y(i) - aout.yfit(i)
       endif
   endfor
   aout.coef(0) = b1
   aout.coef(1) = aa
   return,aout
endif

;----------- Polynomial fit.
x=findgen(nb_data)
y_good = y(indice_good_data)
x_good = x(indice_good_data)
x_good = x_good - x_good(0)

	     ;------ Find the coefficients of the polynome
aout.coef=poly_fit(x_good,y_good,ndegre,yfit)

	     ;------ Set the output
aout.yfit(indice_good_data)  = yfit
aout.resid(indice_good_data) = y_good - yfit

return,aout
END
;-



