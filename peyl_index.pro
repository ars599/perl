;+
;========================================================================
; NAME:
;       PEYL_INDEX
;
; PURPOSE:
;	Program that return an array that correspond to the index of
;	interpolation of the array new_arr into the array old_arr..       
;	(OUTPUT SERVE FOR THE INTERPOLATE IDL ROUTINE)
;
; CALLING SEQUENCE:
;       peyl_index, old_arr, new_arr, int_idx
;
; INPUTS:
;	old_arr : array of values (one dimension) for the old base.
;
;	new_arr : array of values (one dim) to be interpolated into
;		  the old array
;
; OPTIONAL INPUT PARAMETERS:
;	none
;
; OUTPUTS:
;	int_idx : array of interpolated index of new_arr into old_arr
;       
; RESTRICTIONS:
;	work for only one dimension array.
;
;========================================================================

PRO PEYL_INDEX,old_lat,new_lat,int_idx

nnew=n_elements(new_lat)
nold=n_elements(old_lat)

int_idx=fltarr(nnew)
low_idx=fltarr(nnew)

; look for the index of the new lats in the old lats
for j=0L,nnew-1 do begin

  for i=0L,fix(nold) -1 do begin

  if new_lat(j) ge old_lat(i) then low_idx(j)=i
  endfor

 if low_idx(j) lt nold-1 then begin
  int_idx(j)= low_idx(j)+(new_lat(j)-old_lat(low_idx(j)))/$
              (old_lat(low_idx(j)+1)-old_lat(low_idx(j)))
 endif else begin
  int_idx(j)= low_idx(j)
 endelse

endfor

END
;-

