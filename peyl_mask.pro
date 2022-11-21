;+
;========================================================================
; NAME:
;       PEYL_MASK
;
; PURPOSE:
;       Programme that mask an array (lon,lat,"mon") 
;	for a specified grid over continent or ocean.
;	the value over masked point is 1.e35
;
; CALLING SEQUENCE:
;       arr_masked = peyl_mask (arr, grid_name, /land, /ocean, /entier)
;
; INPUTS:
;	arr : the geophysical field to mask (lon, lat, "mon")
;
;	grid_name : string of the name of the grid that correspond 
;		    to this field
;
; OPTIONAL INPUT PARAMETERS:
;	/land : if this keyword is set then land are masked..
;
;	/ocean : if this keyword is set then only the ocean are masked
;	         else by default ocean + antartic are masked..
;
;	/entier : if set then there is no grid with fractionnal land-see
;		  cover (value less than 0.5 are set to 0 and value more
;		  than 0.5 to 1)
;
; OUTPUTS:
;	arr_masked : geophysical array (lon, lat, "mon") containing
;		     the value of arr but 1.e35 over the masked areas..
;       
; RESTRICTIONS:
; 
;
;========================================================================
FUNCTION peyl_mask, arr, grid_name, land=land, ocean=ocean, entier=entier

valmask=1.e35

;--------- Define the grid mask array
s=size(arr)
grid = peyl_choixgrid(grid_name, /noinfo)
if (s(1) ne grid.im or s(2) ne grid.jm) then begin
   print,'ERREUR PEYL_MASK : wrong size of array :',s
   print,'                          size of grid :',grid.im,grid.jm
   stop
endif
if s(0) eq 3 then nmon=s(3) else nmon=1
mask = fltarr(grid.im,grid.jm)
arr_mask = arr


;--------- Copie the executable fortran to compute the mask
;	   at the desired resolution
;
spawn,'\cp /home/data02/peylin/TM/cgrid/PROG/make_mask.exe .'
openw,u,'tempo',/get_lun
printf,u,strupcase(strcompress(grid_name))
free_lun,u
spawn,'make_mask.exe < tempo', temp
;;;print,temp

;--------- Read the mask  
peyl_gridread,'mask_data',mask,/f77,/noinfo

;--------- Erase the fortran files..
spawn,'\rm tempo make_mask.exe mask_data'

;--------- Build the correct mask (ocean/land) and if no fraction...
if keyword_set(land) then mask = 1. - mask $
else if (not keyword_set(ocean)) then begin
     jj = where (grid.lat lt -60.)
     mask(*,jj) = 0.     
endif

if keyword_set(entier) then begin
   mask(where(mask lt 0.5))=0.
   mask(where(mask ge 0.5))=1.
endif   

;-------- Compute the masked array
for n=0,nmon-1 do begin
    tt = arr(*,*,n) 
    ii = where(mask eq 0.)
    tt = tt * mask
    tt(ii) = valmask
    arr_mask(*,*,n) = tt
endfor

return,arr_mask
end
;-


