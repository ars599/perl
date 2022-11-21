;+
;=========================================================================
; NAME:
;       PEYL_GETPOS
;
; PURPOSE:
;       function to retreive the indices for longitude and latitude
;	of some points (defined by lon and lat) in a given grid.
;
; CALLING SEQUENCE:
;       result = peyl_getpos (grid_name, lon ,lat)
;
; INPUTS:
;	grid_name : the name of the grid to use
;	lon : array of longitudes in degre -180 < xx <= 180
;	lat : array of latitudes in degre  -90 < yy <= 90
;
; OPTIONAL INPUT PARAMETERS:
;	grid : optional struture that contain the longitude and 
;              latitudes and delta long delta latitudes of the grid 
;              (grid.lon, grid.lat, grid.dlon, grid.dlat)
;
; OUTPUTS:
;	result = structure containing :
;	       { lon : array of indices of the input longitudes 
;	         lat : array of indices of the input latitudes 
;	         alon : array of fractional indices of the input longitudes 
;	         alat : array of fractional indices of the input latitudes 
;	       }
;       
; RESTRICTIONS:
;	lon and lat must have the same size..
;
;=========================================================================

FUNCTION peyl_getpos, grid_name, lon, lat,$
                      grid = grid, no_interpol=no_interpol

if not keyword_set(no_interpol) then no_interpol=0

;--- Retreive the grid.
if not keyword_set(grid) then $
  grid = peyl_choixgrid(grid_name,/noinfo) 

;--- Some checks.
nlon = n_elements(lon)
nlat = n_elements(lat)
if nlon ne nlat then begin
   print,'ERREUR peyl_getpos : wrong dim of lon and lat ::',nlon,nlat
   stop
endif

;--- Reset lon between -180 and 180
; ii = where (lon gt 180., cc)
; if cc gt 0 then lon(ii)=lon(ii)-180.
; ii = where (lon lt -180., cc)
; if cc gt 0 then lon(ii)=lon(ii)+180.

;---- Check latitudes
ii = where(lat gt 90 or lat lt -90, cc)
if cc gt 0 then message,'Erreur peyl_getpos : lat outside [-90,90]'


;----- Define the output array
if no_interpol then $
  aout = { lon:intarr(nlon), lat:intarr(nlat)} $
else aout = { lon:intarr(nlon), lat:intarr(nlat), $
              alon:fltarr(nlon), alat:fltarr(nlat) } 
  
;----- Find the specific indices
; REM : IF MANY INDICES TAKE THE SOUTHERN OR WESTERN ONE...
;
for i=0L,nlon-1 do begin
   aout.lon(i)=(where(abs(grid.lon(*)-lon(i)) le (max(grid.dlon)/2.)))(0)
   aout.lat(i)=(where(abs(grid.lat(*)-lat(i)) le (max(grid.dlat)/2.)))(0)
   if (aout.lon(i) eq -1 and lon(i) gt 0.) then aout.lon(i)=n_elements(grid.lon)-1
   if (aout.lon(i) eq -1 and lon(i) lt 0.) then aout.lon(i)=0
endfor

;----- Find the interpolated indices (float)
;
if no_interpol eq 0 then begin
    peyl_index, grid.lon, lon, aa
    aout.alon = aa
    peyl_index, grid.lat, lat, aa
    aout.alat = aa
endif


return,aout
END
;-



