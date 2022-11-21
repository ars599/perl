;+
; NAME:
;       PEYL_GLOMEAN
;
; PURPOSE:
;       Calculate the global and zonal mean and the total for
;	a geophysical field (quantity/m2) 
;	the dimension are (lon, lat, "nmon")
;	If there is 3 dimensions then the statistiques are calculated
;	for each month (nmon).
;
; CALLING SEQUENCE:
;       result = peyl_glomean (arr, grid_name=grid_name, dxyp=dxyp,/mask_data,
;			       valmask=valmask, scale=scale, /noinfo )
;
;	example : the equivalent of zmean.pro is a call like :
;		  peyl_glomean (arr, /mask_data, valmask=??)		       
;
; INPUTS:
;       arr : array to process (dim = 1, 2 or 3)
;	
;
; OPTIONAL INPUT PARAMETERS:
;       grid_name : name of the grid to use : if not defined
;		    then the surface of each pixel is calculated
;		    for a regular grid or taken from the dxyp (input).
;
;       dxyp : Surface in m2 of each elements of arr.
;	       nescessary if arr is not a geophysical field (lon, lat, "mon")
;	       Must be the same size than arr.
;
;	/mask_data : set to mask the data equal to valmask.
;
;	valmask : value of the masked data : if set then take for statistiques
;		  only the data differents than valmask.
;		  Only active if mask_data set.
;
;	scale : a scaling factor applied to arr.
;	 
;      /noinfo : to suppress messages.
;
; OUTPUTS:
;       result = structure containing :
;	       { 
;		 surf(nmon) : the total surface used without taking
;			      the pixels where data = valmask (en m2)
;		 tot(nmon) : the total data weighted by surface in m2
;		 mean(nmon): mean global data weighted by the surface 
;		 totlat(nlat,nmon) : total latitudinal data 
;				     weighted by the surface
;		 meanlat(nlat,nmon): mean latitudinal data.
;		 lat(nlat): the latitude (center) of the latitude-band used
;			    that correspond to meanlat or totlat...
;		}
;
;	If arr has only 2 dimension then nmon=1...
;      
; RESTRICTIONS:
; 
;
;==================================================================

FUNCTION PEYL_GLOMEAN, arr, dxyp=dxyp, grid_name=grid_name, $
		      valmask=valmask, mask_data=mask_data, $
		      scale=scale, noinfo=noinfo

if keyword_set(noinfo) then info=0 else info=1
if keyword_set(mask_data) then mask_data=1 else mask_data=0

;---- Extract the dimension of the field..
s=size(arr)
if s(0) gt 3 then begin
   print,'ERREUR GLOMEAN : wrong size of data.... : ',s & stop
endif
if ((s(0) lt 2) and (not keyword_set(dxyp))) then begin
   print,'ERREUR GLOMEAN : size of data <2 and no surface given..',s &stop
endif
im=1 & jm=1 & nm=1
if (s(0) ge 3) then nm=s(3)
if (s(0) ge 2) then jm=s(2)
if (s(0) ge 1) then im=s(1)

if info then print,'Global mean calculation : DATA-SIZE=',s


;--- Define the surface of the grid 
; dxyp = surface in m2
;
if (keyword_set(dxyp)) then begin
   s_dxyp=size(dxyp)
   if info then print,'Global mean calculation : GRID-SIZE=',s_dxyp
   if (s_dxyp(0) gt 2) then begin
      print,'ERREUR GLOMEAN : wrong size of dxyp : ',s_dxyp & stop
   endif 
   if ((s_dxyp(0) eq 2)and(s_dxyp(1) ne im or s_dxyp(2) ne jm)) then begin
      print,'ERREUR GLOMEAN : wrong size of dxyp / data: ',s_dxyp,s & stop
   endif
   if s_dxyp(1) ne im then begin
      print,'ERREUR GLOMEAN : wrong size of dxyp / data: ',s_dxyp,s & stop
   endif
   lat=fltarr(jm)
endif else begin
	   ;--- pre-defined grid..
   if (keyword_set(grid_name)) then begin
      if info then grid = peyl_choixgrid (grid_name) $
         else grid = peyl_choixgrid (grid_name, /noinfo)
      if (grid.nlon ne im or grid.nlat ne jm) then begin
	 print,'ERREUR GLOMEAN :',grid_name,' give different dimensions',$
		' than the data ... :',grid.nlon,grid.nlat,im,jm
	 stop
      endif 
      dxyp=grid.dxyp
      lat =grid.lat
	   ;--- Define a regular grid..
   endif else begin
      Rterre=6375.e3 ; en m
      dxyp=dblarr(im,jm)
      lat=fltarr(jm)
      dlat=!pi/jm
      dlon=2.*!pi/im
      for j=0,jm-1 do begin
	  lat(j)=!pi/2.*(-1+2./jm*(j+0.5))
	  dxyp(*,j)=Rterre^2*dlon*dlat*cos(lat(j))
	  lat(j)=lat(j)*180./!pi    ;---- for output
      endfor
   endelse
endelse


;---- Scale data...
local_data = arr
if (keyword_set(scale)) then local_data = local_data*scale

;---- Define the aoutput
;
if s(0) le 2 then begin
   x1=0. & x2=fltarr(jm)
endif else begin
   x1=fltarr(nm) & x2=fltarr(jm,nm)
endelse
stat={   surf      :x1 ,$
         tot       :x1 ,$
         mean	   :x1 ,$
         totlat	   :x2 ,$
         meanlat   :x2 ,$
	 lat       :lat $
        }  

;---- Mean calculations..
;
if (not keyword_set(dxyp)) then stat.lat=lat
max_value=1.e36
if mask_data then max_value=valmask

if s(0) le 2 then begin
   if mask_data then begin
      ii=where(arr eq valmask, cc)
      if cc gt 0 then dxyp(ii)=0.
   endif
   stat.surf = total(dxyp) 
   local_data = local_data * dxyp
   stat.tot  = total( local_data)
   stat.mean  = peyl_division(stat.tot, stat.surf, max_value)
   if s(0) eq 2 then begin
      stat.totlat  = total(local_data, 1)
      stat.meanlat  = peyl_division(stat.totlat, total(dxyp,1), max_value)
      ii=where(stat.meanlat eq max_value,cc)
      if cc gt 0 then stat.totlat(ii)=max_value
   endif

		;--- calculate 3d field..
endif else begin
   glb_dxyp = fltarr(im,jm,nm)
   for n=0,nm-1 do glb_dxyp(*,*,n)=dxyp
   if mask_data then begin
      ii=where(arr eq valmask, cc)
      if cc gt 0 then glb_dxyp(ii)=0.
   endif
   stat.surf = total(total(glb_dxyp,1),1) 
   glb_data = local_data*glb_dxyp
		 ;---- calculate the values...
   stat.tot  = total(total(glb_data,1),1)

   stat.mean  = peyl_division(stat.tot, stat.surf, max_value)
   stat.totlat  = total(glb_data,1)
   stat.meanlat  = peyl_division(stat.totlat, total(glb_dxyp,1), max_value)
   ii=where(stat.meanlat eq max_value,cc)
   if cc gt 0 then stat.totlat(ii)=max_value
endelse

stat.surf = stat.surf ;;;;;;*1.e-6   ;--- to get km2
return,stat
END
;-







