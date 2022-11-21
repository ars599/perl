;+
; NAME:
;       PEYL_choixgrid
;
; PURPOSE:
;       to get the caracteristique of a grid given its grid_name
;	read the file /flurry/home/fdelage/LIB/idl(/home/gcarb/contrib/chgrid/grid_definition)
;
; CALLING SEQUENCE:
;       grid = peyl_choixgrid (name,noinfo=noinfo)
;
; INPUTS:
;     grid_name : the name of the grid.
;
; OPTIONAL INPUT PARAMETERS:
;     noinfo : for no information print out..
;
; OUTPUTS:
;       grid : structure with the following elements : result : a/b
;	     {  im : number of lon    (0 if grid not found...)
;		jm : number of lat    (0 if grid not found...)
;		lon(im) : center longitude     -180 < xx =< 180 
;		lat(jm) : center latitude      -90 < yy =< 90
;		dxyp(im,jm) : area of the grid-cells
;		dlon(im) : increment in longitude for the im lon
;		dlat(jm) : increment in latitude for the jm lat
;               s_lat_0: south border of the first latitude 
;               w_lon_0: west border of the first longitude 
;            }
;	 all value are in degre..
;
;=====================================================================

FUNCTION PEYL_CHOIXGRID, grid_name, noinfo=noinfo

if keyword_set(noinfo) then info=0 else info=1

;--- Open the grid file..
file_defgrid='/flurry/home/fdelage/LIB/idl/grid_definition'
openr,unit,file_defgrid,/get_lun

;--- Find the specific grid..
ngrid=0 & im=0 & jm=0
name = ' ' & s_lat_0=0. & w_lon_0=0.
buf_name = strarr(200)
grid_name2 = STRUPCASE(STRCOMPRESS(grid_name,/remove_all))
readf,unit,ngrid

for n=0,ngrid-1 do begin
   readf,unit,name
   name=STRUPCASE(STRCOMPRESS(name,/remove_all))
   readf,unit,im,jm
   if (im le 0 or jm le 0) then begin
      print,'CHOIX_GRID : Erreur dans les longitudes et latitudes... : ',im,jm
      stop
   endif
   dlat=fltarr(jm)
   dlon=fltarr(im)
   readf,unit,dlat
   readf,unit,dlon
   readf,unit,s_lat_0,w_lon_0
   buf_name(n)=name+'('+auto_string(im,0)+','+auto_string(jm,0)+')'
   if (name eq grid_name2) then begin
      if info then print,'Grid ',name,' found ; dim = ',auto_string([im,jm],0)
      free_lun,unit
      goto,suite
   endif
endfor 
print,'ERREUR PEYL_CHOIXGRID : Grid ',grid_name2,' not found from FILE :'
print,file_defgrid
print,'Grid available : ',buf_name(0:ngrid-1)
free_lun,unit
return,{name:grid_name, im:0, jm:0}

;-----------------------------------------------------------------
; Calculate the boundary of the grid..
;
suite:

;--- ae: radius of earth
ae=6.371229d6
d2r=!pi/180.

;--- longitudes of eastern edges of G boxes
e_lon=fltarr(im)
e_lon(0)=w_lon_0+dlon(0)
for i=1,im-1 do e_lon(i)=e_lon(i-1)+dlon(i)

;--- latitudes of northern edges of G boxes
n_lat=fltarr(jm)
n_lat(0)=s_lat_0+dlat(0)
for j=1,jm-1 do n_lat(j)=n_lat(j-1)+dlat(j)

;--- Put all longitudes in the same interval of values -180 < .. <= +180

; indice = where (e_lon-dlon/2. gt 180., count)
; if count ne 0 then begin
;    e_lon(indice)=e_lon(indice) - 360.
;    if info then print,'Reajustement des lon > 180 pour i :',indice
; endif

;--------------------------------------------------------------------
; compute area of G boxe in m**2
; REM: lat and long are in degre..

dxyp=fltarr(im,jm)
for i=0,im-1 do begin
   for j=0,jm-1 do begin
       dd=2.*(ae*ae)*dlon(i)*d2r*sin(0.5*dlat(j)*d2r)
       dxyp(i,j)=dd*cos((n_lat(j)-0.5*dlat(j))*d2r)
   endfor
endfor

;--------------------------------------------------------------------
; Set the output (grid)


grid={ name:grid_name,$
       nlon:im ,$ 
       nlat:jm ,$ 
       dlat:dlat ,$ 
       dlon:dlon ,$ 
       lon:e_lon-dlon/2.,$ 
       lat:n_lat-dlat/2.,$ 
       dxyp:dxyp,$ 
       s_lat_0:s_lat_0 ,$ 
       w_lon_0:w_lon_0 $ 
      }

return,grid
END
;-
