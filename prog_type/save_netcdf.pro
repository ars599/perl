PRO MAIN

;---- Parametres generaux
path_in = '/home/satellites/peylin/COCO2/'
path_out = '/home/satellites/delage/PROJETS/FOSSILS/NC_FILE/' 
file_out = 'toto.nc'
;--
nval_per_day = 24
ntime_out = 2

;------------------------
; READ DATA
;------------------------

file_in = 'lmdz_96x72x19.coco2.histrac.an1990.m'
peyl_sread,DATA,file=path_in+file_in,nl=nl

npoints = nl
if npoints ne 8100 then stop
nveget = 4
ntime = 24

lon = reform(DATA(0,*))
lat = reform(DATA(1,*))
veget_type = fltarr(npoints,nveget)
veget_type(*,0) = reform(DATA(2,*))
veget_type(*,1) = reform(DATA(3,*))
veget_type(*,2) = reform(DATA(4,*))
veget_type(*,3) = reform(DATA(5,*))
zone = fltarr(npoints,ntime)
for i=0,ntime-1 do begin
    zone(*,i) = reform(DATA(6+i,*))
endfor

;------------------------
;  SAUVEGARDE NETCDF
;------------------------

 time = indgen(ntime)+1

 dim_size = [ntime,npoints,nveget]
 dim_val = {D0:time, D1:' ', D3:' '}
 dim_name  = ['TIME','NB','VEGET']
 dim_title = ['Time since','Number of point','Veget type']
 dim_units = ['hour','-','-']

 var_name  = ['LON','LAT','VEGET_TYPE','ZONE']
 var_title = ['','','','']
 var_units = ['','','','']
 var_dim = {V0:[1],V1:[1],V2:[1,2],V3:[0,2]}

 fff = path_out + file_out
 peyl_writenc,lon,lat,veget_type,zone,file=fff,var_dim=var_dim,var_name=var_name,$
   var_title=var_title,var_units=var_units,dim_size=dim_size,dim_val=dim_val,$
   dim_name=dim_name,dim_title=dim_title,dim_units=dim_units,var_misval=missval

stop
end
