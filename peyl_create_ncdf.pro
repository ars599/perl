;+
;==================================================================
; NAME:
;       PEYL_CREATE_NETCDF
;
; PURPOSE:
;       
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
; RESTRICTIONS:
; 
;
;==================================================================
PRO PEYL_CREATE_NETCDF, data,$
                        file = file,$
                        grid_name = grid_name,$
                        lon = lon,$
                        lat = lat,$
                        

if not(keyword_set(file)) then file = 'file.ncdf'

grid = peyl_choixgrid('TM75',/noinfo)
lev_number=[1,2,3,4,5,6,7,8,9]
lev_alt = [0.2,.8,1.9,3.6,5.9,8.6,11.7,15.4,20.6]

;--- open file

ncdf_id = ncdf_create(file_name,/clobber)

;--- create dimensions

lat_dim_id = ncdf_dimdef(ncdf_id,'lat',nlat) 
lon_dim_id = ncdf_dimdef(ncdf_id,'lon',nlon) 
lev_dim_id = ncdf_dimdef(ncdf_id,'lev',nlev)
time_dim_id = ncdf_dimdef(ncdf_id,'time',nmon)

;---- create variables for dimensions

lat_id=ncdf_vardef(ncdf_id,'lat',[lat_dim_id],/float)
lon_id=ncdf_vardef(ncdf_id,'lon',[lon_dim_id],/float)
lev_id=ncdf_vardef(ncdf_id,'lev',[lev_dim_id],/float)
time_id=ncdf_vardef(ncdf_id,'time',[time_dim_id],/float)

;--- fin de creation de l'entete
ncdf_control,ncdf_id,/endef


ncdf_varput,ncdf_id,lat_id,grid.lat
ncdf_varput,ncdf_id,lon_id,grid.lon
ncdf_varput,ncdf_id,lev_id,lev_alt
ncdf_varput,ncdf_id,time_id,time

for g=0,nb_component-1 do begin
    ncdf_control,ncdf_id,/redef
    var_name = data_type+'_'+component
    variable_id = ncdf_vardef(ncdf_id,var_name, $
                   [lon_dim_id,lat_dim_id,lev_dim_id,time_dim_id],/float)
    ncdf_control,ncdf_id,/endef
    data = reform(local_data(g).val,[nlon,nlat,nlev,nmon*nb_year_model])
    ncdf_varput,ncdf_id,variable_id,data
endfor

;ncdf_control,ncdf_id,/redef
;ncdf_attput,ncdf_id,'case','Henning ',/GLOBAL
;ncdf_attput,ncdf_id,'lat',lat,/GLOBAL


END
;-

