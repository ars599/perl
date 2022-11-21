;+
;==============================================
; EXEMPLE DE PROGRAME POUR CHANGER DE GRILLE
;=============================================
PRO MAIN

;------ Param entres

path_in = '/home/satellites/peylin/INVERSION/ANOM2003/RECOMPOSE/'
file_in = 'fco2.LSCE.1989-2005.m.lmdz96.nc'
var_name_in = 'Poste_glo'        ;--- Name of variable / else manual choice
var_name_time = 'TIME'        ;--- Name of variable / else manual choice

grid_name_in = 'LMDZ96'      ;--- name of input grid !
missval = 1e+20
NS_inverse_in = 0               ;-- 1 si donne entree sont du Nord vers Sud
coef = 1.                        ;-- scaling factor

;------ Param sortie + Variable special LMDZ : extra_lon to add an
;       additionnal longitude; NS_inv to invert to North -> South the
;       data

path_out = ''
file_out = 'out.nc'
var_name_out = var_name_in
grid_name_out = 'mask11'
lmdz_extra_lon_out = 0          ;--- Special lmdz pour l'extra-longitude
NS_inverse_out = 0              ;--- 1 pour avoir du N vers S
units_out = 'xxx'

;---- Defintion des grilles : toutes les grilles sont dans fichier:
; /home/gcarb/contrib/chgrid/defgrid.f

grid_in  = peyl_choixgrid(grid_name_in)
grid_out = peyl_choixgrid(grid_name_out)

stop
;---- Lecture des donnees et definition dates

peyl_readnc,data_in,file=path_in+file_in,var_name=var_name_in

                                ;--- check dim
if n_elements(data_in(*,0,0)) ne grid_in.nlon then message,'Prbl longitude..'
if n_elements(data_in(0,*,0)) ne grid_in.nlat then message,'Prbl latitude..'

                                ;--- Scaling
foo = where(data_in eq missval, cc)
data_in = data_in * coef
if cc gt 0 then data_in(foo) = missval

                                ;--- gestion des dates..
peyl_readnc,date,file=path_in+file_in,var_name=var_name_time
ntime = n_elements(date)
if n_elements(data_in(0,0,*)) ne ntime then message,'probleme time...'

;----- Changement de grille (by surface overlaps; account for valmask..)

data_out = PEYL_CHGRID(data_in,grid_in,grid_out,$
                       itypedata=itypedata, valmask=valmask, fraction=fr,$
                       lmdz_extra_lon_out = lmdz_extra_lon_out,$
                       NS_inverse_in  = NS_inverse_in,$
                       NS_inverse_out = NS_inverse_out,$
                       info = info )

;------ Sauvegarde au format Netcdf

lon_out = grid_out.lon
lat_out = grid_out.lat
if lmdz_extra_lon_out then lon_out = [lon_out,lon_out(0)+360]
if NS_inverse_out then lat_out = reverse(lat_out)
dim_val = {d0:lon_out,d1:lat_out,d2:date}
dim_size = [grid_out.nlon+lmdz_extra_lon_out,grid_out.nlat,ntime]

                                ;--- A modifier
dim_name  = ['longitude','latitude','TIME']
dim_title = ['longitude','latitude','TIME']
dim_units = ['degrees_east','degrees_north','date']
;dim_unlimited = 'XXX' ;--- name of the unlimited dim if wanted
add_dim = [0,0,0]               ;--- 1 to add the dim values as variables
    
var_name  = [var_name_out]
var_units = [units_out]
var_dim = {V0:[0,1,2]}

peyl_writenc,data_out,file=path_out+file_out,$
  var_dim=var_dim,var_name=var_name,var_title=var_title,var_units=var_unit,$
  dim_size=dim_size,dim_val=dim_val,dim_name=dim_name,$
  dim_title=dim_title,dim_units=dim_units,$
  var_misval=missval,add_dim=add_dim,dim_unlimited=dim_unlimited


stop
END
;-


