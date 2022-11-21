;+
;==============================================
; EXEMPLE DE PROGRAME POUR AGGREGER UN CHAMPS 2D
;         SUR UN ENSEMBLE DE REGIONS..
;=============================================
PRO MAIN

;------ Parametres 

path_in = '/home/satellites/peylin/INVERSION/ANOM2003/RECOMPOSE/'
file_in = 'fco2.LSCE.1989-2005.m.lmdz96.nc'
var_name_in = 'Poste_glo'        ;--- Name of variable / else manual choice
var_name_time = 'TIME'        ;--- Name of variable / else manual choice

grid_name = 'LMDZ96'      ;--- name of input grid !
missval = 1e+20
coef = 1.                        ;-- scaling factor


;------ Param sortie 

path_out = ''
file_out = 'out.nc'
var_name_out = 'FCO2_INV'
units_out = 'KgC/h'


;---- Defintion grille : toutes les grilles sont dans fichier:
; /home/gcarb/contrib/chgrid/defgrid.f

grid  = peyl_choixgrid(grid_name)


;---- Lecture des donnees et definition dates

peyl_readnc,data_in,file=path_in+file_in,var_name=var_name_in

                                ;--- check dim
if n_elements(data_in(*,0,0)) ne grid.nlon then message,'Prbl longitude..'
if n_elements(data_in(0,*,0)) ne grid.nlat then message,'Prbl latitude..'

                                ;--- Scaling
foo = where(data_in eq missval, cc)
data_in = data_in * coef
if cc gt 0 then data_in(foo) = missval

                                ;--- gestion des dates..
peyl_readnc,date_in,file=path_in+file_in,var_name=var_name_time
ntime = n_elements(date_in(*))


;-------------------------------------------------------------
;             Definition des regions et lecture donnees
;   EXEMPLE DANS LE CAS STANDARD
;-------------------------------------------------------------

tab_regions = [$
                ['std_sam_sud+std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_nam_des+std_nam_crop+std_nam_florid+std_nam_for_e+std_nam_for_o+std_nam_for_n+std_nam_tun+std_safr_des+std_safr_e+std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_aus_des+std_aus_for+std_eur_med+std_eur_o+std_eur_c+std_eur_e+std_eur_balk+std_eur_nordo+std_eur_norde+std_eur_turc+std_eqas_o+std_eqas_e+std_nas_inde+std_nas_viet+std_nas_chines+std_nas_chinen+std_nas_japon+std_nas_des+std_nas_steps+std_nas_stepo+std_nas_stepe+std_nas_stepn+std_nas_sibe+std_nas_sibc+std_nas_sibn+std_nas_sibo+std_nas_tun+std_satl_aus+std_satl_mid+std_satl_sub+std_eqatl+std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube+std_eqind_o+std_eqind_e+std_medit','Global Total'],$
                ['std_sam_sud+std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_nam_des+std_nam_crop+std_nam_florid+std_nam_for_e+std_nam_for_o+std_nam_for_n+std_nam_tun+std_safr_des+std_safr_e+std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_aus_des+std_aus_for+std_eur_med+std_eur_o+std_eur_c+std_eur_e+std_eur_balk+std_eur_nordo+std_eur_norde+std_eur_turc+std_eqas_o+std_eqas_e+std_nas_inde+std_nas_viet+std_nas_chines+std_nas_chinen+std_nas_japon+std_nas_des+std_nas_steps+std_nas_stepo+std_nas_stepe+std_nas_stepn+std_nas_sibe+std_nas_sibc+std_nas_sibn+std_nas_sibo+std_nas_tun','Total land'],$
                ['std_satl_aus+std_satl_mid+std_satl_sub+std_eqatl+std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube+std_eqind_o+std_eqind_e+std_medit','Total ocean'],$
;--------
                ['std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid+std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_turc+std_eur_e+std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_japon+std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit','North Hemis total'],$
                ['std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_eqas_o+std_eqas_e+std_eqatl+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_eqind_o+std_eqind_e','Tropical total'],$
                ['std_sam_sud+std_safr_des+std_safr_e+std_safr_madag+std_aus_des+std_aus_for+std_satl_aus+std_satl_mid+std_satl_sub+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube','Southern total'],$
;--------
                ['std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid+std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_turc+std_eur_e+std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_japon+std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit','North Hemis total'],$
                ['std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid+std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_turc+std_eur_e+std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_japon','North Hemis land'],$
                ['std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit','North Hemis ocean'],$
;--------
;                ['std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_eqas_o+std_eqas_e+std_eqatl+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_eqind_o+std_eqind_e','Tropical total'],$
;                ['std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_safr_sav+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_eqas_o+std_eqas_e','Tropical land'],$
;                ['std_eqatl+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_eqind_o+std_eqind_e','Tropical ocean'],$
;--------
;                ['std_sam_sud+std_safr_des+std_safr_e+std_safr_madag+std_aus_des+std_aus_for+std_satl_aus+std_satl_mid+std_satl_sub+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube','Southern total'],$
;                ['std_sam_sud+std_safr_des+std_safr_e+std_safr_madag+std_aus_des+std_aus_for','Southern land'],$
;                ['std_satl_aus+std_satl_mid+std_satl_sub+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube','Southern ocean'],$
;--------
                ['std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid','North America total'],$
                ['std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_turc+std_eur_e','Total Europe'],$
                ['std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_japon','North Asia total'],$
;--------
                ['std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde','North Atlan ocean'],$
                ['std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit','North Pacific ocean'],$
                ['std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit','North Pacific ocean'],$
;--------
;                ['std_sam_sav+std_sam_for+std_sam_andes+std_eqam','Tropical south america'],$
;                ['std_safr_sav+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab','Tropical africa'],$
;                ['std_eqas_o+std_eqas_e','Tropical asia'],$
;--------
                ['std_eqatl','Tropical atlantic ocean'],$
                ['std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde','Tropical pacific ocean'],$
                ['std_eqind_o+std_eqind_e','Tropical indian ocean'],$
;
;--------
                ['std_spac_ause+std_satl_aus+std_sind_aus+std_spac_auso','Austral ocean'],$
                ['std_spac_subo+std_spac_sube+std_satl_sub+std_sind_subo+std_sind_sube','Sub tropical ocean'],$
                ['std_spac_mido+std_spac_mide+std_satl_mid+std_sind_mido+std_sind_mide','Sub antartic ocean'],$
;--------
                ['std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop','North America boreal'],$
                ['std_eur_nordo+std_eur_norde','North Europe'],$
                ['std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc','North Boreal Asia'],$
;--------
                ['std_nam_des+std_nam_for_e+std_nam_florid','North America temperate'],$
                ['std_eur_o+std_eur_c+std_eur_e','Central mid Europe'],$
                ['std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_japon','North mid Asia'],$
;--------
                ['ce_north','north Europe'],$
                ['ce_west','west Europe'],$
                ['ce_central','central Europe'],$
;--------
                ['ce_west+ce_central','west + central Europe'],$
                ['ce_east','east Europe'],$
                ['ce_south','south Europe'],$
                ['c_fra','France']$
              ]
nreg = n_elements(tab_regions(0,*))


DATA_TOT = fltarr(nreg,ntime)

;----- Lecture des differentes regions

FOR nr=0,nreg-1 DO BEGIN

    ind = peyl_choixregnew(tab_regions(0,nr), grid_name=grid_name, $
                           noinfo=1, extend_reg=0,$
                           grid_pixel=grid_pixel,fraction=use_fraction)

    ;--- calcul du total par region
    for nt=0,ntime-1 do begin
        temp = data_in(*,*,nt)
        data_tot(nr,nt) = total(temp(ind.ii2d) * grid.dxyp(ind.ii2d))
    endfor

ENDFOR


;------ Sauvegarde au format Netcdf

dim_val = {d0:indgen(nreg),d1:date_in}
dim_size = [nreg,ntime]

                                ;--- A modifier
dim_name  = ['regions','TIME']
dim_title = ['regions','TIME']
dim_units = ['index','date']
;dim_unlimited = 'XXX' ;--- name of the unlimited dim if wanted
add_dim = [0,0]               ;--- 1 to add the dim values as variables
    
var_name  = ['regions_name',var_name_out]
var_units = ['string',units_out]
var_dim = {V0:[0],V1:[0,1]}

peyl_writenc,reform(tab_regions(1,*)),data_tot,file=path_out+file_out,$
  var_dim=var_dim,var_name=var_name,var_title=var_title,var_units=var_units,$
  dim_size=dim_size,dim_val=dim_val,dim_name=dim_name,$
  dim_title=dim_title,dim_units=dim_units,$
  var_misval=missval,add_dim=add_dim,dim_unlimited=dim_unlimited


stop
END
;-


