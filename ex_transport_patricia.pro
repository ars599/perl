;+
;======================================================================
;
;     EXEMPLE DE PROGRAME POUR UTILISER LE TRANSPORT OFFLINE
;       (simulation T3L3 pour comparaison)
;
;======================================================================
@peyl_transport
@peyl_chtime
pro test

;----------------------------------------------------------------------
;    Definition du scenario de Flux
;----------------------------------------------------------------------

;--- Define grid to work with
grid_name = 'LMDZ96'
grid = peyl_choixgrid(grid_name)

;--- Define fluxes..

path_flux = '/home/loop/pcadule/LOOP02_FLUXES/INPUT/'
file = 'LOOP02_NEW.nc'

var_name = ['CO2FLUX_MONTHLY_LOOP']

peyl_readnc,file=path_flux+file,flux_orig,var_name=var_name
flux_orig = flux_orig(0:grid.nlon-1,*,*)
flux_north2south = 0

;--- Define time of fluxes
peyl_readnc,file=path_flux+file,date,var_name=['TAXIS']
ntime = n_elements(date)
flux_decdate = 2000.+(findgen(ntime)+0.5)/12.
flux_date = peyl_date2date(decdate=flux_decdate,type_out='date')

;--- To have KgC/h (from gC/m2/m)
for n=0,ntime-1 do flux_orig(*,*,n) = flux_orig(*,*,n)*grid.dxyp*1.e-3/(365./12.)/24.


;----------------------------------------------------------------------
;    Choix des Observations
;----------------------------------------------------------------------
                                
;--- Liste of site
;obs_name = ['MLO','SPO','SCH']
;obs_name = ['ALL']
obs_name = ['MLO']
                                
;--- Type of simul
type_simul = 'month_lmdz96'
                                
;--- Period of observations
obs_start = [2000,6,1]
obs_end = [2000,12,31]

ntime_obs = (obs_end(0)-obs_start(0))*12 + obs_end(1)-obs_start(1)+1

;----------------------------------------------------------------------
;   Choix d'une region et periode
;----------------------------------------------------------------------

;----- Definition des regions et periodes + tableau de sauvegarde
;/home/satellites/peylin/REGIONALISATION/regions_std
regions = [$
;            ['std_north_land','North Hemis land']$
            ['std_nam_crop+std_nam_florid+std_nam_for_e+std_nam_for_o','Amerique Nord (usa)']$
;            ['std_nam_for_n','Amerique Nord (canada)'] $
;           ['',''] $
           ]
nregions = n_elements(regions(0,*))

nperiod = 1
nobs = n_elements(obs_name)

data_save = fltarr(nobs,ntime_obs,nregions,nperiod)


;----- Limit the regions

FOR ireg=0,nregions-1 DO BEGIN

    ind = peyl_choixregnew(regions(0,ireg), grid_name = grid_name, $
                           box_coord = box_coord,noinfo = noinfo, $
                           extend_reg = 0)

    flux_reg = flux_orig
    for t=0,ntime-1 do begin
        temp = flux_orig(*,*,t)
        temp(ind.ii2d_inv) = 0.
        flux_reg(*,*,t) = temp
    endfor


;----- Limite period of injection flux

    FOR iperiod=0,nperiod-1 DO BEGIN

;--- Period of emission (ANNEE/MOIS REEL/JOUR) (!commencement a 1)
        if iperiod eq 0 then begin
            flux_need_start = [2000,6,1]
            flux_need_end = [2000,8,31]
        endif
        
;--- Limit flux period 
        flux = flux_reg
        flag_clim = 0
        foo = where(flux_date(0,*) ge (flux_need_start(0)-1) and $
                    flux_date(0,*) le (flux_need_end(0)+1), cc)
        if cc gt 0 then begin
            flux = flux_reg(*,*,foo)
            flux_date = flux_date(*,foo)
        endif else begin
            print,'Cas clim obligatoire...'
            flag_clim = 1
        endelse


;----------------------------------------------------------------------
;   Appel au programme et sauvegarde..
;----------------------------------------------------------------------

        save_file = 'ex_transport.nc'
        info = 1
        out = PEYL_TRANSPORT(obs_name, obs_start, obs_end, $
                             flux, flux_date, $
                             flux_north2south = flux_north2south,$
                             flux_need_start = flux_need_start, $
                             flux_need_end = flux_need_end, $
                             flux_used_val = flux_used, $
                             flux_used_date = flux_used_date, $
                             type_simul = type_simul, $
                             flag_clim = flag_clim, $
                             plot_retro = 0, $
                             no_leap = 1, $
                             interpol = interpol, $
                             save_file = save_file, $
                             info = info)

;----- Sauvegarde des resultats

        data_save(*,*,ireg,iperiod) = out.conc(*,*)

    ENDFOR
ENDFOR



;----------------------------------------------------------------------
;         Plots
;----------------------------------------------------------------------

ccg_opendev,dev='psc',saveas='ex_transport.ps'
!p.multi = 0
for n=0,out.nobs-1 do begin
    title = out.name(n)
    decdate = peyl_date2date(juldate=out.juldate,type_out='decdate')
    plot,decdate,out.conc(n,*),title=title
;    if peyl_pause() then stop
endfor
ccg_closedev,dev='psc'

stop
end
;-


