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
grid = peyl_choixgrid('LMDZ96')

;--- Define fluxes..

;path_flux = '/home/satellites/peylin/INVERSION/ANOM2003/RECOMPOSE/REF/'
;file = 'fco2.inv_ref.m.1986-2004.lmdz96.nc'

path_flux = '/home/satellites/bousquet/TRANSCOM/T3L3/LMDZ/'
file = 'fco2.casa.nep.glo.fy00.m.lmdz96.nc'
;file = 'fco2.sib.nep.glo.fy00-03.m.lmdz96.nc'
;file = 'fco2.sib.nep.glo.fy02.j.lmdz96.2002.nc'
;file = 'fco2.sib.nep.glo.fy03.j.lmdz96.2003.nc'
;file = 'fco2.taka_trcom.oce.glo.fy95.m.lmdz96.nc'
;file = 'fco2.trcom.fos.glo.fy98.m.lmdz96.nc'
;file = 'fsf6.trcom.sf6.glo.fy99-03.m.lmdz96.nc'

var_name = ['Poste_glo','casa_mon']

peyl_readnc,file=path_flux+file,flux,var_name=var_name
flux = flux(0:grid.nlon-1,*,*)
flux_north2south = 1

;--- Define time of fluxes
peyl_readnc,file=path_flux+file,date,var_name=['TIME','time','date']
ntime = n_elements(date)
flux_date = peyl_date2date(decdate=date,type_out='date')

;--- To have KgC/h (from KgC/m2/h)
for n=0,ntime-1 do flux(*,*,n) = flux(*,*,n)*grid.dxyp

;--- Period of emission 
flux_need_start = [2000,1,1]
flux_need_end = [2002,12,31]
                                
;--- Limit flux period 
flag_clim = 0
foo = where(flux_date(0,*) ge (flux_need_start(0)-1) and $
            flux_date(0,*) le (flux_need_end(0)+1), cc)
if cc gt 0 then begin
    flux = flux(*,*,foo)
    flux_date = flux_date(*,foo)
endif else begin
    print,'Cas clim obligatoire...'
    flag_clim = 1
endelse


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
obs_start = [2002,1,1]
obs_end = [2002,12,31]

save_file = 'ex_transport.nc'


;----------------------------------------------------------------------
;   Appel au programme
;----------------------------------------------------------------------

info = 0
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


