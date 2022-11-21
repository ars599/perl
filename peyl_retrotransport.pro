;+
;========================================================================
; NAME:
;       PEYL_TRANSPORT
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
;       
; RESTRICTIONS:
; 
;
;========================================================================
FUNCTION PEYL_RETROTRANSPORT, obs_name, obs_start, obs_end, $
                              type_simul = type_simul, $
                              plot_retro = plot_retro, $
                              no_leap = no_leap, $
                              interpol = interpol, $
                              save_file = save_file, $                        
                              info = info


journal,'journal_peyl_retrotransport.txt'

;-----------------------------------------------------------------
; Check inputs
;-----------------------------------------------------------------

if n_elements(info) eq 0 then info = 0
if n_elements(save_file) eq 0 then save_file = ''
if n_elements(no_leap) eq 0 then no_leap = 1
if n_elements(type_simul) eq 0 then type_simul = 'month_lmdz96'
if n_elements(flag_clim) eq 0 then flag_clim = 0
if n_elements(plot_retro) eq 0 then plot_retro = 0

if n_elements(obs_start) ne 3 then message,'probleme obs_start'
if n_elements(obs_end) ne 3 then message,'probleme obs_end'

if n_elements(flux_need_start) eq 0 then flux_need_start = flux_date(*,0) 
if n_elements(flux_need_end) eq 0 then flux_need_end = flux_date(*,n_elements(flux_date)-1) 
if n_elements(flux_north2south) eq 0 then flux_north2south = 0


;---- Check sizes of flux date
ss1 = size(flux_date)
if ss1(0) gt 2 then message,'Dimention problem with flux_date array !!'
if ss1(0) eq 2 then ntime_flux = ss1(2) else ntime_flux = 1
if ss1(1) lt 3 then message,'Error flux_date 1st dim should have at least 3 elements!'

;----- Check compatibility of flux and flux_date arrays..

ss2 = size(flux)
if ss2(0) lt 2 or ss2(0) gt 3 then message,'Dimention problem with flux array!!'
if ss2(0) eq 2 then nt = 1 else nt = ss2(3)
if nt ne ntime_flux then message,'Error flux_date not compatible with flux array!'


;---- File to check the retro plumes extrapolation
if plot_retro then begin
    ccg_opendev,dev='psc',saveas='retro_transport.ps',portrait=0
    loadct,39
endif

;-----------------------------------------------------------------
; Define general variables  
;-----------------------------------------------------------------

;---- Define variables specific of each simulation type

case strlowcase(type_simul) of 
    'month_lmdz96' : begin
        path_retro = ['/home/carbone/bousquet/LMDZ/RETROS/BACKRUN/',$
                      '/home/carbone/bousquet/LMDZ/RETROS/BACKRUN2/'] 
        site_info = ['/home/carbone/bousquet/LMDZ/RETROS/sites_backrun',$
                    '/home/carbone/bousquet/LMDZ/RETROS/sites_backrun2']
        prefix = ['backrun.lmdz96.histrac.','backrun2.lmdz96.histrac.']
        obs_units = 'months'
        grid_name = 'lmdz96'
        coef_retro = 1.0e20     
    end
    'month_lmdze192' : begin
        path_retro = ['/home/scratchatm/bousquet/FBASE/LMDZE192/'] 
        site_info = ['/home/carbone/bousquet/LMDZ/RETROS/sites_backrun']
        prefix = 'backrun.lmdze192.histrac.'
        obs_units = 'months'
        grid_name = 'lmdze192'
        coef_retro = 1.0e20     
    end
    'day_lmdze192' : begin
        path_retro = ['/home/satellites/carouge/RETRO_DAYTIME/']                      
        site_info = ['??']
        prefix = 'backrun.lmdze192.histrac.'
        obs_units = 'days'
        grid_name = 'lmdze192'
        coef_retro = 1.0e20     
    end
    else : begin
       print,'Error : type_simul not recognized!!'
       print,' Choice between : month_lmdz96, month_lmdze192, day '
       stop
   end
endcase
npath = n_elements(path_retro)

                                ;--- Define the grid
grid = peyl_choixgrid(grid_name)
if grid.nlon ne ss2(1) then message,'problem of nlon for flux'
if grid.nlat ne ss2(2) then message,'problem of nlat for flux'

                                ;--- Specific for the monthly retros,
                                ;    we position the output at mid month
if obs_units eq 'months' then begin
    obs_start(2) = 15
    obs_end(2) = 15
endif 

;---- For the time manadgement
flux_units = 'days'                  ;--- For timegen function
jour=[31,28,31,30,31,30,31,31,30,31,30,31]
jour_cumul = intarr(13)
for i=1,12 do jour_cumul(i) = total(jour(0:i-1))
precis = 1.e-6                  ;--- precision for julian date (at least second)


;-----------------------------------------------------------------
;   Check the name of stations and path of retro..
;-----------------------------------------------------------------

;---- Read all possible sites from the retroplumes: loop over path.. 

allvar_name = strarr(300)
allvar_path = strarr(300)
kk = 0
for n=0,npath-1 do begin
                                ;--- Read one file
    file = file_search(path_retro(n) + prefix(n) + '*.nc')
;    print,file(0)
    peyl_readnc,file=file(0),vv,/only_var_name

    foo = where(vv ne 'nav_lat' and vv ne 'nav_lon' and $
                vv ne 'presnivs' and vv ne 'time_counter',cc)
    if cc le 0 then message,'Hum problem with stations...'
    allvar_name(kk:kk+cc-1) = vv(foo)
    allvar_path(kk:kk+cc-1) = path_retro(n)
    kk = kk + cc
endfor
allvar_name = strupcase(allvar_name(0:kk-1))
allvar_path = allvar_path(0:kk-1)

;----- Specific case of all station asked
if strupcase(obs_name(0)) eq 'ALL' then obs_name = allvar_name

;----- Check obs name and associated path
obs_name = strupcase(obs_name)
nobs = n_elements(obs_name)
obs_path = strarr(nobs)
for n=0,nobs-1 do begin
    foo = where(allvar_name eq obs_name(n), cc)
    if cc ne 1 then begin
        print,'Problem with a station not found : ', obs_name(n)
        stop
    endif    
    obs_path(n) = allvar_path(foo(0))
endfor


;-----------------------------------------------------------------
; Define obs time arrays..
;-----------------------------------------------------------------

;---- Define julian date from calendar date 
obs_juldate_start = julday(obs_start(1),obs_start(2),obs_start(0))
obs_juldate_end   = julday(obs_end(1),obs_end(2),obs_end(0))
if obs_juldate_end lt obs_juldate_start then message,'Problem of obs start / end'

;---- Construct a daily time axis covering the period 
obs_juldate = timegen(start=obs_juldate_start,$
                      final=obs_juldate_end+0.1,units=obs_units)

;---- Define the calendar obs time axis and eliminate the 29 Febr
;     (leap year) for daily case..
obs_strdate = peyl_date2date(juldate=obs_juldate,date=obs_date,type_in='juldate',no_leap=no_leap)
ntime_obs   = n_elements(obs_juldate)

;---- Define the "date" of injection as needed for the retroplume
;     files.. depend on daily or monthly retros.. 
obs_inj = intarr(ntime_obs)
if obs_units eq 'days' then begin
    obs_inj = obs_date(2,*) + jour_cumul(obs_date(1,*)-1)
endif else begin
    obs_inj = obs_date(1,*)
endelse

                                ;-- Diagnostic
if info gt 0 then begin
    print,'----- Treating observations (nt/deb/fin): ',ntime_obs,obs_strdate([0,ntime_obs-1])
    if info eq 2 then print,'    Date Obs : ',obs_strdate
;  print,'Date inj : ',obs_inj
endif


;-----------------------------------------------------------------
;  Check & process Original flux dates 
;-----------------------------------------------------------------

;---- Define julian date for the flux that are given 
flux_juldate = peyl_date2date(date=flux_date,type_out='juldate')

;---- Define type of flux : Month / day / hour / 3hour...
if ntime_flux eq 1 then dt_flux = 1. else $
  dt_flux = mean(flux_juldate(1:*) - flux_juldate(0:ntime_flux-1))

dd = dt_flux/100.
case 1 of
    (dt_flux lt 1.-dd) : flux_type = 'hours'
    (dt_flux ge 1.-dd and dt_flux le 1.+dd) : flux_type = 'days'
    (dt_flux ge 28. and dt_flux le 31.) : flux_type = 'months'
    else : message,'flux type not recognized'
endcase

;---- Check if flux cover full years..
nyear_flux = flux_date(0,ntime_flux-1)-flux_date(0,0)+1
flux_fullyear = 1
if ((flux_type eq 'months' and (flux_date(1,ntime_flux-1) ne 12 or flux_date(1,0) ne 1)) or $ 
    (flux_type eq 'days' and (flux_date(2,ntime_flux-1) ne 31 or flux_date(2,0) ne 1)) or $ 
    (flux_type eq 'hours' and (flux_date(2,ntime_flux-1) ne 31 or flux_date(2,0) ne 1))) then $
  flux_fullyear = 0

                                ;-- Diagnostic
if info gt 0 then begin
    print,'------ Flux provided (type/dt/nt/ny): ',flux_type,dt_flux,ntime_flux,nyear_flux
endif


;-----------------------------------------------------------------
; Define flux date for the period that is needed
;-----------------------------------------------------------------

;---- Define julian date for the period of the flux that is needed
flux_need_juldate_start = julday(flux_need_start(1),flux_need_start(2),flux_need_start(0))
flux_need_juldate_end   = julday(flux_need_end(1),flux_need_end(2),flux_need_end(0))

dt_flux_need = 1.
flux_need_juldate = timegen(start=flux_need_juldate_start,$
                            final=flux_need_juldate_end+0.1,units=flux_units)


;----- Define calendar date for the flux period needed (daily values)
;      and eliminate the 29 Febr (no leap year in lmdz)..
flux_need_strdate = peyl_date2date(juldate=flux_need_juldate,type_in='juldate',$
                                   date=flux_need_date,no_leap=no_leap)
ntime_flux_need = n_elements(flux_need_juldate)


;------ Check if we need to define a climatology: if the period needed
;       for the flux is larger than the period given
add_clim = 0
if flag_clim then add_clim = 1
if ntime_flux eq 1 then add_clim = 0
if ( ntime_flux gt 1 and nyear_flux gt 1 and $
     (flux_need_juldate(ntime_flux_need-1) gt flux_juldate(ntime_flux-1)+precis or $
      flux_need_juldate(0) lt flux_juldate(0)-precis) ) then add_clim = 1
if add_clim then begin
    if info gt 0 then print,'Need to construct a flux climatology'    
    if flux_fullyear eq 0 then $
      message,'Problem : flux does not cover full year period for climatology..'
endif

                                ;-- Diagnostic
if info gt 0 then begin
    print,'------- Flux needed : ',ntime_flux_need,flux_need_strdate([0,ntime_flux_need-1])
    if info ge 2 then print,'   Date Flux_needed : ',flux_need_strdate
endif


;-----------------------------------------------------------------
;  Define flux data to be used : flux_used & flux_used_juldate !
;-----------------------------------------------------------------


;------ define a flux array with daily time step if needed (retroplumes are
;       daily sampled) AND add the climatology if needed at the end!
if dt_flux ne 1. then begin
    if info gt 0 then print,'Define a daily time step flux from original flux'

    peyl_chtime, flux, $
      champ_in_date = flux_date, $
;      champ_in_juldate = flux_juldate, $
      flux_used, $
;      units_out = units_out, $
      dt_champ_out = dt_flux_need, $
      champ_out_date = flux_used_date, $
      champ_out_juldate = flux_used_juldate, $
      add_clim = add_clim, $
      no_leap = no_leap, $
      interpol = interpol,$
      info = info
      
endif else begin
                                ;--- Add the climatology at the end of
                                ;    the flux field..
    if add_clim then begin
        if n_elements(flux(0,0,*)) ne 365*nyear_flux then message,'Humm problem flux time step..'
        flux_used = fltarr(grid.nlon,grid.nlat,ntime_flux+365)
        flux_used(*,*,0:ntime_flux-1) = flux
        ii = indgen(nyear_flux)
        for nt=0,365-1 do flux_(*,*,ntime_flux+nt) = total(flux(*,*,nt+ii))/nyear_flux
        clim_date = flux_date(*,0:365-1) & clim_date(0,*) = 1
        flux_used_date = [flux_date,clim_date]
    endif else begin
        flux_used = flux
        flux_used_date = flux_date
    endelse
endelse 
ntime_flux_used = n_elements(flux_used_date(0,*))

                                ;--- Check the total fluxes..
print,'Units of fluxes should be in KgC/hour !'
if info ge 1 then begin
    tot1 = total(flux(*,*,*))*24.*dt_flux
    nnn = ntime_flux_used-1
    if add_clim then nnn = nnn - 365
    tot2 = total(flux_used(*,*,0:nnn))*24.*dt_flux_need
    print,'Total flux : orig/used (kgC)',tot1,tot2
    if info ge 2 then begin
        for y=0,nyear_flux-1 do begin
            foo = where(flux_date(0,*) eq flux_date(0,0)+y, cc)
            tot1 = total(flux(*,*,foo))*24.*dt_flux
            foo = where(flux_used_date(0,*) eq flux_date(0,0)+y, cc)
            tot2 = total(flux_used(*,*,foo))*24.*dt_flux_need
            print,'Yearly flux : year/orig/used (kgC)',flux_date(0,0)+y,tot1,tot2
        endfor
        if add_clim then begin
            foo = where(flux_used_date(0,*) eq 1, cc)
            tot2 = total(flux_used(*,*,foo))*24.*dt_flux_need
            print,'Yearly clim flux (kgC): ',tot2            
        endif
    endif
endif


;-----------------------------------------------------------------
; Final processing : REVERSE TIME ORDER and define a pointer 
; of flux need to flux_used, switch N-S if needed
; and change units to GtC per grid
;-----------------------------------------------------------------

;------ Reverse time step of the flux_used array
if ntime_flux_used gt 1 then begin
    flux_used = reverse(flux_used,3,/overwrite)
    flux_used_date = reverse(flux_used_date,2,/overwrite)
endif

;------ Reverse time step of the flux_need array
if ntime_flux_need gt 1 then begin
    flux_need_juldate = reverse(flux_need_juldate,/overwrite)
    flux_need_date    = reverse(flux_need_date,2,/overwrite)
endif

;---- Define a pointer of all flux needed into the original flux_date
flux_need_pointer = intarr(ntime_flux_need)
if ntime_flux_used eq 1 then flux_need_pointer(*) = 0 else begin
    flag = intarr(ntime_flux_need)
                                ;--- broose all time needed.. 
    for nt=0,ntime_flux_need-1 do begin
                                ;-- Disregard the year for a
                                ;   climatologycal case..
        if flag_clim then yy_used = 1 else yy_used = flux_need_date(0,nt)        
        foo = where(flux_used_date(0,*) eq yy_used and $
                    flux_used_date(1,*) eq flux_need_date(1,nt) and $
                    flux_used_date(2,*) eq flux_need_date(2,nt), cc)
        if cc eq 0 then begin
            yy_used = 1
            foo = where(flux_used_date(0,*) eq yy_used and $
                        flux_used_date(1,*) eq flux_need_date(1,nt) and $
                        flux_used_date(2,*) eq flux_need_date(2,nt), cc)
            if cc eq 0 then begin
                print,'Problem flux_need : not found ',flux_need_strdate(nt)
                stop
            endif
            flag(nt) = 1 
        endif 
        flux_need_pointer(nt) = foo(0)
    endfor 
                                ;--- Check clim case forced..
    foo = where(flag eq 1,cc)
    if cc gt 1 and info gt 0 then print,'Warning : use clim fluxes for ',flux_need_strdate(foo)
endelse


;------ Switch from S-2-North to N-2-S array because retroplumes are
;       stored from North to South
if flux_north2south eq 0 then begin
    if info gt 0 then print,'Switch flux to get North->South order'
    flux_used = reverse(flux_used,2,/overwrite)
endif


;------ Change units : from KgC/h to GtC/days
flux_used = flux_used * 24. * 1.e-12


;-----------------------------------------------------------------
; Define output structure
;-----------------------------------------------------------------

out = {$
        nobs : nobs,$
        ntime_obs : ntime_obs,$
        name : obs_name,$
        lon : fltarr(nobs),$
        lat : fltarr(nobs),$
        alt : fltarr(nobs),$
        time_injec : fltarr(nobs,2),$
        date : obs_date,$
        juldate : obs_juldate,$
        conc : fltarr(nobs,ntime_obs) $
      }


;-----------------------------------------------------------------
; Read the "retro plumes"
;-----------------------------------------------------------------
 
                               ;--- !!!  COEF pour avoir les bonnes
                                ;    unites ppmv/GtC !!!
coef = 1.D / coef_retro * 1.e18 * 28.96/12.
limit = 0.470 ;---- pour LMDZ96

file_obs_yy = obs_date(0,*)
if obs_units eq 'days' then begin
    nfile_flux = 1 
    file_obs_yy = 2001
endif else begin
    nfile_flux = 12
endelse

;----- Loop over all time step of the obs (all retro plumes)
for nt=0,ntime_obs-1 do begin
    print,'Retro plume for ',obs_strdate(nt)

                                ;--- Treat all possible path and all
                                ;    station of a path together
    for np=0,npath-1 do begin
        ii_obs = where(obs_path eq path_retro(np), nobs_used)
        if nobs_used eq 0 then continue

                                ;--- Retroplume saved for 1yr max
        var_retro = fltarr(grid.nlon,grid.nlat,365,nobs_used)

                                ;--- Loop over all files stored for a
                                ;    given retroplume
        kk = 0L
        for nf=0,nfile_flux-1 do begin
            
                                ;--- define month and year of the
                                ;    retroplume file
            month_flux = obs_date(1,nt) - nf
            year_flux  = file_obs_yy(nt)
            if month_flux le 0 then begin
                month_flux = month_flux + 12
                year_flux  = year_flux - 1
            endif
            
                                ;--- Read the file: read all sites.. 
            file = prefix(np) + auto_string(obs_inj(nt),0) + $
              '.an'+auto_string(year_flux,0) + $
              '.m'+auto_string(month_flux,nb_digit=2) + '.nc'                
            if info gt 0 then print,'  read: ',file
            peyl_readnc,var, file=path_retro(np)+file,var_multi_name=obs_name(ii_obs)
            nt_file = n_elements(var(0,0,0,*))

                                ;--- scale it to get ppm / GtC; note
                                ;    we keep the N to S order for lati
            var_retro(*,*,kk:kk+nt_file-1,*) = var(*,*,0,*,*) * coef                
            kk = kk + nt_file
        endfor 

                                ;--- Define the date of the
                                ;    retroplumes : backward order !!! 
        ntime_retro = kk
        if (obs_units eq 'months' and ntime_retro ne 365) then message,'problem of nt retro !'
        dd_used = obs_date(2,nt) 
        if obs_units eq 'months' then dd_used = jour(obs_date(1,nt)-1)
        start = julday(obs_date(1,nt),dd_used,obs_date(0,nt),12.)
        final = julday(obs_date(1,nt),dd_used,obs_date(0,nt)-1.,12.)        
        final = final + 0.5
        retro_juldate = timegen(start=start,final=final,units=units,step_size=-1.)            
        retro_strdate = peyl_date2date(juldate=retro_juldate,type_in='juldate',$
                                       date=retro_date,no_leap=no_leap)
        if n_elements(retro_juldate) ne ntime_retro then $
          message,'Problem defining the dates of the retroplumes'
        if info ge 1 then begin
            print,'Date retro (nb/deb/fin): ',ntime_retro,retro_strdate([0,ntime_retro-1])
            if info ge 2 then print,'Date retro : ',retro_strdate
        endif

                                ;--- Extrapolate backward the
                                ;    retroplume if the last time of
                                ;    the retroplume is greater than
                                ;    the begining of the flux: On
                                ;    ecrase var_retro et retro_juldate!
        if retro_juldate(ntime_retro-1) gt flux_need_juldate(ntime_flux_need-1)+precis then begin
            var_retro = time_extrapolate(var_retro, retro_juldate, $
                                         flux_need_juldate(ntime_flux_need-1),$
                                         limit=limit, seuil_exp=0.008, $
                                         ndays_limit_exp=365*3.,$
                                         ndays_limit_lin=365.*2,$
                                         no_leap=no_leap,$
                                         info=info )
        endif 

                                ;--- Plot the retroplume to check it!! 
        if plot_retro then begin
            for no=0,nobs_used-1 do begin
                tit = obs_name(ii_obs(no))+ ': ' + obs_strdate(nt)
                step=4 & nn=20 & kk=0
                !p.multi=[0,2,2,0]
                for i=0,grid.nlon-1,step do begin
                    for j=0,grid.nlat-1,step do begin
                        if (kk mod nn) eq 0 then $
                          plot,dec,var_retro(i,j,*,no),yrange=[0.,1.],$
                          xstyle=1,/nodata,title=tit,/ynozero
                        oplot,dec,var_retro(i,j,*,no),color=255./nn*(kk mod nn)
                        kk = kk + 1
                    endfor
                endfor
            endfor
        endif 
       
                                ;--- Multiply the retroplume with the
                                ;    sources (stored from N to S) and
                                ;    sum the contributions.. All time
                                ;    arrays are backward !

                                ;-- Date de debut retro valide
        date_deb = min([flux_need_juldate(0),retro_juldate(0)])
        print,'Deb of valid retro ',peyl_date2date(juldate=date_deb)
        i1 = where(abs(retro_juldate-date_deb) lt precis, cc)
        if cc ne 1 then message,'Problem to define retro date deb...'
        j1 = where(abs(flux_need_juldate-date_deb) lt precis, cc)
        if cc ne 1 then message,'Problem to define flux date deb...'

                                ;-- Date de fin retro valide
        date_fin = flux_need_juldate(ntime_flux_need-1)
        print,'Fin of valid retro ',peyl_date2date(juldate=date_fin)
        i2 = where(abs(retro_juldate-date_fin) lt precis, cc)
        if cc ne 1 then message,'Problem to define retro date fin...'
        j2 = ntime_flux_need-1
        jj = flux_need_pointer(j1:j2)

                                ;-- Treat all sites
        for no=0,nobs_used-1 do $
          out.conc(no,nt) = total(var_retro(*,*,i1:i2,no) * flux_used(*,*,jj)) 

        if info gt 0 then begin
            print,'i1/i2/j1/j2 : ',i1(0),i2(0),j1(0),j2(0)
            print,'jj : ',jj
;            if peyl_pause() then stop
        endif
               
    endfor 

endfor


;-----------------------------------------------------------------
; Read the information for the station (lon, lat, alt, time_injec)
;-----------------------------------------------------------------
    
for np=0,npath-1 do begin
    peyl_sread,file=site_info(np),temp,nc=nc,nl=nl
    temp(0,*) = strlowcase(temp(0,*))
    ii_obs = where(obs_path eq path_retro(np), nobs_used)
    if nobs_used eq 0 then continue
    for no2=0,nobs_used-1 do begin
        no = ii_obs(no2)
        foo = where(temp(0,*) eq strlowcase(obs_name(no)), cc)
        if cc ne 1 then message,'Problem to read info for '+obs_name(no)
        out.lon(no) = temp(1,foo(0))
        out.lat(no) = temp(2,foo(0))
        out.alt(no) = temp(3,foo(0))
        out.time_injec(no,*) = temp(4:5,foo(0))
    endfor
endfor


;-----------------------------------------------------------------
; Save netcdf file if required
;-----------------------------------------------------------------

if save_file ne '' then begin

    nn = n_elements(obs_date(*,0))
    dim_size = [nobs,ntime_obs,nn]
    dim_name = ['nobs','ntime_obs','nn']
    dim_units = ['integer','decimal date','integer']
    decdate = peyl_date2date(juldate=out.juldate,type_out='decdate')
    dim_val = {D1:indgen(nobs), D2:decdate, D3:indgen(nn)}

    var_name = ['Conc','Juldate','Date','Name','Lon','Lat','Alt']
    var_units = ['ppm','julian date','integer date','string','Degre East','Degre North','m']
    var_dim = {V1:[0,1],V2:[1],V3:[2,1],V4:[0],V5:[0],V6:[0],V7:[0]}

    add_dim = 0
    peyl_writenc, out.conc,out.juldate,out.date,$
      out.name,out.lon,out.lat,out.alt,$
      file      = save_file,$
      var_dim   = var_dim,$
      var_name  = var_name,$
      var_title = var_title,$
      var_units = var_units,$
      var_misval = var_misval,$
      dim_size  = dim_size,$
      dim_val   = dim_val,$
      dim_name  = dim_name,$
      dim_title = dim_title,$
      dim_units = dim_units,$
      dim_unlimited = dim_unlimited,$
      add_dim    = add_dim,$
      nomessage  = nomessage


endif



;----- Close files and return
if plot_retro then ccg_closedev,dev='psc'
journal            
return,out
END


;==================================================================
; FUNCTION to Extrapolate the fluxes
;==================================================================

FUNCTION time_extrapolate, var_retro, retro_juldate, juldate_start,$
                           limit=limit, seuil_exp=seuil_exp, $
                           ndays_limit_exp=ndays_limit_exp,$
                           ndays_limit_lin=ndays_limit_lin,$
                           no_leap = no_leap,$
                           info=info

if n_elements(info) eq 0 then info=0

;--- Define the limit 
if n_elements(limit) eq 0 then limit = 0.47
ll = limit/50.

;--- Define the convergence "seuil" for the exponential and associated
;    date..
if n_elements(seuil_exp) eq 0 then seuil_exp = 0.008                    
if n_elements(ndays_limit_exp) eq 0 then ndays_limit_exp = 365.*3  
juldate_limit_exp = retro_juldate(0) - ndays_limit_exp

;---- Definition de date for the convergence to be achieved
;     in the linear extrapolation
if n_elements(ndays_limit_lin) eq 0 then ndays_limit_lin =  365.*2
juldate_limit_lin = retro_juldate(0) - ndays_limit_lin

;--- Define dimension of the arrays
nlon = n_elements(var_retro(*,0,0,0))
nlat = n_elements(var_retro(0,*,0,0))
nstat = n_elements(var_retro(0,0,0,*))

;---- Define time step to extrapolate : BACKWARD order and get rid of 29-Feb
nretro = n_elements(retro_juldate)
start = juldate_start
final = retro_juldate(nretro-1)-0.5
retro_extrapol_juldate = timegen(start=start,final=final,$
                                 step_size=1.,units='days')
nextrapol = n_elements(retro_extrapol_juldate)
if nextrapol ne (fix(final-start)+1) then $
  message,'Problem time extrapolation...'
if no_leap then begin
    foo = peyl_date2date(juldate=retro_extrapol_juldate,no_leap=no_leap)
    nextrapol = n_elements(retro_extrapol_juldate)
endif
retro_extrapol_juldate = reverse(retro_extrapol_juldate)

;---- Extension of the retro plume time axis
retro_juldate = [retro_juldate,retro_extrapol_juldate]
ntime_tot = nretro + nextrapol

;---- Define final arrray
var_retro_extrapol = fltarr(nlon,nlat,ntime_tot,nstat)
var_retro_extrapol(*,*,0:nretro-1,*) = var_retro

;---- Define last 2 "mean" values use for the exponential
nder=30 & navder = 30 & i0avder = nretro-60
ii_dernier = indgen(nder) + nretro - nder
ii_avdernier = indgen(navder) + i0avder
avdernier_mean     = total(var_retro(*,*,ii_avdernier,*),3)/navder
juldate_avder_mean = mean(retro_juldate(ii_avdernier))
dernier_mean       = total(var_retro(*,*,ii_dernier,*),3)/nder
juldate_der_mean   = mean(retro_juldate(ii_dernier))
dernier            = var_retro(*,*,nretro-1,*) 

;---- Check!
if info ge 1 then begin
    print,'nretro/nextrapol : ',nretro,nextrapol
    if info ge 2 then begin
        print,'Date retro: ',peyl_date2date(juldate=retro_juldate)
        print,'Date avd_mean/der_mean: ',peyl_date2date(juldate=[juldate_avder_mean,juldate_der_mean])
    endif
;    stop
endif

;---- Classement des types de retro avec identification des cas
;     bizarres sans convergence amorcee vers limit ou limit deja
;     atteinte..                                 
foo = where((dernier gt (limit-ll) and dernier lt (limit+ll)) or $
            (((dernier_mean lt limit) and (dernier_mean le avdernier_mean)) or $
             ((dernier_mean gt limit) and (dernier_mean ge avdernier_mean))), $
            complement = fooinv, ncomplement=ccinv, cc)

;---- Fit asymptotique par a*exp(b*x) en prenant les moyenne mensuelle
;     des 2 derniers mois pour calculer a et b!
if ccinv gt 0 then begin
    x = [juldate_avder_mean,juldate_der_mean] - retro_juldate(0)
    y0 = avdernier_mean(fooinv) - limit 
    y1 = dernier_mean(fooinv) - limit 
    b = alog(y0/y1)/(x(0)-x(1))
    a = y0/exp(b*x(0))
                                ;--- Cas special ou l'exponentielle a
                                ;    une courbure trop faible et ne
                                ;    tend pas vers seuil assez
                                ;    vite. On traitera ces pts par
                                ;    extrap lineaire
    ylimit = a * exp( b * (juldate_limit_exp-retro_juldate(0)) ) 
    temp = where(ylimit lt -seuil_exp or ylimit gt seuil_exp, cc_temp)
    if cc_temp gt 0 then foo = [foo,fooinv(temp)]
endif

;---- Definition de la pente pour cas de l'extrapolation lineaire;
;     pente pour atteindre la limit en ndays_limit; et Separation des
;     cas limit inf et limit sup!
temp1 = where(dernier(foo) lt limit, complement=temp2, ncomplement=cc_sup, cc_inf)
if cc_inf gt 0 then foo_inf = foo(temp1)
if cc_sup gt 0 then foo_sup = foo(temp2)
pente = (limit-dernier)/(juldate_limit_lin-retro_juldate(nretro-1))


;--- Extrapolation proprement dites pour tous les 2 cas de figure..
for i=0,nextrapol-1 do begin
    temp = var_retro_extrapol(*,*,nretro+i,*)
                                ;-- FIRST : exponential extrapolation
    if ccinv gt 0 then temp(fooinv) = limit + a * $
      exp( b * (retro_juldate(nretro+i)-retro_juldate(0))) 

                                ;-- SECOND : linear extrapolation
                                ;   limited to "limit"; possibly
                                ;   erasing exponential case..
    if cc_inf gt 0 then temp(foo_inf) = ( dernier(foo_inf) - pente(foo_inf)*(i+1.) ) < limit
    if cc_sup gt 0 then temp(foo_sup) = ( dernier(foo_sup) - pente(foo_sup)*(i+1.) ) > limit

    var_retro_extrapol(*,*,nretro+i,*) = temp
endfor   
                                                                
;---- Check graphique des valeurs..
if 0 then begin
    temp = array_indices([nlon,nlat],foo,/dimensions)
    iilon1 = reform(temp(0,*))
    iilat1 = reform(temp(1,*))
    temp = array_indices([nlon,nlat],fooinv,/dimensions)
    iilon2 = reform(temp(0,*))
    iilat2 = reform(temp(1,*))
    i = iilon2(20)
    j = iilat2(20)
    plot,retro_juldate,var_retro_extrapol(i,j,*,0)
    i = iilon1(20)
    j = iilat1(20)
    oplot,retro_juldate,var_retro_extrapol(i,j,*,0)
    if peyl_pause() then stop
endif
       
;--- Check des valeurs aberantes..
fooNAN = where(finite(var_retro_extrapol) eq 0,ccNAN)
if ccNAN gt 0 then begin
    PRINT,'BIG erreur (NAN) pour ',ccNAN,fooNAN
    stop
endif

return,var_retro_extrapol

END
;-


