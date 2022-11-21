;+
;========================================================================
; NAME:
;       PEYL_
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
;=====================================================
;	Pour changer la base de temps d'un champ 3D.
;	(moyenne ou interpolation)
;INPUT:
;	champ:champ 3D sur la bse de temps time_in.
;	      La dimension temps est la troisieme.
;	time_in: pas de temps initial
;	time_out: pas de temps final
;
;OUTPUT:
;	champ_out:champ 3D sur la base de temps time_out
;=====================================================
PRO peyl_chtime,champ_in, $
                champ_in_flag_date = champ_in_flag_date, $
                champ_in_date = champ_in_date, $
                champ_in_juldate = champ_in_juldate, $
                champ_out, $
                champ_out_flag_date = champ_out_flag_date, $
                units_time_out = units_time_out, $
                dt_champ_out = dt_champ_out, $
                champ_out_date = champ_out_date, $
                champ_out_juldate = champ_out_juldate, $
                add_clim = add_clim, $
                years_clim = years_clim, $
                no_leap = no_leap, $
                interpol = interpol_,$
                edge_truncate = edge_truncate, $
                info = info

FORWARD_FUNCTION  interpol

if n_elements(info) eq 0 then info = 0
if n_elements(no_leap) eq 0 then no_leap = 0
if n_elements(add_clim) eq 0 then add_clim = 0
if n_elements(edge_truncate) eq 0 then edge_truncate = 1
if n_elements(champ_in_flag_date) eq 0 then message,'fournir le type de champs en entree pour date'
if n_elements(champ_out_flag_date) eq 0 then message,'fournir le type de champs en sortie pour date'

;-------------------------------------------------------
;   Check champ_in and time in
;   champ_in_flag_date to decide if juldate considered or date 
;-------------------------------------------------------

;--- Size of the flux
ss_in = size(champ_in)
ntime_in = ss_in(ss_in(0))
if ntime_in eq 1 then message,'Chtime not possible with only one time step !'

;--- Check dates of flux
if champ_in_flag_date eq 'juldate' then begin
    if n_elements(champ_in_juldate(*)) ne ntime_in then $
      message,'Chtime Problem: champ_in_juldate not same size as champ_in'    
    caldat,champ_in_juldate,temp_mm,temp_dd,temp_yy,temp_hh
    if n_elements(champ_in_date) gt 0 then begin
        if (array_equal(temp_yy,champ_in_date(0,*)) eq 0 or $
            array_equal(temp_mm,champ_in_date(1,*)) eq 0 or $
            array_equal(temp_dd,champ_in_date(1,*)) eq 0) then begin
            print,'Huuummm : bizarre, champ_in_date and champ_in_juldate NOT compatible'
            stop
        endif
    endif else begin
        champ_in_date = intarr(4,ntime_in)
        champ_in_date(0,*) = temp_yy
        champ_in_date(1,*) = temp_mm
        champ_in_date(2,*) = temp_dd
        champ_in_date(3,*) = temp_hh
    endelse

endif else if champ_in_flag_date eq 'date' then begin
    if n_elements(champ_in_date(0,*)) ne ntime_in then $
      message,'Chtime Problem: champ_in_date not same size as champ_in'
    if n_elements(champ_in_date(*,0)) ge 4 then hr = champ_in_date(3,*) $
    else hr = replicate(12.,ntime_in)
    champ_in_juldate = julday(reform(champ_in_date(1,*)),reform(champ_in_date(2,*)),$
                              reform(champ_in_date(0,*)),hr)

endif else begin
    message,'fournir dans champ_in_flag_date : juldate or date !'
endelse

;---- Compute nb of year and mean time step
nyear_in = champ_in_date(0,ntime_in-1)-champ_in_date(0,0)+1
dt_champ_in = mean(champ_in_juldate(1:*)-champ_in_juldate(0:ntime_in-2))


;-------------------------------------------------------
;    Define Output dates
;-------------------------------------------------------

if champ_out_flag_date eq 'juldate' then begin
    ntime_out = n_elements(champ_out_juldate)
    caldat,champ_out_juldate,temp_mm,temp_dd,temp_yy,temp_hh
    if n_elements(champ_out_date) gt 0 then begin
        if (array_equal(temp_yy,champ_out_date(0,*)) eq 0 or $
            array_equal(temp_mm,champ_out_date(1,*)) eq 0 or $
            array_equal(temp_dd,champ_out_date(1,*)) eq 0) then begin
            print,'Chtime bizarre: champ_out_date and champ_out_juldate NOT compatible'
            stop
        endif
    endif else begin
        champ_out_date = intarr(4,ntime_out)
        champ_out_date(0,*) = temp_yy
        champ_out_date(1,*) = temp_mm
        champ_out_date(2,*) = temp_dd
        champ_out_date(3,*) = temp_hh
    endelse

endif else if champ_out_flag_date eq 'date' then begin
    ntime_out = n_elements(champ_out_date(0,*))
    champ_out_juldate = peyl_date2date(date=champ_out_date,type_out='juldate')

endif else if champ_out_flag_date eq 'units' then begin
    units = strupcase(units_time_out)
    if (units ne 'Y' or units ne 'YEARS' or $
        units ne 'M' or units ne 'MONTHS' or $
        units ne 'D' or units ne 'DAYS' or $
        units ne 'H' or units ne 'HOURS') then $
      message,'Chtime Error: units_time_out should be : Y/M/D/H !!'
    print,'cas a faire proprement avec date centree...'
    stop
   
endif else if champ_out_flag_date eq 'dt' then begin
    print,'Chtime: dt_champ_out considered as time step in days : regular spacing !'
    deb = julday(1,1,champ_in_date(0,0),0)+dt_champ_out/2.
    fin = julday(12,31,champ_in_date(0,ntime_in-1),24)-dt_champ_out/2.
    champ_out_juldate = timegen(start=deb,final=fin,step_size=dt_champ_out)
    ntime_out = n_elements(champ_out_juldate)
    champ_out_date = peyl_date2date(juldate=champ_out_juldate,type_out='date')

    if no_leap then begin
        foo = where(champ_out_date(1,*) ne 2 or $
                    (champ_out_date(1,*) eq 2 and champ_out_date(2,*) ne 29), cc) 
        if cc ne ntime_out then begin
            print,'Chtime Warning : 29 Feb for leap year NOT USED !!'
            champ_out_date = champ_out_date(*,foo)
            champ_out_juldate = champ_out_juldate(foo) 
            ntime_out = cc
        endif
    endif
    
    if info gt 0 then begin
        print,'Chtime output defined : nn/deb/fin ',ntime_out,$
          peyl_date2date(juldate=[champ_out_juldate(0),champ_out_juldate(ntime_out-1)],nb_field=4)
;        stop
    endif

endif else begin
    message,'fournir dans champ_out_flag_date : juldate or date or units or dt!'
endelse

;--- Define time step
if n_elements(dt_champ_out) eq 0 then $
  dt_champ_out = mean(champ_out_juldate(1:*)-champ_out_juldate(0:ntime_out-2))

ntime_year = round(365./dt_champ_out)
if info ge 1 then print,' Nb of output time step per year : ',ntime_year


;--- Special add climatology
ntot = ntime_out
if add_clim then begin
    deb = julday(1,1,1,0)+dt_champ_out/2.
    fin = julday(12,31,1,24)-dt_champ_out/2.
    clim_juldate = timegen(start=deb,final=fin,step_size=dt_champ_out)
    if ntime_year ne n_elements(clim_juldate) then message,'Problem with climatology..'
    clim_date = peyl_date2date(juldate=clim_juldate,type_out='date')
    ntot = ntot + ntime_year    
    champ_out_juldate = [champ_out_juldate,clim_juldate]
    champ_out_date = [[champ_out_date],[clim_date]]
endif

;--- Define output field..
dim_out = [ss_in(1:ss_in(0)-1),ntot]
champ_out = replicate(champ_in(0),dim_out)
ss_out = size(champ_out)


;-------------------------------------------------------
;   Define type of processing : Mean/interpol
;-------------------------------------------------------

if dt_champ_out eq dt_champ_in then begin
   print, 'Same time step: No change output=input'
   champ_out = champ_in
   return
endif

if n_elements(interpol_) eq 0 then begin 
    interpol = dt_champ_in gt dt_champ_out 
endif else interpol = interpol_


;-------------------------------------------------------
;        Averaging the field
;-------------------------------------------------------
if interpol eq 1 then goto, suite_average
print, 'Chtime: Averaging of the field'

;--- Creation des bornes inferieures et superieures
juldate_out_sup = champ_out_juldate + dt_champ_out/2.
juldate_out_inf = champ_out_juldate - dt_champ_out/2.

;---- Define flag
flag_use_date_in  = intarr(ntime_in)
flag_use_date_out = intarr(ntime_out)

;---- Loop over all time step
tt='' & for i=0,ss_in(0)-2 do tt = tt + '*,'
for t=0,ntime_out-1 do begin
                                ;--- Check date in the range [inf,
                                ;    sup]..
    ii=where(champ_in_juldate gt juldate_out_inf(t) and $
             champ_in_juldate le juldate_out_sup(t),cc)

                                ;--- Include the lowest range as a
                                ;    possibility 
    if cc eq 0 then begin
        ii = where(champ_in_juldate ge juldate_out_inf(t) and $
             champ_in_juldate le juldate_out_sup(t),cc)
        if cc eq 0 then begin
            print,'Problem chtime : no data for period',$
              peyl_date2date(juldate=[juldate_out_inf(t),juldate_out_sup(t)])
            print,'Need to interpolate !!'
            stop
        endif
    endif 
                                ;--- Save some info..
    flag_use_date_in(ii) = flag_use_date_in(ii) + 1
    flag_use_date_out(t) = cc

                                ;--- Perform the averaging
                                ;    operation.. 
    cmde = 'champ_out('+tt+'t) = '
    if cc gt 1 then cmde = cmde + 'total(champ_in('+tt+'ii),ss_in(0))/cc' $
      else cmde = cmde + 'champ_in('+tt+'ii)'
    res = execute(cmde)
endfor

;---- Check of the data not used
foo = where(flag_use_date_in eq 0, cc)
if cc gt 0 then begin
    print,'Chtime bizarre: some champ_in are not used ',$
      peyl_date2date(juldate=champ_in_juldate(foo))
    if peyl_pause() then stop
endif 

;---- Check of the data used several times
foo = where(flag_use_date_in gt 1, cc)
if cc gt 0 then begin
    print,'Chtime bizarre: some champ_in used several times ',$
      peyl_date2date(juldate=champ_in_juldate(foo))
    stop
endif 

suite_average:


;-------------------------------------------------------
;            INTERPOLATION 
;-------------------------------------------------------
if interpol eq 0 then goto,suite_interpol
print, 'Chtime: Interpolation of the field'

;--- Define position of the interpolates as a float between 0 and ntime_in

if 0 then begin
;;    timepos_out = (champ_out_juldate - champ_in_juldate(0)) / $
;;     (champ_in_juldate(ntime_in-1) - champ_in_juldate(0)) * ntime_in
;;    ii = indgen(32)
;;    print,'time in ',peyl_date2date(juldate=champ_in_juldate(ii))
;;    print,'time out ',peyl_date2date(juldate=champ_out_juldate(ii))
;;    print,'timepos_out ',timepos_out(ii)
;;    if peyl_pause() then stop

;--- Interpolation itself (ne marche pas...)
;;    champ_out(*) = interpolate(champ_in,timepos_out,missing=missing)
endif

if 1 then begin
    nn  = 1L & for i=0,ss_in(0)-2 do nn=nn*ss_in(i+1)
    ii1 = indgen(ntime_in)*nn
    ii2 = indgen(ntime_out)*nn
;    print,'nn ',nn
    for i=0L,nn-1 do champ_out(ii2+i) = interpol(champ_in(ii1+i),champ_in_juldate,champ_out_juldate(0:ntime_out-1))
    if edge_truncate then begin
        foo = where(champ_out_juldate(0:ntime_out-1) lt champ_in_juldate(0),cc)
        for i=0,cc-1 do champ_out(indgen(nn)+foo(i)*nn) = champ_in(indgen(nn))
        foo = where(champ_out_juldate(0:ntime_out-1) gt champ_in_juldate(ntime_in-1),cc)
        for i=0,cc-1 do champ_out(indgen(nn)+foo(i)*nn) = champ_in(indgen(nn)+(ntime_in-1)*nn)
    endif
endif 

suite_interpol:


;-------------------------------------------------------
;   Define climatological field
;-------------------------------------------------------

if add_clim eq 0 then goto,suite_clim

if info gt 0 then print,'Chtime: Compute climatology...'

nn = n_elements(years_clim) 
if nn eq 0 then begin
                                ;--- Check if flux cover full years
                                ;    define initially..
    ndays = round(dt_champ_out * ntime_out)
    if ndays ne (nyear_in*365) then begin
        print,'Chtime Error: Not full years for the clim '
        print,'   -> use years_clim to define the years to use '
        stop
    endif
    ii_clim = indgen(ntime_out)
    nyear_clim = nyear_in

                                ;--- Year to use for climatol. are
                                ;    supplied.. 
endif else begin
    ii_clim = -1
    for n=0,nn-1 do begin
        foo = where(champ_out_date(0,0:ntime_out-1) eq years_clim(n),cc)
        if cc ne ntime_year then begin
            print,'Chtime Problem define climatology : not full year ',years_clim(n),cc,ntime_year
            stop
        endif
        ii_clim = [ii_clim,foo]
    endfor
    ii_clim = ii_clim(1:*)
    nyear_clim = nn
endelse
                                ;--- Define the climatology using one
                                ;    time step each year..
jj = indgen(nyear_clim)*ntime_year
ii = ii_clim(jj)
tt='' & for i=0,ss_in(0)-2 do tt = tt + '*,'
for t=0,ntime_year-1 do begin
    cmde = 'champ_out('+tt+'ntime_out+t) = '
    if nyear_clim gt 1 then cmde = cmde + 'total(champ_out('+tt+'ii+t),ss_out(0))/nyear_clim' $
      else cmde = cmde + 'champ_out('+tt+'ii+t)'
    res = execute(cmde)
    if info ge 2 and t eq 0 then print,cmde
endfor

suite_clim:


;-------------------------------------------------------
;   Some Check (debbug)
;-------------------------------------------------------

if 0 and info ge 1 then begin
    nn  = 1L & for i=0,ss_in(0)-2 do nn=nn*ss_in(i+1)
    step = nn/100 > 1
    !p.multi = [0,1,1,0]
    loadct,39
    ii1 = indgen(ntime_in)*nn
    ii2 = indgen(ntime_out)*nn
    ii3 = (ntime_out*nn) + indgen(ntime_year)*nn
    x1 = peyl_date2date(juldate=champ_in_juldate,type_out='decdate')
    x2 = peyl_date2date(juldate=champ_out_juldate,type_out='decdate')
    for n=0,nn-1,step do begin
        plot,x1,champ_in(ii1+n)
        oplot,x2,champ_out(ii2+n),col=250
        if add_clim then begin
            x3 = peyl_date2date(juldate=champ_out_juldate(0:ntime_year-1),type_out='decdate')            
            oplot,x3,champ_out(ii3+n),col=150
        endif
        if peyl_pause() then stop
    endfor
endif

end
;-
