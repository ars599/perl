@peyl_plottime
@/home/satellites/delage/PROJETS/LMDZ/SELECT_DATA/prog/taylor
;
;/////////////////////////////////////////////////
;/////////////////////////////////////////////////
;
FUNCTION STATISTICS,xarr,yarr,index=index
res = MC_LINFIT(xarr,yarr,CHISQ=chisq,SIGMA=sigma,R=coeff_corr)
intercept=res(0) & slope=res(1)
nn=n_elements(xarr)
ypred=replicate(intercept,nn)+slope*xarr
rmse = sqrt(total((xarr-yarr)^2)/nn) & rmse_sys= sqrt(total((xarr-ypred)^2)/nn) & rmse_uns= sqrt(total((yarr-ypred)^2)/nn)
if index then xmean = replicate(total(xarr)/nn,nn) ;--- new 28/01/2003
if index then index_agreement=1.-total((xarr-yarr)^2)/total((abs(xarr-xmean)+abs(yarr-xmean))^2) ;--- modified 28/01/2003
if index then result=fltarr(8) else result=fltarr(7)
result(0) = intercept
result(1) = slope
result(2) = coeff_corr^2
result(3) = rmse
result(4) = rmse_uns
result(5) = rmse_sys
if index then result(6) = index_agreement else result(6) = nn
if index then result(7) = nn
return,result
END
;
;/////////////////////////////////////////////////
;/////////////////////////////////////////////////
;
PRO MAIN 

;-- Main path
path_mod ='/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/modeloutput/'
;path_obs = '/home/gcarb/contrib/globalview/globalview_06/'
;path_obs = '/home/aerocarb/web_ce_atm/base_de_donnees/cont/data_fmt/'
path_obs = '/home/satellites/delage/PROJETS/LMDZ/INTERFACE/DATA_FILES/'
;path_ps  = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/ps_files/rapport_gems/radon/' 
;path_restore = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/ps_files/rapport_gems/radon/'
path_ps  = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/ps_files/'
path_restore = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/ps_files/sav/'

;-- Main option and size
restore_file = 0
globalview = 0
nday = 730
nobs_max = 100000
ntime_in = 17520
missval = 1E+20
nskip = 35
if globalview eq 1 then nskip = 7

;-- Defines time variables
time_name = ['JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY',$
             'AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER']
jour      = [31,28,31,30,31,30,31,31,30,31,30,31]

;-- Defines julian date 
julday_start = julday(1,1,2002,0,30)
julday_final = julday(12,31,2003,23,30)
julyear_beg = 2002.
julyear_end = 2004.

;-- Defines range to plot
for ww = 1,1 do begin
winter = ww
if winter then begin
    julday_start1 = julday(1,1,2002,12,0)
    julday_final1 = julday(12,31,2003,12,0)
    str_season = '2002-2003'
endif else begin
    julday_start1 = julday(6,1,2002,12,0)
    julday_final1 = julday(8,31,2002,12,0)
    str_season = 'SUMMER'
endelse

;-- Defines daily mean
hour_start = 12
hour_end = 17
;hour_start = -2
;hour_end = 3
;--
for mmm = 1,1 do begin
mean_flag = mmm
if mean_flag then begin
    mean_str = 'mean' 
    str_hour = auto_string(hour_start,nb_digit=2)+':00-'+auto_string(hour_end,nb_digit=2)+':00'
endif else begin 
    mean_str = 'nomean'
    str_hour = ''
endelse

;-- Defines model to use
; model_name = ['AM2.GFDL.2006-01-27','AM2t.GFDL.2006-01-27','CCSR.NIES.FRCGC.2006-03-07',$ ;'CDTM.JMA.2006-01-31',$
;               'LMDZ.LSCE.2006-02-03','LMDZ_THERM.2006-02-10','NIES05.NIES_ESC.2006-03-07',$ ;'PCTM.CSU.2006-01-27',$
;               'PCTM.GSFC.2005-12-12','REMO.BGC.2006-03-06','STAG.AIST.2006-03-24','TM3_fg.BGC.2005-11-23',$
;               'TM3_vfg.BGC.2005-11-23','TM5_eur1x1.SRON.2006-01-25','TM5_eur1x1.SRON.2006-03-24',$
;               'TM5_glb3x2.ESRL.2006-03-21','TM5_glb3x2.ESRL.2006-03-21','TM5_nam1x1.ESRL.2006-03-21']
; model_filename = ['.AM2.GFDL.','.AM2t.GFDL.','.CCSR_NIES.FRCGC.',$ ;'.CDTM.JMA.',$
;                   '.LMDZ.LSCE.','.LMDZ_THERM.LSCE.','.NIES05.NIES_ESC.',$ ;'.PCTM.CSU.',$
;                   '.PCTM.GSFC.','.REMO.MPIBGC.','.STAG.AIST.','.TM3_fg.BGC.',$
;                   '.TM3_vfg.BGC.','.TM5_eur1x1.SRON.','.TM5_eur1x1.SRON.',$
;                   '.TM5_glb3x2.ESRL.','.TM5_glb3x2.ESRL.','.TM5_nam1x1.ESRL.']

;model_name = ['LMDZ.LSCE.2006-08-02','LMDZ4_KZ.LSCE.2007-11-22','TM5_glb3x2.ESRL.2006-03-21']
;model_filename = ['.LMDZ.LSCE.','.LMDZ4_KZ_lmdz_trcom.LSCE.','.TM5_glb3x2.ESRL.']
;model_name = ['LMDZ4_KZ.LSCE.2007-11-22','LMDZ4_KZ.LSCE.2007-11-22','LMDZ4_KZ.LSCE.2007-11-22','LMDZ4_KZ.LSCE.2007-11-22','TM5_glb3x2.ESRL.2006-03-21']
;model_filename = ['.LMDZ4_KZ_ecbin_interp.LSCE.','.LMDZ4_KZ_ecbin_trcom.LSCE.','.LMDZ4_KZ_era_trcom.LSCE.','.LMDZ4_KZ_lmdz_trcom.LSCE.','.TM5_glb3x2.ESRL.']
;model_name = ['LMDZ.LSCE.2006-08-02','LMDZ4.LSCE.2007-09-06','LMDZ4_38.LSCE.2007-11-20','TM5_glb3x2.ESRL.2006-03-21']
;model_filename =
;['.LMDZ.LSCE.','.LMDZ4.LSCE.','.LMDZ4_38.LSCE.','.TM5_glb3x2.ESRL.']
;model_name = ['LMDZ4_Ttke.LSCE.2007-12-11','LMDZ_KZ.LSCE.2007-12-11','TM3_fg.BGC.2005-11-23','IFS.ECMWF.2006-09-27','TM5_glb3x2.ESRL.2006-03-21']
;model_filename = ['.LMDZ4_Ttke.LSCE.','.LMDZ_KZ.LSCE.','.TM3_fg.BGC.','.IFS.ECMWF.','.TM5_glb3x2.ESRL.']
model_name = ['LMDZ4_Ttke.LSCE.2007-12-11','LMDZ4_Ttke_Therm.LSCE.2007-12-11','TM3_fg.BGC.2005-11-23','TM5_glb3x2.ESRL.2006-03-21']
model_filename = ['.LMDZ4_Ttke_opt.LSCE.','.LMDZ4_Ttke_Therm_Nt_opt.LSCE.','.TM3_fg.BGC.','.TM5_glb3x2.ESRL.']
;model_name = ['LMDZ4_Ttke.LSCE.2007-12-11','LMDZ_KZ.LSCE.2007-12-11','TM3_fg.BGC.2005-11-23','TM5_glb3x2.ESRL.2006-03-21']
;model_filename = ['.LMDZ4_Ttke.LSCE.','.LMDZ_KZ.LSCE.','.TM3_fg.BGC.','.TM5_glb3x2.ESRL.']
;model_name = ['LMDZ4.LSCE.2007-09-06','LMDZ4_38.LSCE.2007-11-20','TM3_vfg.BGC.2005-11-23','TM5_glb3x2.ESRL.2006-03-21','TM5_eur1x1.SRON.2006-03-24']
;model_filename = ['.LMDZ4.LSCE.','.LMDZ4_38.LSCE.','.TM3_vfg.BGC.','.TM5_glb3x2.ESRL.','.TM5_eur1x1.SRON.']
;model_filename = ['.LMDZ.LSCE.','.LMDZ_THERM.LSCE.','.TM3_fg.BGC.']
;model_filename = ['.LMDZ.LSCE.','.LMDZ_THERM.LSCE.','.TM5_eur1x1.SRON.','.Chimere.LSCE.']
;model_name = ['LMDZ.LSCE.2006-08-02','LMDZ_THERM.2006-07-18','LMDZ4.LSCE.2007-09-06','LMDZ4.LSCE.2007-09-06']
;model_filename = ['.LMDZ.LSCE.','.LMDZ_THERM.LSCE.','.LMDZ.LSCE.','.LMDZ4.LSCE.']
;color_tot = [20,100,150,220,0]
color_tot = [1,4,10,22]
nmod =  n_elements(model_name)
nobs = 1
;-- Defines tracers to use
;tracer_name = ['radon']
tracer_name = ['fossil98','CASA','Taka02']
;tracer_name = ['fos','sib_hr','taka']
ntracer = n_elements(tracer_name)
;-- Defines stations to use
;  station_name = ['PRS','CMN','KZD','PUY','SCH','PAL','HUN010','CBW020','MHD','HEI','WES','LMP']
;  data_filename = ['prs_21C0_ext.co2','cmn_17C0_ext.co2','kas_53C0_ext.co2',$
;                   'puy_21C0_ext.co2','sch_23C0_ext.co2','pal_30C0_ext.co2',$
;                   'hun0010_35C3_ext.co2','cbw0020_52C3_ext.co2','mhd_11C0_ext.co2',$
;                   'hei_22C0_ext.co2','wes_23C0_ext.co2','lam_99C9_ext.co2'$
;                  ]
IF tracer_name(0) NE 'radon' THEN BEGIN
   station_name = ['PRS','CMN','PUY','SCH','HUN115','CBW200','MHD','PAL','HEI']
                   ;'Berezo5','Salym45','Igrim24','Noyabrsk21','Parabel35']
   data_filename = ['prs_21C0_ext.co2','cmn_17C0_ext.co2','puy_21C0_ext.co2','sch_23C0_ext.co2',$
                    'hun0115_35C3_ext.co2','cbw0200_52C3_ext.co2','mhd_11C0_ext.co2','pal_30C0_ext.co2','hei_22C0_ext.co2'];,$
                    ;'frd040_06C3_ext.co2',$
                    ;'BRZ_5m_2003.dat','DEMlow_2003.dat','IGRlow_2003.dat','NOYlow_2003.dat','PRBlow_2003.dat'$
                   ;]
ENDIF ELSE BEGIN
   station_name = ['PUY','SCH','MHD','HEI','PAL','ZEP']
   data_filename = ['puy_21C0_ext.rn','rn_sch/sch_23C0_ext.rn','mhd_11C0_ext.rn','hei_22C0_ext.rn','pal_30C0_ext.rn','zep_31C0_ext.rn']
ENDELSE



; station_name = ['HUN115']
; data_filename = ['hun0115_35C3_ext.co2']

;  station_name = ['ALT','BAL','BGU','BRW','BSC','CBA','COI','ESP',$
;                  'FRD','GSN','HUN010','ICE','KZD','LMP','MHD',$
;                  'PAL','RYO','SBL','SHM','SIS','STM','TAP',$
;                  'WIS']


;   data_filename = ['alt_06C0_ext.co2','bal_01D1_ext.co2','bgu_11D0_ext.co2','brw_01C0_ext.co2',$
;                    'bsc_01D0_ext.co2','cba_01D0_ext.co2','coi_20C0_ext.co2','esp_06D0_ext.co2',$
;                    'frd040_06C3_ext.co2','gsn_24D0_ext.co2','hun010_35C3_ext.co2','ice_01D0_ext.co2',$
;                    'kzd_01D0_ext.co2','lmp_28D0_ext.co2','mhd_01D0_ext.co2',$
;                    'pal_01D0_ext.co2','ryo_19C0_ext.co2','sbl_06C0_ext.co2','shm_01D0_ext.co2',$
;                    'sis_02D0_ext.co2','stm_01D0_ext.co2','tap_01D0_ext.co2','wis_01D0_ext.co2']





nstat = n_elements(station_name)

;--- GOTO // restore_session
IF restore_file THEN GOTO,restore_session

;------------------------
;  READ MODEL DATA
;------------------------

DATA_MOD = fltarr(nmod,nstat,ntime_in) & DATA_MOD = replicate(0.,nmod,nstat,ntime_in)
;-- // LOOP on models
FOR nm = 0,nmod-1 DO BEGIN

    ;-- Defines file names
    file_name1 = path_mod + model_name(nm) + '/all' + model_filename(nm) + '2002.nc'
    file_name2 = path_mod + model_name(nm) + '/all' + model_filename(nm) + '2003.nc'

    ;if nm eq 0 then begin
    ;-- Read station name
    peyl_readnc,tmp,var_name = 'site_name',file=file_name2
    statname_model = string(tmp)
    for i = 0,n_elements(statname_model)-1 do statname_model(i) = strtrim(statname_model(i),2)
    ;if nm eq 4 then statname_model = ['CBW020','CBW200','MHD','SCH']
    ;-- Read tracer name
    peyl_readnc,tmp,var_name = 'tracer_name',file=file_name2
    tracername_model = string(tmp)
    for i = 0,n_elements(tracername_model)-1 do tracername_model(i) = strtrim(tracername_model(i),2)
    ;-- Read concentration
    ier = file_test(file_name1)
    if ier eq 1 then begin
        peyl_readnc,VAR1,var_name = 'conc',file=file_name1
    endif else begin
        VAR1 = replicate(missval,nstat,ntracer,8760)
    endelse
    peyl_readnc,VAR2,var_name = 'conc',file=file_name2
    ;if nm eq 1 then begin
    ;    var1 = var1*2.41
    ;    var2 = var2*2.41
    ;endif

    ;-- // LOOP on stations
    FOR ns = 0,nstat-1 DO BEGIN
        
        ;-- find station to extract
        ptr_stat = where(statname_model EQ station_name(ns),nn)
        IF nn NE 0 THEN BEGIN
            
            ;-- // LOOP on tracers
            FOR nf = 0,ntracer-1 DO BEGIN
                
                ;-- find tracer to extract
                ptr_tracer = where(tracername_model EQ tracer_name(nf),nn)
                IF nn NE 0 THEN BEGIN
                   IF model_name(nm) EQ 'IFS.ECMWF.2006-09-27' THEN BEGIN
                      rt = indgen(2920)*3
                      ;-- first year (2002)
                      tmp = VAR1(ptr_stat,ptr_tracer,*)
                      DATA_MOD(nm,ns,rt)   = DATA_MOD(nm,ns,rt)   + tmp
                      DATA_MOD(nm,ns,rt+1) = DATA_MOD(nm,ns,rt+1) + tmp
                      DATA_MOD(nm,ns,rt+2) = DATA_MOD(nm,ns,rt+2) + tmp
                      ;-- second year (2003)
                      tmp = VAR2(ptr_stat,ptr_tracer,*)
                      DATA_MOD(nm,ns,8760+rt)   = DATA_MOD(nm,ns,8760+rt)   + tmp
                      DATA_MOD(nm,ns,8760+rt+1) = DATA_MOD(nm,ns,8760+rt+1) + tmp
                      DATA_MOD(nm,ns,8760+rt+2) = DATA_MOD(nm,ns,8760+rt+2) + tmp
                   ENDIF ELSE BEGIN
                      ;-- first year (2002)
                      DATA_MOD(nm,ns,0:8759) = DATA_MOD(nm,ns,0:8759) + VAR1(ptr_stat,ptr_tracer,*)
                      ;-- second year (2003)
                      DATA_MOD(nm,ns,8760:17519) = DATA_MOD(nm,ns,8760:17519) + VAR2(ptr_stat,ptr_tracer,*)
                   ENDELSE                  
                ENDIF ELSE BEGIN
                    print,'PB ::',tracer_name(nf)
                    stop
                ENDELSE

            ENDFOR

        ENDIF ELSE BEGIN
            print,'PB ::',station_name(ns)
            print,model_name(nm)
            ;stop
        ENDELSE

    ENDFOR

ENDFOR

;--------------------
;  READ OBS DATA
;--------------------

;-- Allocate arrays
TIME_COUNTER_OBS = replicate(missval*1D0,nstat,nobs_max)
DATA_OBS = replicate(missval,nstat,nobs_max)
NOBS_STAT = replicate(0,nstat)

cobs = 10
if globalview eq 1 then cobs = 1

s=''
;-- // LOOP on stations
FOR ns = 0,nstat-1 DO BEGIN

    ;-- Defines file name
    file_name = path_obs + data_filename(ns)
    ;-- Open data file
    openr,unit,file_name,/GET_LUN
    FOR i = 0,nskip-1 DO readf,unit,s
    
    i = 0L
    WHILE NOT EOF(unit) DO BEGIN

        ;-- Read data file
        readf,unit,s
        line = strsplit(s,' ',/EXTRACT)

        ;-- Test data quality
        cobs = 11
        IF tracer_name(0) EQ 'radon' THEN cobs = 10
        ;--
        ;IF station_name(ns) EQ 'FRD' THEN BEGIN
        ;  cobs = 2
        ;  line(cobs-1) = '.'
        ;ENDIF
        ;IF ns GE 10 THEN BEGIN
        ;  cobs = 10
        ;ENDIF
        IF strmid(line(cobs-1),0,1) EQ '.' OR strmid(line(cobs-1),0,1) EQ '?' OR strmid(line(cobs-1),0,1) EQ '#' OR globalview EQ 1 THEN BEGIN
            ;-- Test if data is in good time range
            IF double(line(0)) GE julyear_beg AND double(line(0)) LT julyear_end THEN BEGIN
                IF float(line(cobs)) GT 0 THEN BEGIN
                    TIME_COUNTER_OBS(ns,i) = double(line(0))
                    DATA_OBS(ns,i) = float(line(cobs))
                    i = i + 1
                ENDIF
            ENDIF

        ENDIF

    ENDWHILE

    NOBS_STAT(ns) = i
    free_lun,unit

END

;--------------------
;  PUT MODEL AT OBSERVATION TIME
;--------------------

DATA_MOD_NEW = replicate(missval,nmod,nstat,nobs_max)
;-- Defines time_counter
temp = timegen(start=julday_start,final=julday_final,units='H')
caldat,temp,mo,dy,yr,hr,mn
ccg_date2dec,yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=time_counter

;-- // LOOP on stations
FOR ns = 0,nstat-1 DO BEGIN

    ;-- // LOOP on obs time
    FOR nt = 0,NOBS_STAT(ns)-1 DO BEGIN

        time_temp = TIME_COUNTER_OBS(ns,nt)

        ii = where(time_counter gt time_temp,nn)
        if ii(0) eq 0 then begin
            DATA_MOD_NEW(*,ns,nt) = DATA_MOD(*,ns,ii(0))
        endif
        if nn eq 0 then begin
            DATA_MOD_NEW(*,ns,nt) = DATA_MOD(*,ns,n_elements(time_counter)-1)
        endif
        if (ii(0) ne 0) and (nn ne 0) then begin
            diff1 = (time_temp - time_counter(ii(0)-1))/(time_counter(ii(0))-time_counter(ii(0)-1))
            diff2 = (time_counter(ii(0)) - time_temp)/(time_counter(ii(0))-time_counter(ii(0)-1))
            
            DATA_MOD_NEW(*,ns,nt) = diff2 * DATA_MOD(*,ns,ii(0)-1) + diff1 * DATA_MOD(*,ns,ii(0))
        endif

    ENDFOR

ENDFOR

;------------------------
;  DAILY MEAN
;------------------------

DATA_MOD_MEAN = replicate(missval,nmod,nstat,nday)
DATA_OBS_MEAN = replicate(missval,nstat,nday)

FOR ns = 0,nstat-1 DO BEGIN

    FOR nd = 0,nday-1 DO BEGIN
    
        datedec1 = 2002. + double(nd/365.) + hour_start/(365*24.)
        datedec2 = 2002. + double(nd/365.) + hour_end/(365*24.)
        
        ii = where(TIME_COUNTER_OBS(ns,*) GE datedec1 AND TIME_COUNTER_OBS(ns,*) LT datedec2,nn)
        IF nn NE 0 THEN BEGIN
            
            jj = where(DATA_OBS(ns,ii) ne missval,mm)
            if mm ne 0 then begin
                
                ;-- OBS
                DATA_OBS_MEAN(ns,nd) = total((DATA_OBS(ns,ii))(jj))/mm

                ;-- MOD
                for nm = 0,nmod-1 do begin

                    IF mm EQ 1 THEN BEGIN 
                        DATA_MOD_MEAN(nm,ns,nd) = (DATA_MOD_NEW(nm,ns,ii))(jj)
                    ENDIF ELSE BEGIN
                        DATA_MOD_MEAN(nm,ns,nd) = total((DATA_MOD_NEW(nm,ns,ii))(jj))/mm
                    ENDELSE
                endfor

            endif
            
        ENDIF            
        
    ENDFOR
ENDFOR

;------------------------
;  RESTORE FILE
;------------------------

restore_session:

file_save = path_restore+'lmdz_intercomp-'+str_season+'_'+str_hour+'.gz'
IF restore_file THEN BEGIN
    restore,file_save
ENDIF ELSE BEGIN
    print,''
    print,' --> Sauvegarde de la session dans : ',file_save
    save_cmde = "save, filename='"+file_save+"',/compress,"
    save_cmde = save_cmde+$
      'DATA_MOD_NEW,DATA_MOD_MEAN,TIME_COUNTER_OBS,DATA_OBS,DATA_OBS_MEAN,NOBS_STAT'
    res = execute(save_cmde)
ENDELSE

;========================
;  PLOTS
;========================

;--------- Define structure "par_plot" passed to plot routine
nplot=nmod
par_plot = { $
;---- GENERAL
position:[0.1,0.1,0.9,0.9],$
;  xrange: [0.,0.],$             ;--- [0,0] for automatic definition else the range
  xrange: [1,nmod],$             ;--- [0,0] for automatic definition else the range
  yrange: [0,0],$             ;--- [0,0] for automatic definition else the range
  max_number:missval-1,$            ;--- maximum number to account for in the ranges
  ythick:2,$                    ;--- y thickness
  xthick:2,$                    ;--- x thickness
  cthick:2,$                    ;--- character thickness
  csize:2.,$                   ;--- character size
  color_text:0,$                ;--- color of the text
  seuil:0,$                 ;--- seuil maximum des err prise en compte (0 sinon)..
;
;---- AXIS
ystyle:1,$                      ;--- 1 exact yrange, 2 extended range
  xstyle:1,$                    ;--- 1 exact yrange, 2 extended range
  xlabel : 1,$
  xtickformat : [' %M','Year %Y'],$
;  xtickformat : ['%H:%I','%D %M %Y'],$
;  xtickformat : ['%M'],$
  xtype:'',$                    ;--- 'month' for 12 monthly values else automatic
  xticks:0,$                    ;--- number of major ticks per period (year, month...)
;  xticks:xlimit(1)-xlimit(0),$                    ;--- number of major ticks per period (year, month...)
  xtickv:replicate(0,50),$      ;--- valeur correspondant aux ticks: (xticks + 1) valeurs
  xtickname:replicate('',50),$  ;--- text imprime pour chaque tick ('' pour automatique)
  xminor:1,$                    ;--- number or minor ticks
  yticks:0,$                    ;--- number of Y major ticks (0 for automatic)
  yminor:2,$                    ;--- number or Y minor ticks (0 for automatic)

;
;---- TITLES
xtitle:[''],$                     ;--- Titre en X
  ytitle: 'Daily mean ('+str_hour+') CO!D2!N concentration (PPM)',$              ;--- Titre en Y
  title: '',$                   ;--- Major Title
  xpos_title:0.5,$              ;--- [0,1] : abscisse for title posi. if coord_title= Normal/data
  ypos_title:0.98,$              ;--- [0,1] : ordonnee for title posi. if coord_title= Normal/data
  coord_title:' ',$             ;--- 'Normal': for normal coordi, 'data' for data coord else standard above the graph
  csize_title:1.5,$             ;--- character size of major title
  cthick_title:2,$              ;--- character thick of major title
  footnote: '',$                ;--- String for footnote at bottom of page..
;
;---- MODEL
add_mod:replicate(1,nplot),$    ;--- to add each component or not
  add_mod_err:replicate(0,nplot),$ ;-- 1 to add model std_dev; 2 Mean error on the side !
  color_mod:[0,50,150,200,250],$
  lines_mod:replicate(0,nplot),$
  thick_mod:replicate(2,nplot),$   ;--- global thickness for model
  thick_mod_err:replicate(2,nplot),$ ;--- thickness for model error bar
  symbol_mod: replicate(0,nplot),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
  symtype_mod:replicate(1,nplot),$ ;--- model symbol type
  symsize_mod:replicate(0.8,nplot),$              ;--- model symbol size
  symfill_mod:replicate(0,nplot),$ ;--- model symbol filling
  add_mod_grise:0,$                ;-- 1: grise fournit dans structure model, 2 calculer avec courbes de mod_grise, 3: 1ere courbe de mod_grise + std_dev
  mod_grise:replicate(0,nplot),$   ;-- Models utilise pour Option 2/3 de add_mod_grise
  color_grise:220,$

;
;---- OBSERVATIONS
add_obs: replicate(1,nobs), $      ;-- to add observation component or not
  add_obs_err: replicate(0,nobs),$ ;--- 1 to add error from std_dev
  color_obs:24,$
  lines_obs:replicate(1,nobs),$
  thick_obs:replicate(2,nobs),$   ;--- global thickness for obs
  thick_obs_err:replicate(2,nobs),$ ;--- thickness for obs error bar
  symbol_obs: replicate(0,nobs),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
  symtype_obs:replicate(3,nobs),$ ;--- obser. symbol
  symsize_obs:replicate(1.5,nobs),$ ;--- obser. symbol size ; 1.5
  symfill_obs:replicate(1,nobs),$ ;--- obser. symbol filling
;
;---- LEGEND
add_leg_mod:replicate(0,nmod),$
  add_leg_obs:replicate(0,nmod),$
  mod_leg:replicate('',nmod),$
  obs_leg:replicate('',nobs),$
  ;xpos_leg: [0.2, 0.4, 0.7, 0.8],$        ;-- X position of the legs in Normal/DATA coord.
  ;ypos_leg: [0.8, 0.03, 0.03, 0.03],$        ;-- Y position of the legs "
  xpos_leg: [0.10,0.28,0.45,0.60,0.75],$        ;-- X position of the legs in Normal/DATA coord.
  ypos_leg: [0.45,0.45,0.45,0.45,0.45],$        ;-- Y position of the legs "
  nleg_col: 5,$                           ;-- Nb of colone (xpos_leg) for the legs used
  coord_leg:'normal',$                    ;-- Normal or Data coordonates
  incr_text_leg:1.,$                      ;-- Scaling for incremental Dy between text leg
  csize_leg  : 1.,$                       ;-- character size
  cthick_leg : 2 $                        ;-- character thick
}


;-- Defines time_counter
time_counter = timegen(start=julday_start,final=julday_final,units='D')

;-- Treat model data

;-

IF tracer_name(0) NE 'radon' THEN BEGIN
FOR ns = 0,nstat-1 DO BEGIN

    ii = where( DATA_MOD_MEAN(0,ns,*) ne missval,nn)
    jj = where( DATA_MOD_NEW(0,ns,*) ne missval,nn)
    ;--
    mean_tot_obs1 = peyl_mean(DATA_OBS_MEAN(ns,ii),mask=missval)
    mean_tot_obs2 = peyl_mean(DATA_OBS(ns,jj),mask=missval)
    ;DATA_OBS_MEAN(ns,ii) = DATA_OBS_MEAN(ns,ii) - mean_tot_obs1
    ;DATA_OBS(ns,jj) = DATA_OBS(ns,jj) - mean_tot_obs2

    FOR nm = 0 ,nmod-1 DO BEGIN

        mean_tot_mod = peyl_mean(DATA_MOD_MEAN(nm,ns,ii),mask=missval)
        DATA_MOD_MEAN(nm,ns,ii) = DATA_MOD_MEAN(nm,ns,ii) - mean_tot_mod + mean_tot_obs1
        ;--
        mean_tot_mod = peyl_mean(DATA_MOD_NEW(nm,ns,jj),mask=missval)
        DATA_MOD_NEW(nm,ns,jj) = DATA_MOD_NEW(nm,ns,jj) - mean_tot_mod + mean_tot_obs2
    ENDFOR
 ENDFOR

ENDIF ELSE BEGIN

   FOR ns = 0,nstat-1 DO BEGIN
      ii = where( DATA_MOD_MEAN(2,ns,*) ne missval,nn)
      DATA_MOD_MEAN(2,ns,ii) = DATA_MOD_MEAN(2,ns,ii)/17.6
      ii = where( DATA_MOD_MEAN(3,ns,*) ne missval,nn)
      DATA_MOD_MEAN(3,ns,ii) = DATA_MOD_MEAN(3,ns,ii)/17.6
   ENDFOR

ENDELSE


;-- Postscript file name
file_ps = 'plot_inter_'+mean_str+'_'+str_hour+'_'+str_season+'.ps'
;-- Open postscript file
ccg_opendev,dev='psc',saveas=path_ps + file_ps,portrait=0
CCG_RGBLOAD,file='/home/users/delage/lib/9color_table'
;loadct,39


;-- Defines positions of the first plots in the page
nplot_page = 2
pos1 = fltarr(4,nplot_page)
pos1 = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.02,$
                        LEFTBORD=0.08,TOPBORD=0.98,$
                        BOTFIX=0.1,TOPFIX=0.1,$
                        LEFTFIX=0.03,RIGHTFIX=0.,$
                        BETWEEN=0,$
                        ORDER=order, Ncol = 1)

;-- Defines positions second of the plots in the page
nplot_page = 2*nmod
pos2 = fltarr(4,nplot_page)
pos2 = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.02,$
                        LEFTBORD=0.08,TOPBORD=0.98,$
                        BOTFIX=0.1,TOPFIX=0.1,$
                        LEFTFIX=0.03,RIGHTFIX=0.1,$
                        BETWEEN=0.2,$
                        ORDER=order, Ncol = nmod)


stat_result = replicate(0.,nstat,nmod,5)


;-- // LOOP on stations
FOR ns = 0,nstat-1 DO BEGIN

    !p.multi = [0,2,nmod,0]

    ;--- Defines first plot in the page
    if mean_flag then begin 
        DATA_M = reform(transpose(reform(DATA_MOD_MEAN(*,ns,*)),[1,0]))
        DATA_O = reform(DATA_OBS_MEAN(ns,*))
        model_loc = {date:time_counter, val:DATA_M}
        obs_loc = {date:time_counter, val:DATA_O}
    endif else begin
        DATA_M = reform(transpose(reform(DATA_MOD_NEW(*,ns,*)),[1,0]))
        DATA_O = reform(DATA_OBS(ns,0:NOBS_STAT(ns)-1))
        temp = reform(TIME_COUNTER_OBS(ns,0:NOBS_STAT(ns)-1))
        ccg_dec2date,temp,yr,mo,dy,hr,mn
        time_counter = julday(mo,dy,yr,hr,mn)
        model_loc = {date:time_counter, val:DATA_M}
        ;-
        obs_loc = {date:time_counter, val:DATA_O}
    endelse
    ;-
    par_plot.title = 'STATION :: '+station_name(ns)
    par_plot.symbol_mod = 0
    par_plot.add_mod_err = 0
    par_plot.add_leg_mod=1
    ;for nm = 0,nmod-1 do begin
    ;   par_plot.mod_leg(nm) =  strmid(model_filename(nm),1,strlen(model_filename(nm))-2)
    ;endfor
    par_plot.mod_leg = ['LMDZ-STD','LMDZ-OPT','TM3','TM5']
    par_plot.add_leg_obs=1
    par_plot.obs_leg = 'Observation'
    par_plot.xticks = 12
    par_plot.xminor = 2
    par_plot.position = pos1(*,0)
    par_plot.xtitle = 'Years 2002-2003'
    ;par_plot.xtitle = 'Year 2003'
    par_plot.ytitle = 'Radon concentration (Beq/m!D3!N) !C Daily mean ('+str_hour+') '
    if tracer_name(0) ne 'radon' then par_plot.ytitle = 'CO!D2!N concentration (PPM) !C Daily mean ('+str_hour+') '
    par_plot.xlabel=1
    par_plot.xrange = [julday_start1,julday_final1]
    par_plot.yrange = [peyl_min([reform(DATA_M,n_elements(DATA_M)),reform(DATA_O)],mask=missval),$
                       peyl_max([reform(DATA_M,n_elements(DATA_M)),reform(DATA_O)],mask=missval)]
    par_plot.color_mod = color_tot

    ;--- Plot proprement dit .    
    peyl_plottime, model_loc, obs_loc, par_plot

    FOR nm = 0 ,nmod-1 DO BEGIN

        ;--- Defines second plot in the page
        model_loc = {date:DATA_O, val:reform(DATA_M(*,nm))}
        obs_loc = {date:0, val:0}
        ;-
        par_plot.add_mod_err = 0
        par_plot.add_leg_mod=0
        par_plot.add_leg_obs=0
        par_plot.position = pos2(*,nmod+nm)
        par_plot.xlabel=0
        par_plot.xticks = 5
        par_plot.xminor = 2
        IF tracer_name(0) NE 'radon' THEN BEGIN
           par_plot.xrange=[355,400]
           par_plot.yrange=[355,400]
        ENDIF ELSE BEGIN
           par_plot.xrange=[0,8]
           par_plot.yrange=[0,8]
        ENDELSE
        par_plot.symbol_mod = -1
        par_plot.xtitle = 'OBSERVATION'
        par_plot.ytitle = 'MODEL' 
        par_plot.title = ''
        par_plot.color_mod = color_tot(nm)

        ;--- Plot proprement dit .    
        peyl_plottime, model_loc, obs_loc, par_plot

        ;- statictics
        tt = where(time_counter ge julday_start1 and time_counter le julday_final1,nn)
        ii = where(DATA_O(tt) ne missval,nn)
        if nn eq 0 then break
        stat_result(ns,nm,0:2) = (STATISTICS(reform((DATA_O(tt))(ii)),reform((DATA_M(tt,nm))(ii)),index=0))(0:2)
        NSD = VARIANCE(reform((DATA_M(tt,nm))(ii)))/VARIANCE(reform((DATA_O(tt))(ii)))
        stat_result(ns,nm,3) = NSD
        stat_result(ns,nm,4) = STDDEV(reform((DATA_O(tt))(ii)))
        ;-- Plot R squared, NSD and curve
        xx = pos2(0,nmod+nm)+0.08
        yy = 0.4
        xyouts,xx,yy,'R!U2!N    = '+auto_string(stat_result(ns,nm,2),2),orientation=0.,$
          charsize=par_plot.csize/2.5,charthick=par_plot.cthick,alignment=0.5,/normal
        yy = 0.38
        xyouts,xx,yy,'Slope = '+auto_string(stat_result(ns,nm,1),2),orientation=0.,$
          charsize=par_plot.csize/2.5,charthick=par_plot.cthick,alignment=0.5,/normal
        yy = 0.36
        xyouts,xx,yy,'NSD   = '+auto_string(NSD,2),orientation=0.,$
               charsize=par_plot.csize/2.5,charthick=par_plot.cthick,alignment=0.5,/normal
        ;-- PLot regression curve
        xxx = reform(reform((DATA_O(tt))(ii)))
        yyy = stat_result(ns,nm,1) * xxx + stat_result(ns,nm,0)
        oplot,xxx,yyy,thick = 2.,linestyle=2
        ;--- Plot de la droite 1:1
        xxx = par_plot.yrange(0)+indgen(11)*(par_plot.yrange(1)-par_plot.yrange(0))
        oplot,xxx,xxx,$
          color = 0,$
          thick = 1.5,$
          linestyle = 1

    ENDFOR


ENDFOR

ccg_closedev,dev='psc',saveas=file_ps

;-------------------
;  TAYLOR PLOTS
;-------------------

;-- Postscript file name
if mean_flag then begin
    xytitle = str_season+' daily '+mean_str+' data (' + str_hour + ')'
    ps_title = str_hour+'_'+mean_str
endif else begin
    xytitle = str_season+' 2002 data'
    ps_title = mean_str
endelse

file_ps = 'taylor_'+ps_title+'_'+str_season+'.ps'
;-- Open postscript file
ccg_opendev,dev='psc',saveas=path_ps + file_ps,portrait=0
CCG_RGBLOAD,file='/home/users/delage/lib/9color_table'
;loadct,39

!p.multi = [0,4,3,0]

;-- Tracé du diagramme de Taylor

nplot_page = nstat
pos = fltarr(4,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.02,$
                       LEFTBORD=0.08,TOPBORD=0.90,$
                       BOTFIX=0.1,TOPFIX=0.1,$
                       LEFTFIX=0.03,RIGHTFIX=0.1,$
                       BETWEEN=0.2,$
                       ORDER=order, Ncol = 4)

;-- // LOOP on stations
FOR ns = 0,nstat-1 DO BEGIN

   tt = 1.
   if station_name(ns) eq 'MHD' then  tt = 1.5
    TAYLOR, stat_result(ns,*,3), acos(stat_result(ns,*,2)), tt, max(acos(stat_result(ns,*,2))), undef=undef, position=pos(*,ns) $
      , sigma=sigma, half=half $;, code=['LMDZ standard','LMDZ thermique','TM5'] $
      , tickinterval=0.5 $
      , thick=2., charthick=2., charsize=0.8 $
      , csize=0.7, minor=2, symbol=1, symsize=1.5 ,color = color_tot $
      , title = station_name(ns) ,xtit='',ytit='' $
      ,otitle=otitle

    ;toto = "162B
    ;!XYOUTS,0.5,0.25,/data,'!4' + String(toto) + '!X!DOBS!N',charthick=2.5,charsize=1.2
                                ;XYOUTS,0.8,0.25,'=' +
                                ;auto_string(stat_result(ns,*,4),2),charthick=0.5,charsize=0.8
    XYOUTS,0.97,-0.03,/data,'X',charthick=2.5,charsize=1.2,col=24

ENDFOR

;-- Plot model legend
PEYL_LLEGEND,	x=0.65,y=0.2,$
  tarr=par_plot.mod_leg,larr=replicate(0,nmod),carr=color_tot,$
  sarr = replicate(1,nmod), farr = replicate(0,nmod), $
  charsize=1.5,$
  charthick=2.,$
  thick=3.,$
  incr_text=incr_text,$
  symsize=1.,$
  only_name=only_name,$
  dx_line = 0.01, $
  data=data

XYOUTS,0.5,0.95,/NORMAL,xytitle,charthick=2.,charsize=1.8,alignment=0.5

ccg_closedev,dev='psc',saveas=file_ps

;========================

;-- Postscript file name
file_ps = 'plot_inter_monthly-'+str_hour+'_'+mean_str+'.ps'
;-- Open postscript file
ccg_opendev,dev='psc',saveas=path_ps + file_ps,portrait=0
CCG_RGBLOAD,file='/home/users/delage/lib/9color_table'
;loadct,39

;-- Defines positions of the plots in the page
nplot_page = 12
pos = fltarr(4,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.1,$
                       LEFTBORD=0.08,TOPBORD=0.95,$
                       BOTFIX=0.1,TOPFIX=0.1,$
                       LEFTFIX=0.03,RIGHTFIX=0.,$
                       BETWEEN=0.2,$
                       ORDER=order, Ncol = 3)

;-- Defines time counter
time_counter = timegen(start=julday_start,final=julday_final,units='D')

;-- // LOOP on stations
FOR ns = 0,nstat-1 DO BEGIN

    !p.multi = [0,4,3,0]

    if mean_flag then begin 
        DATA_TMP = reform(DATA_MOD_MEAN(*,ns,*))
        DATA_TMP = reform(transpose(DATA_TMP,[1,0]))
        ;for gg = 0,nmod-1 do DATA_TMP(*,gg) = DATA_TMP(*,gg) - reform(DATA_OBS_MEAN(ns,*))
        model_loc = {date:time_counter, val:DATA_TMP}
        ;--
        DATA_O = reform(DATA_OBS_MEAN(ns,*))
        obs_loc = {date:time_counter, val:DATA_O}
    endif else begin
        DATA_TMP = reform(DATA_MOD_NEW(*,ns,*))
        DATA_TMP = reform(transpose(DATA_TMP,[1,0]))
        temp = reform(TIME_COUNTER_OBS(ns,0:NOBS_STAT(ns)-1))
        ccg_dec2date,temp,yr,mo,dy,hr,mn
        time_counter = julday(mo,dy,yr,hr,mn)
        model_loc = {date:time_counter, val:DATA_TMP}
        ;-
        DATA_O = reform(DATA_OBS(ns,0:NOBS_STAT(ns)-1))
        obs_loc = {date:time_counter, val:DATA_O}
    endelse

FOR nm = 0,23 DO BEGIN

    ;-- Defines time_counter
    nm_ptr = nm mod 12
    yr_ptr = nm/12
    julday_start2 = julday(nm_ptr + 1,1,2002+yr_ptr,12,0)
    julday_final2 = julday(nm_ptr + 1,jour(nm_ptr),2002+yr_ptr,12,0)
    ii = where(time_counter gt julday_start2 and time_counter le julday_final2,nn)
    if nn eq 0 then continue
    ;-
    par_plot.ypos_leg = [0.05,0.05,0.05,0.05,0.05]
    par_plot.title = '';time_name(nm_ptr)
    par_plot.symbol_mod = 0
    par_plot.add_mod_err = 0
    par_plot.add_leg_mod=1
    ;for nmm = 0,nmod-1 do begin
    ;    par_plot.mod_leg(nmm) =  strmid(model_filename(nmm),1,strlen(model_filename(nmm))-2)
    ;endfor
    par_plot.mod_leg =  ['LMDZ-STD','LMDZ-OPT','TM3','TM5']
    par_plot.add_leg_obs=1
    par_plot.obs_leg = 'Observation'
    par_plot.xticks = 4
    par_plot.xminor = 1
    par_plot.position = pos(*,nm_ptr)
    par_plot.xtitle = ''
    par_plot.ytitle = ''
    ;CO!D2!N concentration (PPM) !C Daily mean ('+auto_string(hour_start,2,nb_digit=2)+':00 - '+$
    ;auto_string(hour_end,2,nb_digit=2)+':00) '
    par_plot.xlabel=1
    par_plot.xrange = [julday_start2,julday_final2]
    par_plot. xtickformat = ['%D%M']
    par_plot.yrange = [peyl_min([reform(DATA_TMP(ii,*),n_elements(DATA_TMP(ii,*))),reform(DATA_O(ii))],mask=missval),$
                       peyl_max([reform(DATA_TMP(ii,*),n_elements(DATA_TMP(ii,*))),reform(DATA_O(ii))],mask=missval)]

    par_plot.ystyle = 2
    par_plot.yticks = 4
    par_plot.color_mod = color_tot

    ;--- Plot proprement dit .    
    peyl_plottime, model_loc, obs_loc, par_plot

    if nm_ptr eq 0 then begin
        xx = 0.5
        yy = 0.98
        xyouts,xx,yy,'STATION :: '+station_name(ns)+' Year '+auto_string(2002+yr_ptr,2),$
          orientation=0.,charsize=par_plot.csize,charthick=par_plot.cthick,alignment=0.5,/normal
        xx = 0.00
        yy = 0.5
        xyouts,xx,yy,'CO!D2!N concentration (PPM) !C Daily mean ('+str_hour+') ',$
          orientation=90.,charsize=par_plot.csize,charthick=par_plot.cthick,alignment=0.5,/normal
    endif


ENDFOR

ENDFOR
ccg_closedev,dev='psc',saveas=file_ps

endfor
endfor

END
