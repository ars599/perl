@monthly_mean
@peyl_plottime
;
;/////////////////////////////////////////////////
;/////////////////////////////////////////////////
;
PRO MAIN 

;-- Main parameters
path_mod ='/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/modeloutput/'
;path_mod = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/temp/'
path_obs = '/home/satellites/delage/PROJETS/LMDZ/INTERFACE/DATA_FILES/'
path_ps  = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/ps_files/'
path_restore = '/home/satellites/delage/PROJETS/LMDZ/TRANSCOM3/ps_files/'
restore_file = 1
nday = 730
nobs_max = 100000
ntime_in = 17520
missval = 1E+20
nskip = 35
;-- Defines time variables
time_name = ['JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY',$
             'AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER']
jour      = [31,28,31,30,31,30,31,31,30,31,30,31]
;-- Defines julian date 
julday_start = julday(1,1,2002,0,30)
julday_final = julday(12,31,2003,23,30)
julyear_beg = 2002.
julyear_end = 2004.

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
;model_name = ['LMDZ4.LSCE.2007-09-06','LMDZ4_OPTIM.LSCE.2007-11-08','TM3_vfg.BGC.2005-11-23','TM5_glb3x2.ESRL.2006-03-21','TM5_eur1x1.SRON.2006-03-24']
;model_filename = ['.LMDZ4.LSCE.','.LMDZ4_OPTIM.LSCE.','.TM3_vfg.BGC.','.TM5_glb3x2.ESRL.','.TM5_eur1x1.SRON.']
;model_name = ['LMDZ.LSCE.2006-08-02','LMDZ4_Ttke.LSCE.2007-12-11','LMDZ_KZ.LSCE.2007-12-11','TM5_glb3x2.ESRL.2006-03-21']
;model_filename = ['.LMDZ.LSCE.','.LMDZ4_Ttke_opt.LSCE.','.LMDZ_KZ.LSCE.','.TM5_glb3x2.ESRL.']
model_name = ['LMDZ4_Ttke.LSCE.2007-12-11','LMDZ4_Ttke_Therm.LSCE.2007-12-11','TM3_fg.BGC.2005-11-23','TM5_glb3x2.ESRL.2006-03-21']
model_filename = ['.LMDZ4_Ttke_opt.LSCE.','.LMDZ4_Ttke_Therm_Nt_opt.LSCE.','.TM3_fg.BGC.','.TM5_glb3x2.ESRL.']
;model_name = ['LMDZ.LSCE.2006-08-02','LMDZ_THERM.2006-02-10','LMDZ4_Ttke.LSCE.2007-12-11','LMDZ4_Ttke_Therm.LSCE.2007-12-11','LMDZ_KZ.LSCE.2007-12-11']
;model_filename = ['.LMDZ.LSCE.','.LMDZ_THERM.LSCE.','.LMDZ4_Ttke_opt.LSCE.','.LMDZ4_Ttke_Therm_Nt.LSCE.','.LMDZ_KZ.LSCE.']
color_tot = [1,4,10,22];[20,40,100,220,0]
;color_tot = [50,100,150]
nmod =  n_elements(model_name)
nobs = 1
;-- Defines tracers to use
tracer_name = ['fossil98','CASA','Taka02']
;tracer_name = ['fos','sib_hr','taka']
ntracer = n_elements(tracer_name)
;-- Defines stations to use
station_name = ['PRS','CMN','PUY','SCH','MHD','PAL','HUN115','CBW200','HEI']
data_filename = ['prs_21C0_ext.co2','cmn_17C0_ext.co2',$
                 'puy_21C0_ext.co2','sch_23C0_ext.co2',$
                 'mhd_11C0_ext.co2','pal_30C0_ext.co2',$
                 'hun0115_35C3_ext.co2','cbw0200_52C3_ext.co2',$
                 'hei_22C0_ext.co2'$
                ]

; station_name = ['PRS','CMN','KZD','PUY','SCH','PAL','HUN010','CBW020','MHD','HEI','WES']
; data_filename = ['prs_21C0_ext.co2','cmn_17C0_ext.co2','kas_53C0_ext.co2',$
;                  'puy_21C0_ext.co2','sch_23C0_ext.co2','pal_30C0_ext.co2',$
;                  'hun0010_35C3_ext.co2','cbw0020_52C3_ext.co2','mhd_11C0_ext.co2',$
;                  'hei_22C0_ext.co2','wes_23C0_ext.co2'$
;                 ]


nstat = n_elements(station_name)

;-- Allocate standard deviation array
DATA_STD   = replicate(0.,nstat,nmod,2,2,2)
DATA_STD_O = replicate(0.,nstat,2,2,2)
DATA_STD_NORM = replicate(0.,nstat,nmod,2,2,2)

;-- Loop on period of mean
for hh = 0,1 do begin

if hh eq 0 then begin 
    hour_start = -2
    hour_end = 3
endif else begin
    hour_start = 12
    hour_end = 17
endelse

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
    peyl_readnc,tmp,var_name = 'site_name',file=file_name1
    statname_model = string(tmp)
    for i = 0,n_elements(statname_model)-1 do statname_model(i) = strtrim(statname_model(i),2)
    ;if nm eq 4 then statname_model = ['CBW020','CBW200','MHD','SCH']
    ;-- Read tracer name
    peyl_readnc,tmp,var_name = 'tracer_name',file=file_name1
    tracername_model = string(tmp)
    for i = 0,n_elements(tracername_model)-1 do tracername_model(i) = strtrim(tracername_model(i),2)
    ;-- Read concentration
    peyl_readnc,VAR1,var_name = 'conc',file=file_name1
    peyl_readnc,VAR2,var_name = 'conc',file=file_name2

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
                    ;-- first year (2002)
                    DATA_MOD(nm,ns,0:8759) = DATA_MOD(nm,ns,0:8759) + VAR1(ptr_stat,ptr_tracer,*)
                    ;-- second year (2003)
                    DATA_MOD(nm,ns,8760:17519) = DATA_MOD(nm,ns,8760:17519) + VAR2(ptr_stat,ptr_tracer,*)
                ENDIF ELSE BEGIN
                    print,'PB ::',tracer_name(nf)
                    stop
                ENDELSE

            ENDFOR

        ENDIF ELSE BEGIN
            print,'PB ::',station_name(ns)
            print,model_name(nm)
            stop
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
        ;IF station_name(ns) EQ 'FRD' THEN BEGIN
        ;   cobs = 2
        ;   line(cobs-1) = '.'
        ;ENDIF
        ;IF ns GE 8 THEN BEGIN
        ;   cobs = 10
        ;ENDIF
        IF strmid(line(cobs-1),0,1) EQ '.' OR strmid(line(cobs-1),0,1) EQ '?' OR strmid(line(cobs-1),0,1) EQ '#' THEN BEGIN

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
;  DAYTIME MEAN
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

file_save = path_restore+'lmdz_intercomp-'+auto_string(hour_start,nb_digit=2)+':00-'+auto_string(hour_end,nb_digit=2)+':00'+'.gz'
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
  yrange: [0.,0.],$             ;--- [0,0] for automatic definition else the range
  max_number:missval-1,$            ;--- maximum number to account for in the ranges
  ythick:2,$                    ;--- y thickness
  xthick:2,$                    ;--- x thickness
  cthick:2,$                    ;--- character thickness
  csize:0.8,$                   ;--- character size
  color_text:0,$                ;--- color of the text
  seuil:0,$                 ;--- seuil maximum des err prise en compte (0 sinon)..
;
;---- AXIS
ystyle:1,$                      ;--- 1 exact yrange, 2 extended range
  xstyle:1,$                    ;--- 1 exact yrange, 2 extended range
  xlabel : 1,$
  xtickformat : ['%M'],$
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
  ytitle: 'Daytime mean ('+auto_string(hour_start,2)+':00 - '+auto_string(hour_end,2)+':00) CO!D2!N concentration (PPM)',$              ;--- Titre en Y
  title: '',$                   ;--- Major Title
  xpos_title:0.5,$              ;--- [0,1] : abscisse for title posi. if coord_title= Normal/data
  ypos_title:0.98,$              ;--- [0,1] : ordonnee for title posi. if coord_title= Normal/data
  coord_title:' ',$             ;--- 'Normal': for normal coordi, 'data' for data coord else standard above the graph
  csize_title:1.,$             ;--- character size of major title
  cthick_title:2,$              ;--- character thick of major title
  footnote: '',$                ;--- String for footnote at bottom of page..
;
;---- MODEL
add_mod:replicate(1,nplot),$    ;--- to add each component or not
  add_mod_err:replicate(0,nplot),$ ;-- 1 to add model std_dev; 2 Mean error on the side !
  color_mod:[0,0,0,50,150,200],$
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
  color_obs:240,$
  lines_obs:replicate(0,nobs),$
  thick_obs:replicate(3,nobs),$   ;--- global thickness for obs
  thick_obs_err:replicate(2,nobs),$ ;--- thickness for obs error bar
  symbol_obs: replicate(0,nobs),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
  symtype_obs:replicate(3,nobs),$ ;--- obser. symbol
  symsize_obs:replicate(1.5,nobs),$ ;--- obser. symbol size ; 1.5
  symfill_obs:replicate(1,nobs),$ ;--- obser. symbol filling
;
;---- LEGEND
add_leg_mod:replicate(0,nplot),$
  add_leg_obs:replicate(0,nobs),$
  mod_leg:replicate('',nplot),$
  obs_leg:replicate('',nobs),$
  ;xpos_leg: [0.2, 0.4, 0.7, 0.8],$        ;-- X position of the legs in Normal/DATA coord.
  ;ypos_leg: [0.8, 0.03, 0.03, 0.03],$        ;-- Y position of the legs "
  xpos_leg: [0.2],$        ;-- X position of the legs in Normal/DATA coord.
  ypos_leg: [0.9],$        ;-- Y position of the legs "
  nleg_col: 1,$                           ;-- Nb of colone (xpos_leg) for the legs used
  coord_leg:'normal',$                    ;-- Normal or Data coordonates
  incr_text_leg:1.,$                      ;-- Scaling for incremental Dy between text leg
  csize_leg  : 1.,$                       ;-- character size
  cthick_leg : 2 $                        ;-- character thick
}


;-- Defines time_counter
steps = 5
time_counter_5D = timegen(start=julday_start,final=julday_final,units='D',step_size=steps)
caldat,time_counter_5D,mo5,dy5,yr5,hr5,mn5
time_counter_D = timegen(start=julday_start,final=julday_final,units='D')
caldat,time_counter_D,mo,dy,yr,hr,mn

;-- Treat model data
;-
FOR ns = 0,nstat-1 DO BEGIN
    ii = where( DATA_MOD_MEAN(0,ns,*) ne missval,nn)

    mean_tot_obs = peyl_mean(DATA_OBS_MEAN(ns,ii),mask=missval)

    FOR nm = 0 ,nmod-1 DO BEGIN
        mean_tot_mod = peyl_mean(DATA_MOD_MEAN(nm,ns,ii),mask=missval)
        DATA_MOD_MEAN(nm,ns,ii) = DATA_MOD_MEAN(nm,ns,ii) - mean_tot_mod + mean_tot_obs
        
        
    ENDFOR
ENDFOR
;--
DATA_MOD_MONTHLY = monthly_mean(DATA_MOD_MEAN,nval_per_day=1,nmon=24,$
                                year_deb=2002,month_deb=1,nday_incr=steps,$
                                missval=missval)
DATA_OBS_MONTHLY = monthly_mean(DATA_OBS_MEAN,nval_per_day=1,nmon=24,$
                                year_deb=2002,month_deb=1,nday_incr=steps,$
                                missval=missval)

;-- Postscript file name
file_ps = 'plot_monthlytrend_'+auto_string(hour_start,nb_digit=2)+':00-'+auto_string(hour_end,nb_digit=2)+':00'+'.ps'
;-- Open postscript file
ccg_opendev,dev='psc',saveas=path_ps + file_ps,portrait=0
CCG_RGBLOAD,file='/home/users/delage/lib/9color_table'
;loadct,39


;-- Defines positions of the first plots in the page
nplot_page = 2
pos = fltarr(4,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.02,$
                        LEFTBORD=0.08,TOPBORD=0.98,$
                        BOTFIX=0.1,TOPFIX=0.2,$
                        LEFTFIX=0.03,RIGHTFIX=0.,$
                        BETWEEN=0,$
                        ORDER=order, Ncol = 1)

;-- // LOOP on stations
!p.multi = [0,nplot_page,1,0]
wint_mon = where(mo5 eq 11 or mo5 eq 12 or mo5 eq 1 or mo5 eq 2)
summ_mon = where(mo5 eq 6 or mo5 eq 7 or mo5 eq 8 or mo5 eq 9)
wint_day = where(mo eq 11 or mo eq 12 or mo eq 1 or mo eq 2)
summ_day = where(mo eq 6 or mo eq 7 or mo eq 8 or mo eq 9)

FOR ns = 0,nstat-1 DO BEGIN

    ;-- Compute diference
    DATA_M = reform(transpose(reform(DATA_MOD_MONTHLY(*,ns,*)),[1,0]))
    DATA_D = reform(transpose(reform(DATA_MOD_MEAN(*,ns,*)),[1,0]))
    for gg = 0,nmod-1 do begin
        oo = where(reform(DATA_OBS_MONTHLY(ns,*)) ne missval,pp)
        DATA_M(oo,gg) = (DATA_M(oo,gg) - reform(DATA_OBS_MONTHLY(ns,oo))) 
        oo = where(reform(DATA_OBS_MEAN(ns,*)) ne missval,pp)
        DATA_D(oo,gg) = (DATA_D(oo,gg) - reform(DATA_OBS_MEAN(ns,oo))) 
        ;-- Compute winter summer std deviation
        good = where(reform(DATA_M(wint_mon,gg) ne missval)) & DATA_STD(ns,gg,0,hh,0) = sqrt(peyl_mean((reform(DATA_M(wint_mon,gg)))(good)^2))
        good = where(reform(DATA_M(summ_mon,gg) ne missval)) & DATA_STD(ns,gg,1,hh,0) = sqrt(peyl_mean((reform(DATA_M(summ_mon,gg)))(good)^2))
        good = where(reform(DATA_D(wint_day,gg) ne missval)) & DATA_STD(ns,gg,0,hh,1) = sqrt(peyl_mean((reform(DATA_D(wint_day,gg)))(good)^2))
        good = where(reform(DATA_D(summ_day,gg) ne missval)) & DATA_STD(ns,gg,1,hh,1) = sqrt(peyl_mean((reform(DATA_D(summ_day,gg)))(good)^2))
    endfor

    good = where(reform(DATA_M(wint_mon,0) ne missval)) & DATA_STD_O(ns,0,hh,0) = STDDEV((DATA_OBS_MONTHLY(ns,wint_mon))(good))
    good = where(reform(DATA_M(summ_mon,0) ne missval)) & DATA_STD_O(ns,1,hh,0) = STDDEV((DATA_OBS_MONTHLY(ns,summ_mon))(good))
    good = where(reform(DATA_D(wint_day,0) ne missval)) & DATA_STD_O(ns,0,hh,1) = STDDEV((DATA_OBS_MEAN(ns,wint_day))(good))
    good = where(reform(DATA_D(summ_day,0) ne missval)) & DATA_STD_O(ns,1,hh,1) = STDDEV((DATA_OBS_MEAN(ns,summ_day))(good))

    ;DATA_O = reform(DATA_OBS_MONTHLY(ns,*))
    DATA_O = 0.
    model_loc = {date:time_counter_5D, val:DATA_M}
    obs_loc = {date:time_counter_5D, val:DATA_O}
    ;-
    par_plot.title = 'STATION :: '+station_name(ns)
    par_plot.symbol_mod = 0
    par_plot.add_mod_err = 0
    par_plot.add_leg_mod=1
    par_plot.mod_leg = ['LMDZ-STD','LMDZ-OPT','TM3','TM5']
    ;for gg = 0,nmod-1 do begin
    ;    par_plot.mod_leg(gg) = strmid(model_filename(gg),1,strlen(model_filename(gg))-2)
    ;endfor
    par_plot.add_leg_obs=1
    par_plot.obs_leg = 'Observation'
    par_plot.xticks = 24
    par_plot.xminor = 1
    par_plot.position = pos(*,2*ns mod nplot_page)
    par_plot.xtitle = 'Years 2002-2003'
    par_plot.ytitle = 'CO!D2!N concentration (PPM) !C '+auto_string(steps,0)+' day- mean ('+auto_string(hour_start,2,nb_digit=2)+':00 - '+$
      auto_string(hour_end,2,nb_digit=2)+':00) '
    par_plot.xlabel=1
    par_plot.xrange = [julday_start,julday_final]
    ;par_plot.yrange = [peyl_min([reform(DATA_M,n_elements(DATA_M)),reform(DATA_O)],mask=missval),$
    ;                   peyl_max([reform(DATA_M,n_elements(DATA_M)),reform(DATA_O)],mask=missval)]
    par_plot.yrange = [peyl_min(DATA_M,mask=missval),$
                       peyl_max(DATA_M,mask=missval)]
    ;print,par_plot.yrange
    ;par_plot.yrange = [-5.,5.]
    par_plot.color_mod = color_tot

    ;--- Plot proprement dit .    
    peyl_plottime, model_loc, obs_loc, par_plot


    ;------------------
    ;------------------

    ;DATA_O = reform(DATA_OBS_MONTHLY(ns,*))
    DATA_O = 0.
    model_loc = {date:time_counter_D, val:DATA_D}
    obs_loc = {date:time_counter_D, val:DATA_O}
    ;-
    par_plot.title = ''
    par_plot.position = pos(*,(2*ns +1) mod nplot_page)
    par_plot.ytitle = 'CO!D2!N concentration (PPM) !C Daytime mean ('+auto_string(hour_start,2,nb_digit=2)+':00 - '+$
      auto_string(hour_end,2,nb_digit=2)+':00) '
    par_plot.xrange = [julday_start,julday_final]
    par_plot.yrange = [peyl_min(DATA_D,mask=missval),$
                       peyl_max(DATA_D,mask=missval)]

    ;--- Plot proprement dit .    
    peyl_plottime, model_loc, obs_loc, par_plot

ENDFOR


ccg_closedev,dev='psc',saveas=file_ps

;-- normalize by sigma obs
for gg = 0,nmod-1 do begin
    DATA_STD_NORM(*,gg,*,hh,*) = DATA_STD(*,gg,*,hh,*)/DATA_STD_O(*,*,hh,*) * 100.
endfor

endfor

norm = 0
if norm eq 1 then  DATA_STD = DATA_STD_NORM

!p.multi = [0,2,1,0]

;-- Postscript file name
file_ps = 'mod-obs_stddev.ps'
if norm eq 1 then  file_ps = 'mod-obs_stddev_normalisee.ps'
;-- Open postscript file
ccg_opendev,dev='psc',saveas=path_ps + file_ps,portrait=0
CCG_RGBLOAD,file='/home/users/delage/lib/9color_table'
;loadct,39

;-- Defines positions of the plots in the page
nplot_page = 2
pos = fltarr(4,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.12,$
                       LEFTBORD=0.02,TOPBORD=0.98,$
                       BOTFIX=0.05,TOPFIX=0.05,$
                       LEFTFIX=0.,RIGHTFIX=0.,$
                       BETWEEN=0.1,$
                       ORDER=order, Ncol = 1)


for kk = 0,1 do begin
for ii = 0,1 do begin

str_mean = 'Daily-mean'
if kk eq 0 then str_mean = auto_string(steps,0)+'-day means)'
    

if ii eq 0 then begin
    str_sea = 'WINTER'
    tit =  'Standard Deviation of MODEL minus OBSERVATION differences ('+str_mean
endif else begin
    str_sea = 'SUMMER'
    tit = ''
endelse

;--
ymin = 0.
ymax0 = 20.;max(DATA_STD(*,*,ii,*,kk))
if norm eq 1 then ymax0 = 150.;max(DATA_STD(*,*,ii,*,kk))
;--
PLOT, [0,1], [0,1] , position = pos(*,ii) $
  ,xrange = [0,nstat+1],yrange=[ymin,ymax0] $
  ,thick=1,color=0,min_value=-200 $
  ,ystyle=1,yminor=2,/ynozero $
  ,xthick=3,ythick=3,charthick=2,charsize=1.2 $
  ,xcharsize=1. $
  ,ycharsize=1. $
  ,xticks=nstat+1 $
  ,title=tit $
  ,ytitle='CO!D2!N Concentrations (PPM)' $
  ,xtickname=[' ',station_name, ' '] $
  ,xstyle=1 $
  ,/NODATA

XYOUTS,1.,ymin+3*(ymax0-ymin)/4.,str_sea,charthick=2,charsize=3.


xwide = 1./(3*nmod+1)
xmin = 7.5/(3*nmod+1)

FOR ns = 0, nstat-1 DO BEGIN

    FOR nm = 0, nmod-1 DO BEGIN
        xmax = xmin + xwide
        ymin = 0.
        ymax = DATA_STD(ns,nm,ii,0,kk) < ymax0
        polyfill,[xmin,xmin,xmax,xmax],[ymin,ymax,ymax,ymin],color=color_tot(nm)
        oplot,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],color=0,thick=3
        ;--
        xmin = xmax
        xmax = xmin + xwide
        ymin = 0.
        ymax = DATA_STD(ns,nm,ii,1,kk) < ymax0
        polyfill,[xmin,xmin,xmax,xmax],[ymin,ymax,ymax,ymin],color=color_tot(nm),$
          /line_fill,spacing=0.1,orientation=45,thick=4
        oplot,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],color=0,thick=3
        ;--
        xmin = xmax + xwide
    ENDFOR
    if norm ne 1 then begin
    tmax = peyl_max(DATA_STD(ns,*,ii,*,kk)) < (ymax0 - 8)
    polyfill,[ns+1-2*xwide,ns+1-2*xwide,ns+1+2*xwide,ns+1+2*xwide],[tmax + 2 ,tmax + 4,tmax + 4,tmax + 2],color=0
    oplot,[ns+1-2*xwide,ns+1+2*xwide,ns+1+2*xwide,ns+1-2*xwide,ns+1-2*xwide],[tmax + 2 ,tmax + 2,tmax + 4,tmax + 4,tmax + 2],color=0,thick=3
    oplot,[ns+1-2*xwide,ns+1+2*xwide,ns+1+2*xwide,ns+1-2*xwide,ns+1-2*xwide],[tmax + 4 ,tmax + 4,tmax + 6,tmax + 6,tmax + 4],color=0,thick=3
    xyouts,ns+1,tmax + 2.8,auto_string(DATA_STD_O(ns,ii,0,kk),1),charthick=2,charsize=0.8,alignment=0.5,color=256
    xyouts,ns+1,tmax + 4.8,auto_string(DATA_STD_O(ns,ii,1,kk),1),charthick=2,charsize=0.8,alignment=0.5
    toto = "162B
    xyouts,ns+1,tmax + 7,'!4' + String(toto) + '!X!DOBS!N = ',charthick=2,charsize=1.,alignment=0.5
    endif
    ;--
    xmin = xmin + xwide
ENDFOR

endfor

;-- legend
xmin = 0.1
xmax = 0.13
ymin = 0.03
ymax = 0.06
polyfill,[xmin,xmin,xmax,xmax],[ymin,ymax,ymax,ymin],/normal
XYOUTS,xmax+0.01,ymin+(ymax-ymin)/4.,'Night time',charthick=2,charsize=1.2,/normal
;-
xmin = 0.3
xmax = 0.33
ymin = 0.03
ymax = 0.06
polyfill,[xmin,xmin,xmax,xmax],[ymin,ymax,ymax,ymin],$
          /line_fill,spacing=0.1,orientation=45,thick=4,/normal
plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],thick=3,/normal
XYOUTS,xmax+0.01,ymin+(ymax-ymin)/4.,'Day time',charthick=2,charsize=1.2,/normal

;-- Plot model legend
PEYL_LLEGEND,	x=0.5,y=0.06,$
  tarr=par_plot.mod_leg,larr=replicate(0,nmod),carr=color_tot,$
  sarr = replicate(1,nmod), farr = replicate(1,nmod), $
  charsize=1.2,$
  charthick=2.,$
  thick=3.,$
  incr_text=incr_text,$
  symsize=1.,$
  only_name=only_name,$
  dx_line = 0.01, $
  data=data

endfor

;-- Close postscript file
ccg_closedev,dev='psc',saveas=file_ps


END
