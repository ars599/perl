;+
;=====================================================================
; NAME:
;       PEYL_ROBS
;
; PURPOSE:
;       Function that read the observations of CO2 or O18 at
;	the station location of the NOAA, CIO, CSIRO, and OTHER 
;	for some specific years.
;	The datas are under /home/data02/peylin/DATA/*
;
; CALLING SEQUENCE:
;       result = peyl_robs (stat_name, year=year, reseau=reseau, 
;		      type=type, path=path, /noinfo)		      
;
; INPUTS:
;	stat_name : name of the station.
;
;	year : array specifiing the years that we want 
;	       either 19xx or xx; default: all year available
;
;
; OPTIONAL INPUT PARAMETERS:
;	reseau : string of 'noaa' 'cio' or 'csiro' or 'other'
;		 to choose the network; default: 'noaa' 
;
;	type : either 'co2' or 'o18' for the type of data (default :
;	co2)
;
;       path : path for observation file to read 
;
;	/noinfo : to avoid message print out 
;
; OUTPUTS:
;	result : a structure 
;	     {
;		name : the name of the station
;		lon  : the longitude of the station (-180 <-> 180)
;		lat  : the latitude of the station (-90 <-> 90)
;		lev  : the level of the station in TM2
;		year : same array as year in input (years of the data)
;
;            }
;
; RESTRICTIONS:
; 
;
;======================================================================

FUNCTION peyl_robs, stat_name, $
                    year=year, $
                    date_orig = date_orig, $
                    type=data_type, $
                    path=path, $
                    noinfo=noinfo

if keyword_set(noinfo) then info=0 else info=1
if keyword_set(date_orig) then date_orig=1 else date_orig=0
if (not keyword_set(data_type)) then type='co2' else type=data_type
if (not keyword_set(year)) then year=-1

if type eq 'del' then type='o18'

;----- Look for years asked..
if year(0) lt 0 then begin
                 ;-- arbitrary large range ...
   year_asked = 1960 + indgen(40)  
endif else begin
   year_asked = year
   if year_asked(0) lt 1900 then year_asked = fix(year_asked + 1900)
   if ((year_asked(0) lt 1950) or (year_asked(n_elements(year_asked)-1) $
     gt 1996)) then begin
      print,'ERREUR : year invalide ',year_asked
      stop
   endif
endelse
nb_year=n_elements(year_asked)

max_number=999.


;----------------------------------------------------------
;           READ THE GOOD FILE
;----------------------------------------------------------
;
statname = strlowcase(stat_name)
if (not keyword_set(path)) then begin
   path = '/home/data02/peylin/DATA/'
   path = path + 'NOAA' + '/ccgvu/'
endif 
infile   = path + statname + '.'
infile   = infile + type + '.all'
if date_orig then begin
    infile = infile + '.td' 
    nb_var=14
endif else begin
    infile = infile + '.ev'
    nb_var=10
endelse

ccg_fread,file=infile,nc=nb_var,var

if var(0,0) lt 1900 then var(0,*)=var(0,*)+1900.
size_var = size(var)
nb_date  = size_var(2)

;----------------------------------------------------------
;	EXTRACT GOOD YEARS
;----------------------------------------------------------
;

indice_date = [-1]
year_found  = [-1]
for i=0,nb_year-1 do begin
    ii = where (fix(var(0,*)) eq year_asked(i), cc)
    if cc gt 0 then begin
       indice_date = [indice_date,ii]
       year_found  = [year_found,year_asked(i)]
    endif
endfor

nb_year_found = n_elements(year_found) - 1
if nb_year_found le 0 then begin
   print,'YEARS asked not found in the data : ', $
          fix(var(0,0)),'--',fix(var(0,nb_date-1))
   stop
endif

year_found = year_found(1:*)
indice_date = indice_date(1:*)
if info then print,'Years used : ',year_found

var = var(*,indice_date)

;----------------------------------------------------------
;	DEFINE THE INDICE IN THE ARRAY VAR..
;----------------------------------------------------------
;

ind = {ori:1, func:2, poly:3, smooth:4, trend:5, detrend:6, $
       smooth_cyc:7, cyc_harm:8, resi_func:9, smooth_resi:10, $
       trend_resi:11, resi_smooth:12, growth:13 }

if not date_orig then $
  ind = {ori:-1, func:1, poly:2, smooth:3, trend:4, detrend:-1, $
         smooth_cyc:5, cyc_harm:6, resi_func:-1, smooth_resi:7, $
         trend_resi:8, resi_smooth:-1, growth:9 }

;--------------------------------------------------------------------
;        READ STATISTIQUE FILE FOR SPECIAL STD_DEV..
;----------------------------------------------------------
;

statfile = path + statname + '.'
statfile = statfile + type + '.stat'
openr,u,statfile,/get_lun
text=''
while not(eof(u)) do begin
    readf,u,text
;;    print,text
    pos = strpos(text,'Residual standard deviation about function:')
    if pos ne -1 then begin
        resid_func_std_dev = float((str_sep(text, ':'))(1))
    endif
    pos = strpos(text,'Residual standard deviation about smooth curve:')
    if pos ne -1 then begin
        resid_smooth_std_dev = float((str_sep(text, ':'))(1))
    endif
endwhile
free_lun,u


;----------------------------------------------------------
;	DEFINE THE OUTPUT
;----------------------------------------------------------
;

stat = peyl_station(statname,/noinfo)

indnew = {ori:0, func:1, poly:2, smooth:3, trend:4, detrend:5, $
       smooth_cyc:6, cyc_harm:7, resi_func:8, smooth_resi:9, $
       trend_resi:10, resi_smooth:11, growth:12 }
if not date_orig then $
  indnew = {ori:-1, func:0, poly:1, smooth:2, trend:3, detrend:-1, $
         smooth_cyc:4, cyc_harm:5, resi_func:-1, smooth_resi:6, $
         trend_resi:7, resi_smooth:-1, growth:8 }

data = {name:statname,$
        lon:stat.lon,$
        lat:stat.lat,$
        lev:stat.lev,$
        year:year_found,$
        date:reform(var(0,*)),$
        val:var(1:*,*), $
        resid_smooth_std_dev:resid_smooth_std_dev,$
        resid_func_std_dev:resid_func_std_dev,$
        ind : indnew $
       }

return,data
END
;-





