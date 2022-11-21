;+
;========================================================================
; NAME:
;       PEYL_STATION
;
; PURPOSE:
;       Function to get informations (lon lat lev) of a station that monitor
;	co2 and (or) o18.
;
; CALLING SEQUENCE:
;       result = peyl_station ( stat_name, /all, /noinfo)
;
; INPUTS:
;	stat_name : array of string with the name of the desired stations
;
; OPTIONAL INPUT PARAMETERS:
;	/all : set if all station wanted (overcome stat_name)
;	
;	/noinfo : to avoid to print out informations.
;
; OUTPUTS:
;	result : an array of structure (number = number of station)
;		 containing :
;	       { name : 'XXX'
;		 lon  : longitude in degre
;	         lat  : latitude in degre
;    	         lev  : vertical level in TM2 (indice)
;	       }
;       
; RESTRICTIONS:
; 
;
;==================================================================

FUNCTION peyl_station, stat_name, all=all, noinfo=noinfo, old_name=old_name

if keyword_set(noinfo) then info=0 else info =1
if (not keyword_set(old_name)) then old_name=0 else old_name =1


;-------- Define the output 
if (keyword_set(all)) then begin
   nb_stat=500
   stat_name=strarr(nb_stat)
endif else begin
                                ;--------- take upper case for
                                ;          stat_name
    stat_name = strupcase(stat_name)
    nb_stat   = n_elements(stat_name)
endelse


foo={name:'',$
     lon:999.,$
     lat:999.,$
     coord:'',$
     lev:999}
data=replicate(foo,nb_stat)
data(*).name = stat_name

;--------- open the good file
infile='/home/carbone/gcarbtm/modele/tm/sites/station_coord.txt'
if old_name then infile='/home/carbone/gcarbtm/modele/tm/sites/OLD/tri_nam_siteTM75'

if info then print, 'Observations readed from : ', infile
openr, unit, infile, /get_lun

;---------- Set the variable for reading.
lect_name ='' & lect_lon=0.
lect_lat=0.   & lect_lev=0    
lect = ''

;------------------------------------------------------
;	LECTURE DU FICHIER
;
degre     = '!9%!5'
stat_flag = replicate(0,nb_stat)
n = 0
compt = 0
while (not eof(unit)) do begin

		       ;---- lecture des caracteristiques d'un site
    readf,unit,lect
;    print,lect
    lect = strcompress(strtrim(lect,2))
    str = str_sep(lect,' ')
    lect_name = strupcase(str(0))
    lect_lon  = float(str(1))
    lect_lat  = float(str(2))
    lect_lev  = fix(str(3))

;;      readf,unit, $
;;	    FORMAT='(A3,x,f6.1,x,f5.1,x,i)', $
;;	    lect_name,lect_lon,lect_lat,lect_lev
;;;      print,lect_name,lect_lon,lect_lat,lect_lev

                       ;----- Test si le site est requis ? 
      if keyword_set(all) then data(n).name=lect_name
      ii = where(data.name eq lect_name, cc)
      if cc eq 0 then goto,suite_stat

                       ;------ Enregistrement du site
      if info then print,'Station ',lect_name,' readed'
      data(ii).lon = lect_lon
      data(ii).lat = lect_lat 
      data(ii).lev = lect_lev 
      latsign='N'
      if lect_lat lt 0 then latsign='S'
      lonsign='E'
      if lect_lon lt 0 then lonsign='W'
      data(ii).coord = '!5'+auto_string(nint(abs(lect_lat)),0)+degre+ $
                       latsign+', '+auto_string(nint(abs(lect_lon)),0)+ $
                       degre+lonsign

      compt=compt+cc  

      suite_stat: n = n + 1

endwhile

fin_lecture: 
free_lun,unit


;------ Print sites not found..
indice = where(data.lon eq 999., count)
if count gt 0 then begin
   if info then print,count,' Stations not found : ',data(indice).name
endif

return,data
END
;-





