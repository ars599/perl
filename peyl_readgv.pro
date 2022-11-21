;+
;==================================================================
;
;        FONCTION POUR LIRE GLOBALVIEW (une station donnee)
;
;==================================================================
FUNCTION  PEYL_READGV,obs_name,reseau_code=reseau_code,sampling=sampling,$
                      species_name=species_name,path=path,glv_old=glv_old

nobs = n_elements(obs_name)
ntimemax = 1800

if (not keyword_set(reseau_code)) then reseau_code=replicate('01',nobs) else begin
    if n_elements(reseau_code) eq 1 then reseau_code=replicate(reseau_code(0),nobs) 
endelse
if (not keyword_set(sampling)) then sampling=replicate('D0',nobs) else begin
    if n_elements(sampling) eq 1 then sampling=replicate(sampling(0),nobs) 
endelse
if (not keyword_set(species_name)) then species_name='co2'
if (not keyword_set(path)) then path='/home/gcarb/contrib/globalview/globalview_08/'
if (not keyword_set(glv_old)) then glv_old=0


out = replicate({lon:0.,lat:0.,alt:0.,$
                 date:dblarr(ntimemax), $
                 val:fltarr(ntimemax),$
                 val_real:fltarr(ntimemax),$
                 nval:0,$
                 zon:fltarr(ntimemax),$
                 diff:fltarr(ntimemax),$
                 nbdat_glo:0,$
                 rsd_glo:0.,$
                 nbdat:intarr(ntimemax),$
                 rsd:fltarr(ntimemax)},$
                nobs)

FOR NN=0,NOBS-1 DO BEGIN

;------------------------------------------------------------------
;       Ouverture du fichier des valeurs etendues..
;------------------------------------------------------------------

file = path + strlowcase(obs_name(nn)) + $
  '_' + reseau_code(nn) + sampling(nn) + $
  '_ext.'+species_name
                                ;--- Old version (before 2000...)
if glv_old  then begin
    file = path + strlowcase(obs_name(nn)) + '_' + reseau_code(nn) + '.ext'
endif


openr,u,file,/get_lun
lect = ''
for j=0,9 do readf,u,lect     ;-- entete
readf,u,lect     ;-- lat , lon, alt
lect = strcompress(strtrim(lect,2))
str=str_sep(lect,' ')
out(nn).lat = float(str(0))
out(nn).lon = float(str(1))
out(nn).alt = float(str(2))
for j=11,15 do readf,u,lect     ;-- entete

k=0
while (not eof(u)) do begin
    readf,u,lect
    lect = strcompress(strtrim(lect,2))
    str=str_sep(lect,' ')
    out(nn).date(k) = double(str(0))
    out(nn).val_real(k)  = float(str(1))
    out(nn).zon(k)  = float(str(2))
    out(nn).diff(k) = float(str(3))
    k=k+1
endwhile    
out(nn).val = out(nn).zon + out(nn).diff
out(nn).nval = k
free_lun,u


;------------------------------------------------------------------
;   Ouvertures du fichier de poids pour lire la standard deviation
;------------------------------------------------------------------

                                ;--- Nouvelle version (2000...)
file2 = path + strlowcase(obs_name(nn)) + $
  '_' + reseau_code(nn) + sampling(nn) + $
  '_wts.'+species_name

                                ;--- Old version (before 2000...)
if glv_old  then begin
    file2 = path + strlowcase(obs_name(nn)) + '_' + reseau_code(nn) + '.wts'
endif



openr,u2,file2,/get_lun
lect = ''
for j=0,15 do begin
    readf,u2,lect               ;-- entete
;    print,lect
endfor

;--- Lecture du rsd moyenn sur la periode dans cas GLview new (98,99,...)

if (glv_old ne 1) then begin
    readf,u2,lect
    lect = strcompress(strtrim(lect,2))
    str=str_sep(lect,' ')
    out(nn).rsd_glo   = float(str(1))
    out(nn).nbdat_glo = float(str(2))
;    print,lect
endif

;---- Lecture des rsd par annee..
while (not eof(u2)) do begin
    readf,u2,lect
    lect = strcompress(strtrim(lect,2))
    str = str_sep(lect,' ')
    date = fix(str(0))
    ii = where(fix(out(nn).date) eq date,cc)
    if cc lt 1 then message,'Probleme wts'
    out(nn).rsd(ii)   = float(str(1))
    out(nn).nbdat(ii) = float(str(2))
endwhile
free_lun,u2


ENDFOR 

return,out
END
;-

