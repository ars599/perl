;+
;==================================================================
; NAME:
;       PEYL_mean_cycle
;
; PURPOSE:
;       Calculate the annual mean cycle from a one dimensional array
;       supplied with the associated date (decimal) : it averages 
;       the data to have only a specific number of mean per year.
;       It computes also the mean cycle over one year.
;
; CALLING SEQUENCE:
;       result = peyl_mean_cycle (arr, date, nb_val, /noinfo )
;
; INPUTS:
;       arr    : array to process (one dimension)
;       date   : associated decimal date (to each value of arr)
;       nb_val : number of mean values per year
;
; OPTIONAL INPUT PARAMETERS:
;      /noinfo : to suppress messages.
;
; OUTPUTS:
;       result = structure containing :
;	       { 
;		 new_val(nb_tot)  : New data average over a period of
;		                    time of 365./nb_val jours..
;		 new_std(nb_tot)  : Std_dev of New data (newval)
;		 new_date(nb_tot) : Decimal date of the new data
;		 new_ind(nb_tot)  : Nb de valeur ayant et moyennee
;		 mean_val(nb_val) : Mean seasonal cycle 
;		 mean_std(nb_val) : Mean std_dev for each meanval
;		 mean_date(nb_val): Decimal date of mean Seasonal cycle
;		 mean_ind(nb_val) : Nb de valeur ayant et moyennee
;		}
;       REM : nb_tot = nb_val * nombre_annee
;
; RESTRICTIONS:
; 
;
;==================================================================
FUNCTION PEYL_MEAN_CYCLE, val, date, nb_val_year, noinfo=noinfo, $
                          round=round, interpole=interpole, allyear=allyear,$
                          nomean=nomean

if keyword_set(noinfo) then info=0 else info=1
if keyword_set(allyear) then allyear=1 else allyear=0
if (not keyword_set(interpole)) then interpole=0 
if (not keyword_set(round)) then round=-4
if (not keyword_set(nomean)) then nomean=0

                                ;---- Definition des jours de l'annee
jour_mois   = [31,28,31,30,31,30,31,31,30,31,30,31]
jour_cumul  = intarr(13)
for n=0,11 do jour_cumul(n+1) = total(jour_mois(0:n))

;---------------------------------------------------------------------
;                   Calcul preliminaires 
;---------------------------------------------------------------------

;-------- Verification des dimensions des tableaux...

nval  = n_elements(val)
ndate = n_elements(date)
if nval ne ndate then begin
   print,'ERREUR MEAN CYCLE : wrong dimensions..',nval,ndate 
   stop
endif


;-------- Rounding of the date values ....  to avoid
;         problem.... by default -2 !!!!!!!

;;;;date = ccg_round(date, round)


;-------- Definition des annees de debut et de fin 
year_deb  = fix(date(0))
year_fin  = fix(date(ndate-1))
nb_year   = year_fin - year_deb + 1
if nb_year lt 0 then begin
   print,'ERREUR MEAN CYCLE : wrong date...',date
   stop
endif

;------ Dates finale possible centrees, ainsi que les dates
;       encadrantes..

delta         = double(1./nb_val_year)
new_date      = year_deb + (findgen(nb_year*nb_val_year)+0.5D) * delta
new_date_inf  = new_date - (delta/2.)
new_date_sup  = new_date + (delta/2.)

;----- On arondi les dates !!! a creuser!!!!
;;;;;;; ???
;;new_date_inf = ccg_round(new_date_inf, round)
;;new_date_sup = ccg_round(new_date_sup, round)
;;new_date     = ccg_round(new_date, round)

;------ Recherche des dates (finales) a traiter reeelement 

dd = delta/5.

ii_inf = where(new_date_inf lt (date(0)+dd), cc1)
ii_sup = where(new_date_sup gt (date(ndate-1)-dd), cc2)
i1=0 
if cc1 gt 0 then i1=ii_inf(cc1-1)
i2=n_elements(new_date)-1
if cc2 gt 0 then i2=ii_sup(0)

if allyear then begin
   i1=0
   i2=n_elements(new_date)-1
endif

new_date     = new_date(i1:i2)
new_date_inf = new_date_inf(i1:i2)
new_date_sup = new_date_sup(i1:i2)
ndate_new    = i2-i1+1


;---------------------------------------------------------------------
;        Calcul du champs nouveaux avec nb_val_year / year
;---------------------------------------------------------------------


;----- Definitions des variable de sortie
new_val   = fltarr(ndate_new)
new_std   = fltarr(ndate_new)
new_ind   = intarr(ndate_new)
new_indreel = intarr(ndate_new)


;------ On interpole aux bonnes dates (pour les cas ou pas assez de
;       donnees pour faire une moyenne)..

if interpole then begin   
   new_val = interpol(val, date, new_date)
   new_ind(*) = 1
   new_indreel(*) = 1
   goto, suite1
endif
    

;------ On prend la moyenne des valeurs dans chaque inteval definit
;       par les nouvelle dates...

bord_flag=0
for i=0,ndate_new-1 do begin

                                ;--- Recherche des dates qui
                                ;    correspondent aux valeur dans
                                ;    l'interval considere.. en
                                ;    incluant la valeur inf sauf si
                                ;    elle a deja ete prise (cas
                                ;    bord_flag=1)
                                ;  
    if bord_flag then $
       ii = where(date gt new_date_inf(i) and date lt new_date_sup(i), cc)$
    else $
       ii = where(date ge new_date_inf(i) and date lt new_date_sup(i), cc)
          
                                ;--- Si pas de valeurs on etend la
                                ;    recherche a la borne sup...
    bord_flag = 0
    if cc eq 0 then begin
       ii2 = where(date eq new_date_sup(i), cc2)
       if cc2 gt 0 then begin
          ii = ii2
          cc = cc2
          bord_flag = 1
       endif
    endif
                                ;--- Calcul des valeurs moyenne et
                                ;    de la deviation standart...
    if cc eq 0 then begin

                                ;--- Pas de donnee: on recherche les
                                ;    valeurs encadrant l'interval
                                ;    considere (1 ou 2 valeurs)
       iibis=-1
       ii1 = where( date le new_date_inf(i), cc1)
       ii2 = where( date ge new_date_sup(i), cc2)
       if cc1 gt 0 then iibis=[iibis,ii1(cc1-1)] 
       if cc2 gt 0 then iibis=[iibis,ii2(0)]
       iibis = iibis(1:*)
       ccbis = n_elements(iibis)

                                ;--- Calcul des valeurs moyenne et
                                ;    de la deviation standart...
       res = peyl_moment(val(iibis),sdev=sdev)
       new_val(i) = res(0)
       new_std(i) = sdev
       new_ind(i) = ccbis
       new_indreel(i) = 0

                                ;--- Warning for user...
       if info then begin
          print,'Warning Mean Cycle: no data for period: ',$
                      new_date_inf(i),new_date_sup(i),$
                      '  => date les + proches: ', date(iibis)
          print,'Il faudrait plutot interpoler les donnees !!'
       endif
       
    endif else begin
        res = peyl_moment(val(ii), sdev=sdev)
        new_val(i) = res(0)
        new_std(i) = sdev
        new_ind(i) = cc
        new_indreel(i) = cc
    endelse

endfor

suite1:

;---------------------------------------------------------------------
;        Calcul du champs moyen annuel avec nb_val_year 
;---------------------------------------------------------------------


year = nint((year_deb+year_fin)/2.)
mean_date  = (findgen(nb_val_year)+0.5)*delta + year

mean_date_inf    = mean_date - (delta/2.) - year
mean_date_sup    = mean_date + (delta/2.) - year

;----- On arondi les dates !!! a creuser!!!!
;;; ??
;;mean_date_inf    = ccg_round(mean_date_inf, round)
;;mean_date_sup    = ccg_round(mean_date_sup, round)
;;mean_date        = ccg_round(mean_date, round)

;------ Definition d'une date sans les annees...
date_bis     = ccg_round (date - fix(date), round)
new_date_bis = ccg_round (new_date - fix(new_date), round)


;----- Definitions des variable de sortie
mean_val   = fltarr(nb_val_year)
mean_std   = fltarr(nb_val_year)
mean_ind   = intarr(nb_val_year)


;----- Cas special ou l'on ne veut pas les sortie moyenne annuelle:
;      SPECIAL pour le cas ou l'on a pas une annee complete...

if nomean then goto, suite_mean


;------ Dans le cas de l'interpolation on travaille sur les valeurs
;       precedemment interpolees.

date_loc = date_bis
val_loc  = val
if interpole then begin
   date_loc = new_date_bis
   val_loc  = new_val
endif


;------ Boucle sur toutes les nouvelles valeurs.
for i=0,nb_val_year-1 do begin

                                ;--- Recherche des dates qui
                                ;    correspondent aux valeur dans
                                ;    l'interval considere quelque soit
                                ;    l'annee....
    ii = where(date_loc ge mean_date_inf(i) and $
               date_loc lt mean_date_sup(i), cc)


                                ;--- Pas de donnee: On refait la
                                ;    recherche avec les valeurs
                                ;    precedement recalculees...
    if (cc eq 0 and interpole eq 0) then begin       
       date_loc = new_date_bis
       val_loc  = new_val
       ii = where (date_loc ge mean_date_inf(i) and $
                   date_loc lt mean_date_sup(i), cc) 
    endif

    if cc eq 0 then begin
       print,'Big erreur dans mean_cycle : Pas de valeurs pour periode : '
       print,mean_date_inf(i),mean_date_sup(i)       
       stop
    endif
                                ;--- Calcul des valeurs moyenne et
                                ;    de la deviation standart...
    res = peyl_moment(val(ii), sdev=sdev)
    mean_val(i) = res(0)
    mean_std(i) = sdev
    mean_ind(i) = cc

endfor

suite_mean:

;---------------------------------------------------------------------
;           Definition de Output
;---------------------------------------------------------------------

out = { new_val   :new_val,$
        new_std   :new_std,$
        new_date  :new_date,$
        new_ind   :new_ind,$
        new_indreel   :new_indreel,$
        mean_val  :mean_val,$
        mean_std  :mean_std,$
        mean_date :mean_date,$
        mean_ind  :mean_ind $
      }

return,out

END
;-

