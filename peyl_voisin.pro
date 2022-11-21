;+
;=========================================================================
; NAME:
;       PEYL_VOISIN
;
; PURPOSE:
;       Program that extrapol for a geophysical field the data that
;	are mask to an average value of the unmask surrounding datas.
;	The field has to be : (lon, lat, "mon")
;	If there is 3 dimension (mon) then each month are treated..
;	
; CALLING SEQUENCE:
;       peyl_voisin, ff, valmask, aout
;
; INPUTS:
;       ff : array of (lon, lat, "mon") of the geophysical field.
;
;	valmask : value of the array ff that correspond to masked datas. 
;
; OPTIONAL INPUT PARAMETERS:
;	/noinfo : to avoid to print out informations.
;
; OUTPUTS:
;	aout : same array than ff but with no more masked data.
;       
; RESTRICTIONS:
;	The interpolation take into account only the adjacent neighbours
;
;=========================================================================

PRO peyl_voisin, ff, valmask, aout, noinfo=noinfo

if (not keyword_set(noinfo)) then print,'PASSAGE DANS PLUS-PROCHE-VOISIN'

;--- Recherche des dims..
s=size(ff)
if (s(0) gt 3 or s(0) lt 2) then begin
   print,'ERREUR VOISIN : wrong size of data',s
   stop
endif
lon=s(1)
lat=s(2)
if s(0) eq 3 then nmon=s(3) else nmon=1

;--- Copy du tableau initial dans l'output...
aout=ff
aa = fltarr(lon,lat,8)
nb_pt = fltarr(lon,lat,8) 

;---- Boucle sur les mois...
for n=0,nmon-1 do begin
    if (not keyword_set(noinfo)) then print,'mois :',n

;--- Copy local 2-D
    if s(0) eq 3 then local_aout=ff(*,*,n) else local_aout=ff

    WHILE (1) do begin

;--- test de sortie..
	 i_mask = where(local_aout eq valmask, cc)
	 if cc eq 0 then goto, fin

;------------ Define the neighbours..
;
;		      aa7   aa3   aa4
;		      aa2  (i,j)  aa0 
;		      aa6   aa1   aa5
;
;---- 4 plus proches voisins (un cote commun) :
;
         aa(*,*,0) = shift(local_aout,-1,0)
	 aa(*,*,1) = shift(local_aout,0,1)  & aa(*,0,1)=aa(*,1,1)
	 aa(*,*,2) = shift(local_aout,1,0)
	 aa(*,*,3) = shift(local_aout,0,-1) & aa(*,lat-1,3)=aa(*,lat-2,3)

;
;---- Les 4 voisins avec un angle commun :
;
         aa(*,*,4) = shift(local_aout,-1,-1) & aa(*,lat-1,4)=aa(*,lat-2,4)
	 aa(*,*,5) = shift(local_aout,-1,1)  & aa(*,0,5)=aa(*,1,5)
	 aa(*,*,6) = shift(local_aout,1,1)   & aa(*,0,6)=aa(*,1,6)
	 aa(*,*,7) = shift(local_aout,1,-1)  & aa(*,lat-1,7)=aa(*,lat-2,7)

;
;--------- Calcul de la valeur moyenne avec plus ou moins de voisin..

	 nb_pt(*)=1.
	 ii = where (aa eq valmask)
	 aa(ii) = 0.
	 nb_pt(ii) = 0.
	 moy = division (total(aa,3), total(nb_pt,3), valmask)
	 local_aout(i_mask) = moy(i_mask)

    endwhile     ; fin du WHILE.... 

;---- Set the output for this month : n
    fin:
    if s(0) eq 3 then aout(*,*,n)=local_aout else aout=local_aout

endfor     ;--- fin de la boucle sur les mois..

END
;-
        









