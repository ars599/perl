;+
;========================================================================
; NAME:
;       PEYL_GETSIMUL_CO2
;
; PURPOSE: recupere les sorties d'une simulation LMDz (ou TM3) de CO2
;          pour un ensemble de points (lon, lat, alt, temps)
;          realise une interpolation verticale       
;
; CALLING SEQUENCE:
;       
;
; INPUTS:
;      plon0 : tableau des longitudes a extraire : taille n
;      plat0 : tableau des latitudes a extraire : taille n
;     
;
; OPTIONAL INPUT PARAMETERS:
;      time0 : tableau des dates a extraires (taille n)
;              FORMAT = intarr(4,n) avec [year, mois, jour, heure]
;              Par defaut toutes les dates de la simul seront extraites
;
;      pres : Niveaux de pressions a extraire (taille n)
;             unite = Pascal !!!
;             Interpolation selon niveaux du model
;             Par defaut tous les niveaux sont extraits
;
;      alt : Niveaux en metre qu dessus du sol a extraire (taille n)
;            sera converti en pression par formule de Holton
;                 en utilisant les pressions de surface du modele!!
;            Inactif si pres fournit
;            Par defaut tous les niveaux sont extraits
;
;      trac : Traceurs a extraires parmis plusieurs possibles 
;             (cf ci dessous)
;             Par defaut traceur de l'inversion
;
;   sort_time : 1 pour que les donnees soient triees en sortie par T
;               croissant (0= defaut)
;
;      path : path des fichiers model (Par defaut path valide)
;
;      grid_name : Nom de la grille du model : defaut LMDZ96
;
;
; OUTPUTS:
;
;      STRUCTURE contenant {data, date, datedec, lev, missval}
; 
;          dimension fonction de si all time, all level ou non
;                  - all time/level: fltarr(n,nlev,ntime,ntrac)
;                  - all time:       fltarr(n,ntime,ntrac)
;                  - all level:      fltarr(n,nlev,ntrac)
;                  - points:         fltarr(n,ntrac)
;
;          retourne missval pour les points non valide (hors domaine
;                   spatial ou temporel)
;       
; RESTRICTIONS:
; 
;
;========================================================================
@satel_lib
FUNCTION peyl_getsimul_co2, plon0, plat0, $
                            time=time0, $
                            alt=alt, pres=pres, $
                            trac=trac, $
                            path=path, $
                            sort_time=sort_time,$
			    new_sim = new_sim,$
                            add_offset = add_offset,$
			    correct_mlo = correct_mlo,$
			    grid_name=grid_name

;---------------------------------------------------------
;              Parametres
;---------------------------------------------------------

if n_elements(new_sim) eq 0 then new_sim = 2

file_glo = 1                    ;-- 0: only tropic; 1: global
coef_lmdz = 1.e6*29./12.        ;-- coef pour avoir des ppm
nlev_lmdz = 19
missval   = -999.

if not keyword_set(grid_name) then grid_name = 'LMDZ96'


;---- Traceurs a combiner par + ou - : inv_ctrl, inv_rew_reg,
;     inv_clair, turc_m_hourly, turc_m_month, taka_ocean,
;     inv_randerson(bbur), andres_fos90, andres_fos95, CASA_month,
;     CASA_day (m00x pour TM3)

if new_sim eq 0 then begin
    if not keyword_set(path) then path = '/home/atmosphere/idelkadi/simulations_lmdzt/lmdz967219reg/'
    trac_tab = [$
                 ['inv_ctrl','inv_ctrl']$
                 ,['inv_ctrl+turc_m_hourly-turc_m_month','inv_diurn']$
                 ,['turc_m_hourly-turc_m_month','turc_diu']$
                 ,['taka_ocean+andres_fos90+turc_m_hourly','oce_bio_fos']$
                 ,['inv_randerson','bbur']$
                 ,['m001+m002+m003','tm3']$
               ]
    date_sim_deb = [1984,1,1,0]
    date_sim_fin = [2002,12,31,24]
    if not keyword_set(correct_mlo) then correct_mlo = 1
    if not keyword_set(add_offset) then add_offset = 0
endif else if new_sim eq 1 then begin
    if not keyword_set(path) then path = ['/home/inversion/idelkadi/OLD_COCO2/']
    trac_tab = [$
                 ['inv_t3claire','inv_ctrl']$
                 ,['inv_t3claire+turc_m_hourly-turc_m_month','inv_diurn']$
                 ,['turc_m_hourly-turc_m_month','turc_diu']$
                 ,['bbur_rander','bbur']$
                 ,['inv_rodenbeck','inv_roden']$
                 ,['m001+m002+m003','tm3_new']$
               ]
    date_sim_deb = [1980,1,1,0]
    date_sim_fin = [2003,12,31,24]
    if not keyword_set(correct_mlo) then correct_mlo = 1
    if not keyword_set(add_offset) then add_offset = 0
endif else if new_sim eq 2 then begin
    if not keyword_set(path) then path = ['/home/carbone/peylin/COCO2/']
    trac_tab = [$
                 ['inv_ctrl','inv_ctrl']$
                 ,['inv_t3tot_smth','inv_t3']$
                 ,['turc_m_hourly-turc_m_daily','turc_diu']$
                 ,['bbur_rand_zmean','bbur']$
                 ,['inv_rodenbeck','inv_roden']$
                 ,['m001+m002+m003','tm3_new']$
               ]
    date_sim_deb = [1987,1,1,0]
    date_sim_fin = [2003,12,31,24]
    if not keyword_set(correct_mlo) then correct_mlo = 0
    if not keyword_set(add_offset) then add_offset = 1
endif
ntrac_tab = n_elements(trac_tab(0,*))

;---- Param de temps pour la simul

jour = [31,28,31,30,31,30,31,31,30,31,30,31]
nval_per_day = 8L
dtval = 3


;---------------------------------------------------------
;              Definition des variables
;---------------------------------------------------------

;---- Definition du nb de points initial

plon = plon0
plat = plat0
npt0 = n_elements(plon0)
if npt0 ne n_elements(plat0) then message,'lon and lat of different size !!'
                                ;--- tableau pour eliminer pts non
                                ;    possible (hors grille ou hors
                                ;    periode): npt au final
valid = lindgen(npt0)
npt = npt0


;---- Definition de la grille et restriction eventuelle au tropics 

if file_glo then begin
    file  = 'histrac.an'+ auto_string(date_sim_deb(0),0) + $
      '.m' + auto_string(date_sim_deb(1),0,nb_digit=2) + '.nc'
endif else begin
    print,'a checker....'
    stop
    file = 'tropic.an'+ auto_string(date_sim_deb(0),0) + $
      '.m' + auto_string(date_sim_deb(1),0,nb_digit=2) + '.nc'
endelse
for p=0,n_elements(path)-1 do begin
    if findfile(path(p)+file) ne '' then break
endfor
peyl_readnc,nav_lat,file=path(p)+file,var_name='nav_lat'
peyl_readnc,nav_lon,file=path(p)+file,var_name='nav_lon'

if 1 then begin
    grid = peyl_choixgrid(grid_name,noinfo=noinfo)
    nlon = grid.nlon
    nlat = grid.nlat
    lon  = grid.lon
    dlon = grid.dlon
    lat  = reverse(grid.lat)
    dlat = reverse(grid.dlat)
    dd   = grid.dlat(0)/2.
    if file_glo eq 0 then begin
        nn = n_elements(nav_lat(0,*))
        iilat_restrict = where(lat le nav_lat(0,0)+dd and lat ge nav_lat(0,nn-1)-dd, nlat)
        lat  = lat(iilat_restrict)
        dlat = dlat(iilat_restrict)
    endif
endif else begin
    lon  = nav_lon(*,0)
    dlon = lon-shift(lon,1) & dlon(0)=dlon(1)
    lat  = reform(nav_lat(0,*))
    dlat = shift(lat,1)-lat & dlat(0)=dlat(1)
    nlon = n_elements(lon)
    nlat = n_elements(lat)
endelse
nlev = 19
grid2 = {lon:lon,lat:lat,dlon:dlon,dlat:dlat}


;---- Choix du traceur

if not keyword_set(trac) then begin 
    trac = 'inv_ctrl'
    print,'Traceurs par defaut utilise..'
endif 
ntrac = n_elements(trac)
itrac = indgen(ntrac)
special_tm3 = 0
for n=0,ntrac-1 do begin
    ii = where(trac_tab(1,*) eq trac(n), cc)
    if cc ne 1 then begin
        print,'Erreur traceur; Choisir selon : '
        for t=0,ntrac_tab-1 do print,'   ',trac_tab(1,t),' = ',trac_tab(0,t)
        stop
    endif
    itrac(n) = ii(0)
    print,'-> traceurs : ',trac(n)
    if trac(n) eq 'tm3' or trac(n) eq 'tm3_new' then special_tm3 = 1
endfor

;----- Definition des positions des points et check limites
                                
pos_lmdz = peyl_getpos(grid_name,plon,plat,grid=grid2,/no_interpol)
pos_tm3  = peyl_getpos('TM3',plon,plat,/no_interpol)

if (not special_tm3 or (special_tm3 and ntrac gt 1)) then begin
    ii = where(pos_lmdz.lat eq -1 or pos_lmdz.lon eq -1, cc)
    if cc ne 0 then begin
        print,'PROBLEME: choix de point en dehors de la grille lmdz..',cc
        valid(ii) = -1
    endif
endif

;---- Definition Niveaux verticaux

special_lev = 0
if keyword_set(pres) or keyword_set(alt) then begin
    all_level = 0
    if keyword_set(pres) then begin
        if keyword_set(alt) then print,'!! Alt not use but Pressure instead !!'
        level = pres
    endif else begin
        print,'! Use altitude : EN Metre au dessus du SOL !'
        level = peyl_holton(alt,/reverse)
        special_lev = 1
    endelse
    nlevel = n_elements(level)
    if nlevel ne npt0 then message,'Erreur: nlevel different de nlon/nlat..'

endif else begin
    nlevel = nlev_lmdz
    all_level = 1
endelse

;----- Definition du debut et fin de la periode a extraire

ccg_date2dec,yr=date_sim_deb(0),mo=date_sim_deb(1),dy=date_sim_deb(2),$
  hr=date_sim_deb(3),mn=0.,dec=datedec_sim_deb
ccg_date2dec,yr=date_sim_fin(0),mo=date_sim_fin(1),dy=date_sim_fin(2),$
  hr=date_sim_fin(3),mn=0.,dec=datedec_sim_fin

if keyword_set(time0) then begin
    time = time0
    ss = size(time)
    if (ss(0) ne 1 and ss(0) ne 2) or $
      (ss(1) ne 4) then message,'time wrongly defined : should be [4,ntime]..'
    ntime = n_elements(time(0,*))
    if ntime ne npt0 then message,'ntime different nlon, nlat !! '
    date_deb = time(*,0)
    date_fin = time(*,ntime-1)
    all_time = 0    
                                ;--- Correction pour annees
                                ;    bisextille: NON prevu dans le
                                ;    model
    ii = where(time(1,*) eq 2 and time(2,*) eq 29, cc)
    if cc gt 0 then begin
        print,'Correction annees bisextille : 29/02 -> 28/02 !!!'
        time(2,ii) = 28
    endif
    ccg_date2dec,yr=time(0,*),mo=time(1,*),dy=time(2,*),hr=time(3,*),$
      mn=replicate(0.,ntime),dec=timedec

endif else begin
    date_deb = date_sim_deb
    date_fin = date_sim_fin
    all_time = 1
    print,'Extraction de la periode : ',datedec_sim_deb,' ',datedec_sim_fin
endelse

                                ;---- Check compatibilite temporelle..
if NOT all_time then begin
    ii = where(timedec lt datedec_sim_deb or timedec gt datedec_sim_fin, cc)
    if cc eq ntime then begin
        print,'Probleme : aucunes dates compatibles avec la simul...'
        print,'    ',datedec_sim_deb,' ',datedec_sim_fin
        stop
    endif
    if cc gt 0 then begin
        print,'Probleme dates hors simul: ',datedec_sim_deb,' ',datedec_sim_fin
        print,'  ==> Retrait des points : ', cc
        valid(ii) = -1
    endif
endif 

;----- Rejet des points non valides et definition des indices des
;      npt points a traiter

                                ;---- Check valid pts
ii_valid = where(valid ne -1, npt)
if npt eq 0 then message,'ERREUR : aucun point valide !!!!'


                                ;----- Sort data by time and redefine
                                ;      date_deb and date_fin; valid
                                ;      devient un tableau d'indice des
                                ;      pts valid dans tableau initial
sort_flag = 0
if NOT all_time then begin
    tt = timedec(ii_valid)
    ii_sort = sort(tt)
    if total(abs(tt(ii_sort)-tt)) gt 0. then begin
        print,'Time n"est pas dans ordre croissant !!! '
        if keyword_set(sort_time) then $
          print,'   => time will be sorted and reset on output.. !'
        sort_flag = 1
        ii_valid_orig = ii_valid
        ii_valid = ii_valid(ii_sort)
    endif
    date_deb = time(*,ii_valid(0))
    date_fin = time(*,ii_valid(npt-1))
endif 


;----- Definition fichiers model a lire

nyear     = date_fin(0) - date_deb(0) + 1
nmon      = 13-date_deb(1) + 12*(nyear-1) - (12-date_fin(1))  
date_file = intarr(nmon,2)
l = 0
for n=0,nyear-1 do begin
    mm = indgen(12) + 1
    if n eq 0 then mm = mm(date_deb(1)-1:11)
    if n eq nyear-1 then mm = indgen(date_fin(1)) + 1
    if nyear EQ 1 then mm = indgen(date_fin(1)-date_deb(1)+1)+date_deb(1)
    c = n_elements(mm)
    date_file(l:l+c-1,0) = date_deb(0)+n
    date_file(l:l+c-1,1) = mm
    l = l+c
endfor
print,'Lecture des fichiers: '+auto_string(date_file(0,0),0)+'/'+auto_string(date_file(0,1),0)+$
  ' -> '+auto_string(date_file(nmon-1,0),0)+'/'+auto_string(date_file(nmon-1,1),0)


;---- Definition des variables de sorties

ntmax = 0L
for n=0,nmon-1 do ntmax = ntmax + jour(date_file(n,1)-1)*nval_per_day

if all_level and all_time then begin
    data = replicate(missval,npt0,nlevel,ntmax,ntrac) 
    date = replicate(fix(missval),4,ntmax) 
    vert_lev = replicate(missval,npt0,nlevel)
endif else if all_level then begin
    data = replicate(missval,npt0,nlevel,ntrac) 
    date = replicate(fix(missval),4,npt0) 
    vert_lev = replicate(missval,npt0,nlevel)
endif else if all_time then begin
    data = replicate(missval,npt0,ntmax,ntrac) 
    date = replicate(fix(missval),4,ntmax) 
    vert_lev = replicate(missval,npt0)
endif else begin 
    data = replicate(missval,npt0,ntrac)
    date = replicate(fix(missval),4,npt0)
    vert_lev = replicate(missval,npt0)
endelse

;---------------------------------------------------------
;   Lecture Pression, delta_P de LMDZ
;---------------------------------------------------------

;---- Definition de pressions mid lev et Psurf : On prend la moyenne
;     des 12 mois..

if 0 then begin
    file = '/home/inca/didier/Runs/Reference/histrac.pmid-monthly.nc'
    peyl_readnc,p_mid,file=file,var_name='pmid'
    p_mid = total(p_mid,4)/n_elements(p_mid(0,0,0,*))
    peyl_readnc,p_surf,file=file,var_name='ps'
    p_surf = total(p_surf,3)/n_elements(p_surf(0,0,*))
    if file_glo eq 0 then begin
        p_mid  = p_mid(*,iilat_restrict,*)
        p_surf = p_surf(*,iilat_restrict)
    endif
;help,p_mid,p_surf

endif else begin
    file = '/home/atmosphere/idelkadi/simulations_lmdzt/start.nc'

                                ;-- lecture P_surf (retrait de la
                                ;   longitude additionnelle) et
                                ;   moyenne temporelle
    peyl_readnc,p_surf,file=file,var_name='ps'
    p_surf = p_surf(0:nlon-1,*,*)
    if (size(p_surf))(0) eq 3 then $
      p_surf = total(p_surf,3)/n_elements(p_surf(0,0,*))
    if file_glo eq 0 then begin
        p_surf = p_surf(*,iilat_restrict)
    endif
    
                                ;-- Coef Bp et Ap
    peyl_readnc,AP,file=file,var_name='ap'
    peyl_readnc,BP,file=file,var_name='bp'
    if n_elements(ap) ne nlev+1 or n_elements(bp) ne nlev+1 then message,'Probleme pression..'
    
                                ;-- Calcul pression niveaux interm et Dp
    p_interm = fltarr(nlon,nlat,nlev+1)
    delta_p  = fltarr(nlon,nlat,nlev)
    for n=0,nlev do begin
        p_interm(*,*,n) = bp(n) * p_surf + ap(n) 
        if n gt 0 then delta_p(*,*,n-1) = p_interm(*,*,n-1)-p_interm(*,*,n)
    endfor
    
                                ;-- Calcul pression mid 
    p_mid = fltarr(nlon,nlat,nlev)
    for n=0,nlev-1 do p_mid(*,*,n) = (p_interm(*,*,n)+p_interm(*,*,n+1)) / 2.
 
endelse

;---------------------------------------------------------
;       Boucle generale sur les fichiers mensuels model
;---------------------------------------------------------

t1  = 0L
np1 = 0L
for im=0L,nmon-1 do begin

                                ;--- Loop pour construire differents
                                ;    traceurs.. 
    for itr=0,ntrac-1 do begin
        itr2 = itrac(itr)
        print,' traceur : ',trac_tab(1,itr2)

                                ;--- Construction du traceur (CAS
                                ;    SPECIAL TM3)
        if (trac_tab(1,itr2) eq 'tm3' or trac_tab(1,itr2) eq 'tm3_new')then begin
            nval_per_day_tm3 = 4L
            tm3_new = 0
            if trac_tab(1,itr2) eq 'tm3_new' then tm3_new = 1 

            model = LECTURE_TM3(date_file(im,*),trac_tab(0,itr2),tm3_new,$
                                add_offset=1,correct_mlo=0)

            pos = pos_tm3
            ntmod = n_elements(model.idate(*,0))
	ddt = model.idate(1,3)-model.idate(0,3)
	      if tm3_new eq 0 then ddt = 12

        endif else begin

            model = LECTURE_LMDZ(date_file(im,*),trac_tab(0,itr2),path,$
                                add_offset=add_offset,correct_mlo=correct_mlo)
      ntmod = jour(date_file(im,1)-1)*nval_per_day
            if ntmod ne n_elements(model.idate(*,0)) then message,'probleme temps..'

            pos       = pos_lmdz
            ddt       = dtval         
        endelse
	  idate_mod = transpose(model.idate)
        
                                ;--- Extraction des points pour tts
                                ;    altitudes et ts les temps...
        if all_level and all_time then begin
            for no=0L,npt-1 do begin
                no2 = ii_valid(no)
                data(no2,*,t1:t1+ntmod-1,itr) = model.val(pos.lon(no2),pos.lat(no2),*,*)
                vert_lev(no2,*) = p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),*)
            endfor 
            date(*,t1:t1+ntmod-1) = idate_mod

                                ;--- Extraction des points pour tts
                                ;    les temps...
        endif else if all_time then begin
            for no=0L,npt-1 do begin
                no2 = ii_valid(no)

                                ;-- Define pressure level and limit it
                                ;   to model range..
                if special_lev then $
                  vert_lev(no2) = peyl_holton(alt(no2),/noinfo,/reverse,$
                            psurf=p_surf(pos_lmdz.lon(no2),pos_lmdz.lat(no2))) $
                else vert_lev(no2) = level(no2)
                vert_lev(no2) = vert_lev(no2) < $
                  p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),0)
                vert_lev(no2) = vert_lev(no2) > $
                  p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),nlev_lmdz-1)

                for t=0L,ntmod-1 do data(no2,t1+t,itr) = $
                  interpol(model.val(pos.lon(no2),pos.lat(no2),*,t), $
                           p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),*), $
                           vert_lev(no2))
            endfor 
            date(*,t1:t1+ntmod-1) = idate_mod
                
                                ;--- Extraction d'un temps pour chaque
                                ;    points et eventuellement tts
                                ;    niveau.. 
        endif else begin
            kk = 0L
            for no=np1,npt-1 do begin
                no2 = ii_valid(no)
                ii = where(idate_mod(0,*) eq time(0,no2) and $
                           idate_mod(1,*) eq time(1,no2) and $
                           idate_mod(2,*) eq time(2,no2) and $
                           idate_mod(3,*) ge time(3,no2)-ddt, cc)
                if cc eq 0 then goto,suite_date1

                if all_level then begin
                    data(no2,*,itr) = model.val(pos.lon(no2),pos.lat(no2),*,ii(0))
                    vert_lev(no2,*) = p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),*)

                endif else begin	
                                ;-- Define pressure level and limit it
                                ;   to model range..
                    if special_lev then $
                      vert_lev(no2) = peyl_holton(alt(no2),/noinfo,/reverse,$
                             psurf=p_surf(pos_lmdz.lon(no2),pos_lmdz.lat(no2),0)) $
                    else vert_lev(no2) = level(no2)
                    vert_lev(no2) = vert_lev(no2) < $
                      p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),0)
                    vert_lev(no2) = vert_lev(no2) > $
                      p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),nlev_lmdz-1)

                    data(no2,itr) = interpol(model.val(pos.lon(no2),pos.lat(no2),*,ii(0)),$
                                             p_mid(pos_lmdz.lon(no2),pos_lmdz.lat(no2),*),$
                                             vert_lev(no2))

                endelse
                date(*,no2) = idate_mod(*,ii(0))
                kk = kk + 1
            endfor
            suite_date1:
        endelse


    endfor                       ;--- Fin boucle traceur
    if all_time then t1 = t1 + ntmod else np1 = np1 + kk


endfor                           ;--- Fin boucle sur les mois..


                                ;--- Re-sort the data to the initial
                                ;    order !! unless not requested
                                ;    (REM: valid = ii_valid(ii_sort))
if keyword_set(sort_time) and sort_flag then begin
    data(ii_valid_orig,*,*,*) = data(ii_valid,*,*,*)
    date(*,ii_valid_orig) = date(*,ii_valid)
    vert_lev(ii_valid_orig,*) = vert_lev(ii_valid,*)
endif

                                ;--- Compute decimal date..
ii = where(date(0,*) ne missval, cc)
datedec = replicate(double(missval),n_elements(date(0,*)))
ccg_date2dec,yr=date(0,ii),mo=date(1,ii),dy=date(2,ii),hr=date(3,ii),$
  mn=replicate(0.,cc),dec=dec
datedec(ii) = dec

return,{data:data,$
        date:date,$
        datedec:datedec,$
        lev:vert_lev, $
        missval:missval $
       }
END
;-
