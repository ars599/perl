;===========================================================================
;  Fonction pour enlever Extra-lon et remettre flux Sud-Nord
;===========================================================================
FUNCTION REFORMAT_FLUX, tempo_var, tempo_lat, nlon
tempo_var = tempo_var(0:nlon-1,*,*)
if tempo_lat(0) gt 0 then begin
    tempo_var = reverse(tempo_var,2,/overwrite)
    print,' Inversion N-S...'
endif
return,tempo_var
END

;===========================================================================
;                Recompose une carte de flux
;         A partir d'un run inverse et des distributions de flux.
;    !!!!!!!!      UNITE  KgC / M2 / HEURE   !!!!!!!!!!!!
;===========================================================================
FUNCTION PEYL_RECOMPOSE_FLUX, file_in=file_in, $
                              path_in=path_in, $
                              path_base=path_base, $
                              flag_reg=flag_reg, $
                              file_base=file_base,$
                              var_name_flux=var_name_flux,$
                              var_name_lat=var_name_lat,$
                              var_name_time=var_name_time,$
                              flux_per_surf=flux_per_surf,$
                              precis_flux=precis_flux,$
                              source_to_use=source_to_use, $
                              source_to_remove=source_to_remove, $
                              reg_to_remove=reg_to_remove, $
                              proc_to_remove=proc_to_remove, $
                              grid_name=grid_name, $
                              split_flux=split_flux, $
                              add_prior=add_prior,$
                              add_err=add_err,$
                              add_sealand_mask=add_sealand_mask,$
                              add_station=add_station,$                              
                              add_journal=add_journal



;--------------------------------------------------------------------------
;        Parametres Generaux : default case
;--------------------------------------------------------------------------

if not ccg_vdef(add_journal) then add_journal = 1
if not ccg_vdef(add_station) then add_station = 0

if not ccg_vdef(add_sealand_mask) then add_sealand_mask = 0
if not ccg_vdef(split_flux) then split_flux = 0
if not ccg_vdef(add_prior) then add_prior = 0
if not ccg_vdef(add_err) then add_err = 0


;------ Definition de l'inversion et du path des fichiers flux tel que
;       injecte dans le model

if not ccg_vdef(path_in) then path_in = ['/home/carbone/peylin/INVERSION/TRANSCOM3/']
if not ccg_vdef(file_in) then $
    message,'You should enter a file name for the inversion result'

if not ccg_vdef(path_base) then path_base = ['/home/geoatm/carouge/FLUX/']


                                ;-- 0 pour cas generique, 1: Claire
                                ;   reg standard, 2: Claire T3-Level1
if not ccg_vdef(flag_reg) then flag_reg = 1

if not ccg_vdef(file_base) then begin
    if flag_reg eq 0 then message,'Flag_reg = 0 : Fournir file_base !'
;;    file_base = [['fco2.taka.oce.glo.fy95.m.lmdz96.nc','oce'],['...','nep']]
endif

if not ccg_vdef(var_name_flux) then var_name_flux= ['fco2','FCO2','flux','FLUX']
if not ccg_vdef(var_name_lat) then var_name_lat = ['lat','LAT','latitude','LATITUDE']
if not ccg_vdef(var_name_time) then var_name_time = ['time','TIME','date','DATE']

if not ccg_vdef(flux_per_surf) then flux_per_surf = 1

                                ;-- Precision en GtC/mois du flux en
                                ;   dessous de laquelle on prendra une
                                ;   distribution plate!
if not ccg_vdef(precis_flux) then precis_flux = 1.e-4


;---- SI ON VEUT SOURCES PARTICULIERES (source.name)
;source_to_use = ['oce.glo','nep.glo','fos.glo']
if not ccg_vdef(source_to_use) then source_to_use = ['all']

;---- Nom de source a ne pas prendre en compte (source.name)
;source_to_remove = ['oce.glo','nep.glo','fos.glo']
if not ccg_vdef(source_to_remove) then source_to_remove = ['none']

;---- Nom de region a ne pas prendre en compte (source_base.reg)
;reg_to_remove = ['sindo','sinde']
if not ccg_vdef(reg_to_remove) then reg_to_remove = ['none']

;---- Nom de processus a ne pas prendre en compte (source_base.proc)
;proc_to_remove = ['fos','oce']
if not ccg_vdef(proc_to_remove) then proc_to_remove = ['none']


;----------------------------------------------------------------
;       Restore fichier et Definitions de quelques variables
;----------------------------------------------------------------

;--- Definition d'un journal pour checker les sorties

if add_journal then journal,'recompose_journal.txt'


;--- RESTORE FICHIER et def Nb de mois (nmon_inv pour old code sinon
;    ntime_inv_max) 

for p=0,n_elements(path_in)-1 do begin
    if findfile(path_in(p)+file_in) ne '' then break
endfor
if p ge n_elements(path_in) then begin
    print,'file not found in the different path : ',file_in
    stop
endif

print,' Restore ',file_in
restore,path_in(p)+file_in

if n_elements(nmon_inv) eq 0 then nmon = max(source(*).nbcrenau) else nmon = nmon_inv 
if n_elements(date_mod) eq 0 then begin
    ii = where(source(*).nbcrenau eq nmon,cc)
    if cc eq 0 then message,'Probleme dans definition date_mod!'
    date_out = source(ii(0)).date_crenau(0:nmon-1)
endif else date_out = date_mod
date_out_bis = date_out - fix(date_out)

dt_out = date_out(1) - date_out(0) 
if abs(dt_out - 1./12.) gt 0.001 then $
  message,'fichier restorer non mensuel !! CAS A IMPLEMENTER '

if n_elements(model) ne 0 then begin
    if not ccg_vdef(grid_name) then grid_name = model $
    else begin
        if model ne grid_name then print,'!WARNING, grid_name redefinit a ',grid_name
        grid_name = model
    endelse
endif else $
  if not ccg_vdef(grid_name) then message,'Pas de variable model: il faut definir grid_name!'


;--- Definition des dimensions / tableaux

grid = peyl_choixgrid(grid_name,/noinfo)
nlon = grid.nlon
nlat = grid.nlat

jour_mois = [31,28,31,30,31,30,31,31,30,31,30,31]
coef_kg2gt =  1.e-12 * 24. * jour_mois ;-- from kg/h to Gt/month
coef_gt2kg =  1.e12 / 24. / jour_mois ;-- from Gt/month to kg/h 

                                ;--- Separation land/ocean/fos/...
if split_flux ge 1 then begin
    temp = source.proc_comp(0)
    ii = sort(temp)
    uu = uniq(temp(ii))
    if n_elements(uu) gt 4 then message,'bizarre + de 4 processus!!'
    trac_tab = temp(ii(uu))
    foo = where(trac_tab ne 'pixel',cc)
    if cc eq 0 then trac_tab = ['land','oce'] $
    else trac_tab =['land','oce',trac_tab(foo)]
    ntrac = n_elements(trac_tab)
    print,'flux splitted as ',trac_tab
                                ;-- all fluxes together
endif else begin
    ntrac = 1                       
    trac_tab = 'glo'
endelse

if add_err then nfield = 2 else nfield = 1

poste_out  = replicate(0.,nlon,nlat,nmon,ntrac,nfield)
if add_prior then prior_out =  replicate(0.,nlon,nlat,nmon,ntrac,nfield)
if add_sealand_mask then mask = replicate(0,nlon,nlat)


;----------------------------------------------------------------
;                DEBUT DES CALCULS
;----------------------------------------------------------------

                                ;--- definition des regions land et
                                ;    ocean pour la grille..
if split_flux ge 1 then begin
    indoce = peyl_choixregnew_back('oce', grid_name=grid_name,$
                              noinfo=1, extend_reg=0,$
                              grid_pixel=grid)
    indland = peyl_choixregnew_back('land', grid_name=grid_name,$
                               noinfo=1, extend_reg=0,$
                               grid_pixel=grid)
    regoce  = strsplit(indoce.name,'+',/extract)
    regland = strsplit(indland.name,'+',/extract)
endif

;--------------- Boucle sur toutes les sources
flag_use = 0
for ns=0,nsrce-1 do begin

                                ;--- Determination des regions de base

    nbcrenau = source(ns).nbcrenau
    nbcomp = source(ns).nbcomp
    iicomp_base = source(ns).indice_comp(0:nbcomp-1)

                                ;--- SELECTION eventuel de certaines
                                ;    sources...
    if n_elements(source_to_use) gt 0 then begin
        if (source_to_use(0) ne 'all' and source_to_use(0) ne '') then begin
            ii = where(source_to_use eq source(ns).name,cc)
            if cc eq 0 then begin
                print,'---> WARNING, Je n utilise pas la source : ',source(ns).name
                goto,suite_source_to_remove
            endif
        endif
    endif 
                                ;--- Retrait eventuel de certaines
                                ;    sources...
    foo = where(source_to_remove eq source(ns).name,cc)
    if cc gt 0 then begin
        print,'---> WARNING, Je n utilise pas la source : ',source(ns).name
        goto,suite_source_to_remove
    endif
                                
                                ;--- Retrait eventuel de certains
                                ;    processus...
    foo = where(proc_to_remove eq source(ns).proc_comp(0),cc)
    if cc gt 0 then begin
        print,'---> WARNING, Je n utilise pas le processus : ',source(ns).proc_comp(0)
        goto,suite_source_to_remove
    endif

                                 
;----------------  Source pixel.
    if source(ns).proc_comp(0) eq 'pixel' then begin
;        print,'source : ',source(ns).name
        
                               ;--- Retrait eventuel de certaines
                                ;    regions..
        ii = where(reg_to_remove eq source(ns).reg_comp(0),cc)
        if cc gt 0 then goto,suite_source_to_remove
        flag_use = 1

                                ;--- Definition du pt grille et du
                                ;    champs concerne si split_flux!
        i = source(ns).indice_comp(0)
        j = source(ns).indice_comp(1)
        l = 0
        if split_flux ge 1 then begin
            foo1 = where(regoce eq source(ns).reg_comp(0),cc1)
            foo2 = where(regland eq source(ns).reg_comp(0),cc2)
            if cc1+cc2 ne 1 then message,'Probleme affectation pixel...'
            if cc1 eq 1 then l = where(trac_tab eq 'oce',cc)  
            if cc2 eq 1 then l = where(trac_tab eq 'land',cc)  
            l = l(0)
        endif

        for im=0,nmon-1 do begin            
            mois_courant = im mod 12

                                ;--- Define the crenau to sum and
                                ;    the scaling factor
            ii = sort(abs(source(ns).date_crenau(0:nbcrenau-1)-date_out(im)))
            ii = ii(0)
            if flux_per_surf then coef = 1./grid.dxyp(i,j) else coef = 1.
            coef = coef * coef_gt2kg(mois_courant)

            poste_out(i,j,im,l,0) = poste_out(i,j,im,l,0) + source(ns).flux_poste(ii)*coef
            if add_prior then $
              prior_out(i,j,im,l,0) = prior_out(i,j,im,l,0) + source(ns).flux(ii)*coef
            if add_err then begin
                poste_out(i,j,im,l,1) = poste_out(i,j,im,l,1) + source(ns).err_flux_poste(ii)*coef
                if add_prior then $
                  prior_out(i,j,im,l,1) = prior_out(i,j,im,l,1) + source(ns).err_flux(ii)*coef
            endif
        endfor 

                                
;-------------- Source regionale..
    endif else begin

        print,'-----------------------------'
        print,'source : ',source(ns).name,nbcomp

                                ;--- Definition du champs concerne si
                                ;    split_flux.. 
        l = 0
        if split_flux ge 1 then begin
            l = where(trac_tab eq source(ns).proc_comp(0),cc)
            if cc ne 1 then message,'Hum... probleme avec source regionale...'
            l = l(0)
        endif
        
                                ;--- Boucle sur les sources de base...
        for nb=0,nbcomp-1 do begin
            iireg = iicomp_base(nb)
            
                                ;--- Retrait eventuel de certaines
                                ;    regions..
            foo = where(reg_to_remove eq source_base(iireg).reg,cc)
            if cc gt 0 then begin
                print,'---> WARNING, Je n utilise pas la region : ',source_base(iireg).name
                goto,suite_comp_to_remove
            endif

                                ;-- Identification sources annuelles
                                ;   pour traitement special!!
            special_presub_ann = 0
            if nbcrenau ne nmon then begin
                special_presub_ann = 1
                if nbcrenau*12 ne nmon then begin
                    print,'Configuration non traitee...',nbcrenau,nmon
                    stop
                endif
                if nbcomp gt 1 then message,'Presub non traite si plusieurs composantes !!'
            endif
            flag_use = 1

                                ;--- Lecture du/des fichiers de base
                                ;    et remplissage du tableau DATA 
            CASE flag_reg of 
                                ;--- Cas STD fournit par utilisateur
                0: begin
                    foo = where(file_base(1,*) eq source(ns).proc_comp(0),cc)
                    if cc eq 0 then begin
                        print,'proc non decrit dans file_base ',source(ns).proc_comp(0)
                        stop
                    endif
                    file      = file_base(0,foo(0))
                end
                                ;--- Cas LMDZ claire (nouvelles
                                ;    regions...)
                1: begin
                    
                                ;--- Definition du nom du fichier...            
                    if source(ns).proc_comp(0) eq 'oce' then begin
                        file = 'fco2.taka.oce.glo.fy95.m.lmdz96.nc'
                    endif else if source(ns).proc_comp(0) eq 'fos' then begin
                        file = 'fco2.aerocarb.fos.glo.fy98.m.lmdz96.nc'
                    endif else if source(ns).proc_comp(0) eq 'bbur' then begin
                        file = 'fco2.guido.bbur.glo.fy79-03.m.lmdz96.nc'
                    endif else if source(ns).proc_comp(0) eq 'nep' then begin
                        file = 'fco2.turc_brut.resp.glo.fy98.j.lmdz96.nc'
                    endif else message,'Processus non reconnu '+source(ns).proc_comp(0)
                end
                                ;--- Cas LMDZ claire (T3
                                ;    regions = leo...)
                2: begin
                    
                                ;--- Definition du nom du fichier en
                                ;    tenant compte des pre-soustrait..            
                    if source(ns).proc_comp(0) eq 'oce' then begin
                        file = 'fco2.flat.oce.glo.fy95.m.lmdz96.nc'
                        if source(ns).name eq 'oce.glo' then $
                          file = 'fco2.taka.oce.glo.fy95.m.lmdz96.nc'
                    endif else if source(ns).proc_comp(0) eq 'fos' then begin
                        ;;-- A ameliorer en prenant en compte fos 95 et interpolation...
                        file = 'fco2.andres.fos.glo.fy90.m.lmdz96.nc'
                    endif else if source(ns).proc_comp(0) eq 'nep' then  begin
                        file = 'fco2.casa.npp.glo.fy90.m.lmdz96.nc'
                        if source(ns).name eq 'nep.glo' then $
                          file = 'fco2.casa.nep.glo.m.lmdz96.nc'
                    endif else message,'Processus non reconnu '+source(ns).proc_comp(0)
                end
                
                else : message,'cas pour flag_reg non prevu...'
            ENDCASE 
            
                                ;--- Recherche du bon fichier et
                                ;    lecture des nom de variables
            for p=0,n_elements(path_base)-1 do begin
                if findfile(path_base(p)+file) ne '' then break
            endfor
            if p ge n_elements(path_base) then begin
                print,'file not found in path_base ',file
                stop
            endif 
            if nb eq 0 then print,file
            peyl_readnc,file=path_base(p)+file,tempo_name,/only_var_names


                                ;--- Lecture du flux EN KgC/m2/h
            for v=0,n_elements(var_name_flux)-1 do begin
                foo = where(tempo_name eq var_name_flux(v),cc)
                if cc eq 1 then break
            endfor
            if v ge n_elements(var_name_flux) then message,'Var_name_flux non valide'
            peyl_readnc,file=path_base(p)+file,tempo_var,var_name=var_name_flux(v)
            ss = size(tempo_var)

                                ;--- Lecture de la latitude
            for v=0,n_elements(var_name_lat)-1 do begin
                foo = where(tempo_name eq var_name_lat(v),cc)
                if cc eq 1 then break
            endfor
            if v ge n_elements(var_name_lat) then message,'Var_name_lat non valide'
            peyl_readnc,file=path_base(p)+file,tempo_lat,var_name=var_name_lat(v)

                                ;--- Reformat flux S-to-N, et retrait
                                ;    longitude additionnelle
            tempo_var = reformat_flux(tempo_var,tempo_lat,nlon)
            
                                ;--- Lecture de base temporelle pour
                                ;    moyenner et choisir bonne annee...
            for v=0,n_elements(var_name_time)-1 do begin
                foo = where(tempo_name eq var_name_time(v),cc)
                if cc eq 1 then break
            endfor
            if v ge n_elements(var_name_time) then begin
                print,'Var_name_time non definie '
                print,'   --> on prend findgen(ntime) '
                tempo_time = (findgen(ss(3))+0.5)/ss(3)
            endif else $
              peyl_readnc,file=path_base(p)+file,tempo_time,var_name=var_name_time(v)

            nt_file = n_elements(tempo_time)
            if nt_file gt 1 then begin
                dt_file = tempo_time(1) - tempo_time(0) 
                                ;-- cas special des "indgen(12)"
                if nt_file eq 12 then begin
                    if abs(dt_file-(1./12.)) gt 0.001 then begin
                        print,'WARNING, re-initialisation de la base de temps sur 12 mois!'
                        dt_file = 1./12.
                        tempo_time = (findgen(12)+0.5)/12.
                    endif 
                endif else begin
                    tt1 = 1./nt_file
                    tt2 = 1./12.
                    if (abs(dt_file-tt1) gt (0.1*tt1) and $
                        abs(dt_file-tt2) gt (0.1*tt2) ) then $
                      message,'HUMMM base de temps Non reconnue OU foireuse!!!'
                endelse
            endif else dt_file = 1.
            
                                ;--- Definition d'une base de temps
                                ;    sans annee et ce pour la premiere
                                ;    annee seulement..
            tempo_time_bis = tempo_time - fix(tempo_time(0))
            foo = where(tempo_time_bis lt 1., cc)
            tempo_time_bis = tempo_time_bis(foo)
            
                                ;--- Restriction du flux a la region
                                ;    consideree..
            ind = peyl_choixregnew_back(source_base(iireg).reg, grid_name=grid_name,$
                                        grid_pixel = grid, /noinfo)
            data = fltarr(nlon,nlat,nt_file)
            for n=0,nt_file-1 do begin
                tt = tempo_var(*,*,n)
                tt(ind.ii2d_inv) = 0.
                data(*,*,n) = tt
            endfor 

                                ;--- Creation d'un mask terre/oce            
            if (add_sealand_mask and ind.type eq 'land') then mask(ind.ii2d) = 1 
                
                                ;--- Creation du tableau restreint au
                                ;    flux de toutes les composantes!
            if nb eq 0 then begin
                regall = peyl_concatstr(source_base(iicomp_base).reg,separator='+')
                indall = peyl_choixregnew_back(regall, grid_name=grid_name,$
                                               grid_pixel=grid,/noinfo)
                data_all = fltarr(nlon,nlat,nt_file) 
                for n=0,nt_file-1 do begin
                    tt = tempo_var(*,*,n)
                    tt(indall.ii2d_inv) = 0.
                    data_all(*,*,n) = tt
                endfor 
            endif
            

;--------------- Boucle sur les mois de l'inversion
            for im=0,nmon-1 do begin
                mois_courant = im mod 12
                
                                ;--- Gestion du cas ou plusieurs pas
                                ;    de temps dans fichier par mois :
                                ;    nim_file et moyenne des pas de
                                ;    temps.
                dd = 0.
                d1 = date_out(im) - dt_out/2. -dd 
                d2 = date_out(im) + dt_out/2. +dd
                foo = where(tempo_time ge d1 and tempo_time lt d2, cc)
                if cc eq 0 then begin
                    d1 = date_out_bis(im) - dt_out/2. -dd
                    d2 = date_out_bis(im) + dt_out/2. +dd
                    foo = where(tempo_time_bis ge d1 and tempo_time_bis lt d2, cc)
                endif
                if cc eq 0 then message,'Probleme pas nb pas temps !!!'
                nim_file = cc
                im_file  = foo
                if (nim_file ne 1 and nim_file ne jour_mois(mois_courant) and $
                    nim_file ne (jour_mois(mois_courant)*2) ) then begin
                    if nim_file lt 28 or nim_file gt 32 then begin
                        print,'Bizarre: nt to average for file not recognized ',nim_file,im_file
                        if peyl_pause() then stop
                    endif
                endif

                                ;-- On definit le fichier moyen de la
                                ;   region en regroupant toutes les
                                ;   regions, a utiliser.. EN KgC/m2/h 
                if nim_file gt 1 then begin
                    contrib     = total(data(*,*,im_file),3)/nim_file 
                    contrib_all = total(data_all(*,*,im_file),3)/nim_file 
                endif else begin
                    contrib     = data(*,*,im_file)
                    contrib_all = data_all(*,*,im_file)
                endelse

                                ;-- Flux totaux en GtC/mois
                                ;   (coef_kg2gt pour passer de kg/h a
                                ;   Gt/mois)...
                flux_reg    = total(contrib*grid.dxyp) * coef_kg2gt(mois_courant)
                flux_allreg = total(contrib_all*grid.dxyp) * coef_kg2gt(mois_courant)

                
                                ;-- GESTION DES SOURCES ANNUELLES
                                ;   dans cas crenaux mensuel.. On
                                ;   definit le fichier moyen annuel 
                im1 = im
                if special_presub_ann then begin
                    im1 = fix( im / 12 )
                    if im1 ge nbcrenau then message,'BBBIGGG Probleme...'
                endif 
                
 
                                ;--- Test de la valeur totale du
                                ;    fichier lu pour normaliser...
                if (abs(flux_reg) ge precis_flux) then begin
                    coef_rescale = 1./flux_reg(0) 
                    
                                ;-- On a une distribution trop proche
                                ;   de zero !! on prend une distrib
                                ;   plate.. comme dans Fbase!
                endif else begin
                    print,'!!! Attention distribution platte : ',flux_reg
                    
                                ;-- Dans cas Presub_ann on ne
                                ;   renormalise pas si le presub a ete
                                ;   calculer par RUN direct et non par
                                ;   FBASE! 
                    if special_presub_ann eq 0 then begin
                        contrib(ind.lon,ind.lat) = 1./(total(grid.dxyp(ind.lon,ind.lat)) * $
                                                       coef_kg2gt(mois_courant) )
                    endif else begin
                        print,'WARNING, Presub suppose calculer selon RUN direct...'
                    endelse
                    coef_rescale = 1.
                endelse
                
                                ;-- Diagnostic pour 2 prere annee
                if im lt 24 then begin
                    print,im,im1,im_file,flux_reg,flux_allreg,$
                      source_base(iireg).flux(im1),source(ns).flux(im1),$
                      source(ns).flux_poste(im1)
                endif
                
                                ;-- Definition des coef de scaling
                                ;   propre a l'inversion... 
                if (abs(source(ns).flux(im1)) lt precis_flux) then begin
                    print,'Rem: Flux total NULLE...',im1,source(ns).flux(im1)
                    coef_poste = source(ns).flux_poste(im1) / nbcomp
                    coef_prior = source(ns).flux(im1) / nbcomp
                endif else begin 	
                    if (abs(source_base(iireg).flux(im1)) lt precis_flux) then begin
                        print,'FLUX region base tout petit ...'
                    endif
                    coef = source_base(iireg).flux(im1)
                    coef_prior = coef
                    coef_poste = coef * (source(ns).flux_poste(im1)/source(ns).flux(im1))
                endelse

                                ;-- Cas special des presub! On veut
                                ;   conserver le flux mensuel de la
                                ;   distribution lue et ne pas le
                                ;   rescaler...
                if special_presub_ann then begin
                    coef_prior = 1./coef_rescale
                    coef_poste = 1./coef_rescale * (source(ns).flux_poste(im1)/source(ns).flux(im1)) 
                endif 
                    
                                ;--- Scaling du flux par bonne valeur
                                ;    et addition aux tableaux de sortie..
                poste_out(*,*,im,l,0) = poste_out(*,*,im,l,0) + contrib * coef_rescale * coef_poste 
                if add_prior then $
                  prior_out(*,*,im,l,0) = prior_out(*,*,im,l,0) + contrib * coef_rescale * coef_prior
                
            endfor  
            
            suite_comp_to_remove:        
        endfor 

    endelse 
    
    suite_source_to_remove:        
endfor 


;------ Petit check pour debugger...
if 0 then begin
    temp = fltarr(nmon)
    temp2 = fltarr(nmon)
    iis = where(source.proc_comp(0) eq 'nep')
    for im=0,nmon-1 do begin 
        temp(im) = total(data_out(*,*,im)*grid.dxyp(*,*))*coef_kg2gt(im mod 12)
        temp2(im) = total(source(iis).flux_poste(im))
    endfor    
    nyr = nmon/12
    temp_an = fltarr(nyr)
    temp_an2 = fltarr(nyr)
    for n=0,nyr-1 do begin
        temp_an(n)  = total(temp(indgen(12)+n*12))
        temp_an2(n) = total(temp2(indgen(12)+n*12))
    endfor
    help,data_out
    print,temp_an
    print,total(temp_an-temp_an2)
    stop
endif


;------- Definition de la sortie

if flag_use eq 0 then begin
    PRINT,'!!!! AUCUNE SOURCES UTILISEE !!!!'
    return,0
endif


cmde = "out = {trac:trac_tab,poste:poste_out,date:date_out"
if add_prior then cmde=cmde+",prior:prior_out"
if add_sealand_mask then cmde=cmde+",mask:mask"
if add_station then cmde=cmde+",station:stat,conc_obs:conc_obs,err_obs:err_obs,nt_obs:ntime_obs_valid,date_obs:date_obs,conc_mod_poste:CONCTOT_POSTE,conc_mod_prior:CONCTOT_PRIOR"
cmde=cmde+"}"
res = execute(cmde)


if add_journal then journal
return,out
;STOP
END
