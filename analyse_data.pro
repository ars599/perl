;;*******************************************************************************
;;PROGRAMME	: ANALYSE_DATA
;;AUTEUR	: C. BACOUR
;;CREATION	: 
;;COMPILATEUR	: IDL
;;
;;OBJECTIF	: Recuperation des fichiers de I/O ORCHIDEE pour visualisation
;;
;; 
;;*******************************************************************************




;;*******************************************************************************
;; Visualisation des series temporelles des variables d'un fichier
;;*******************************************************************************
PRO VISU_TS, FILES=files, ERASE_WIND = erase_wind, MODELE = modele, SITE = site

FORWARD_FUNCTION AFFICHE_TS

;; ----------
;; CONSTANTES
;; ----------
PFTs = [$
         'Soil',$
         'TrBE',$
         'TrBR',$
         'TeNE',$
         'TeBE',$
         'TeBS',$
         'BoNE',$
         'BoBS',$
         'BoNS',$
         'NC3',$
         'NC4',$
         'AC3',$
         'AC4']


IF N_ELEMENTS(erase_wind) EQ 0 THEN erase_wind = 1 ;; ecrase fenetre d'affichage
test_site = 0
IF N_ELEMENTS(site)       NE 0 THEN test_site = 1

;; -- Modeles/variables a visualiser 

variables_except = ['lat', 'lon', 'level', 'indice_col','indice_ligne' ,'mask']

IF N_ELEMENTS(modele) EQ 0 THEN modele  = ['sechiba'] 

; variables de SECHIBA
IF STRCMP(modele, 'sechiba') EQ 1 THEN BEGIN
    variables_visu = ['LAI','PotEvap','TVeg','GPP','NEE'] ;; 'Snowf','Rainf',
;,'SWnet','LWnet','Qh','Qle','Evap','DelSoilMoist','AvgSurfT','Albedo', $
;'LAI','PotEvap','TVeg','ESoil','GPP','agcan','NEE']
ENDIF

;; variables de STOMATE
IF STRCMP(modele, 'stomate') EQ 1 THEN BEGIN
    variables_visu =  ['HET_RESP','TOTAL_SOM_C','LITTER_STR_AB_C','DEADLEAF_COVER','HET_RESP','CO2FLUX','CO2FLUX_MONTHLY', $
                       'LAI','NPP','GPP','TOTAL_BM_C','MAINT_RESP','GROWTH_RESP','BM_ALLOC_LEAF_C','','']
ENDIF


;; -- Fichiers a traiter

IF NOT KEYWORD_SET(files) THEN BEGIN

    ;; -- path
    ;;pathIN = '/home/data02/cbacour/ORCHIDEE/outputs/'
    pathIN = '/home/users/cbacour/ORCHIDEE/source_cedric/RUN/Results/'
    
    ;; -- fichiers 
    IF test_site EQ 0 THEN BEGIN
        files = FILE_SEARCH(pathIN+'*'+modele+'_history*.nc')
    ENDIF ELSE BEGIN
        files = FILE_SEARCH(pathIN+site+'*'+modele+'_history*.nc')        
    ENDELSE
    
ENDIF

PRINT, 'FICHIERS TROUVES',files

;; --------------------
;; LECTURE ET AFFICHAGE
;; --------------------

FOR ifile = 0, N_ELEMENTS(files)-1 DO BEGIN

    file = files[ifile]

    print, '## Traitement du fichier '+file
    
    ;; -- Variables a traiter du fichier --
    ;; ------------------------------------
    PEYL_READNC, variable_names, FILE = file, /ONLY_VAR_NAMES
    nvars = N_ELEMENTS(variable_names)
    
    last = nvars-1
    ivar = 0
    WHILE ivar LT last DO BEGIN
        idx = WHERE(variable_names[ivar] EQ variables_except)
        IF idx[0] NE -1 THEN BEGIN
            CASE ivar OF
                0    : variable_names = variable_names[1:*]
                last : variable_names = variable_names[0:last-1]
                ELSE : variable_names = [ variable_names[0:ivar-1], variable_names[ivar+1:last] ]
            ENDCASE
            ivar = 0
            last = N_ELEMENTS(variable_names)-1
        ENDIF
        ivar = ivar + 1
    ENDWHILE
    nvars = N_ELEMENTS(variable_names)




    ;; -- Chargement des variables --
    ;; ------------------------------

    strVAR = {$         
               name             : PTRARR(nvars,/ALLOCATE_HEAP), $
               datatype         : PTRARR(nvars,/ALLOCATE_HEAP), $
               ndims            : PTRARR(nvars,/ALLOCATE_HEAP), $
               dim_id           : PTRARR(nvars,/ALLOCATE_HEAP), $
               value            : PTRARR(nvars,/ALLOCATE_HEAP), $
               attr_name        : PTRARR(nvars,/ALLOCATE_HEAP), $
               attr_type        : PTRARR(nvars,/ALLOCATE_HEAP), $
               attr_value       : PTRARR(nvars,/ALLOCATE_HEAP), $
               title            : PTRARR(nvars,/ALLOCATE_HEAP), $
               units            : PTRARR(nvars,/ALLOCATE_HEAP) $            
             }


    FOR ivar = 0, N_ELEMENTS(variable_names)-1 DO BEGIN    
        PEYL_READNC, var_value, FILE = file,  var_name = variable_names[ivar], $
          var_info = var_info, attrib = var_attrib, gattrib = gattr
        
        *strVAR.name[ivar]       = variable_names[ivar]
        *strVAR.datatype[ivar]   = var_info.datatype
        *strVAR.ndims[ivar]      = var_info.ndims
        *strVAR.dim_id[ivar]     = var_info.dim
        *strVAR.value[ivar]      = var_value
        *strVAR.attr_name[ivar]  = var_attrib.name
        *strVAR.attr_type[ivar]  = var_attrib.type
        *strVAR.attr_value[ivar] = var_attrib.value         
    ENDFOR


    ;; -- Visualisation --
    ;; -------------------

    vars_visu = REFORM(variables_visu[*])
    iwind = 0

    FOR ivar = 0,N_ELEMENTS(vars_visu)-1 DO BEGIN
        
        idxPOS = WHERE(STRCMP(vars_visu[ivar], variable_names) EQ 1)
        
        IF idxPOS[0] NE -1 THEN BEGIN
            
            IF erase_wind EQ 0 THEN BEGIN
                WINDOW, iwind, XSIZE = 1100, YSIZE = 300
            ENDIF ELSE BEGIN
                WINDOW, 0, XSIZE = 1100, YSIZE = 300
            ENDELSE
            
            ;; Visualisation
            ;; -------------
            AFFICHE_TS, strVAR, gattr, idxPOS
            iwind = iwind + 1

            PRINT, *strVAR.name[idxPOS[0]]
            PRINT, ' -- MIN, MAX, MEDIAN : ', STRC(MIN((*strVAR.value[idxPOS[0]])[*])) + ' ; '+ $
              STRC(MAX((*strVAR.value[idxPOS[0]])[*])) + ' ; '+ $
              STRC(MEDIAN((*strVAR.value[idxPOS[0]])[*]))
            
        ENDIF
        PAUSE
    ENDFOR



ENDFOR ;; fin fichiers

END
;; ------------------------------------------------------------------------------










;;*******************************************************************************
;; COMPARAISON des series temporelles des variables de
;; differents fichiers
;;*******************************************************************************
PRO COMP_TS_FILES, FILE=files, MODELE = modele

FORWARD_FUNCTION AFFICHE_TS



;; ----------
;; CONSTANTES
;; ----------
PFTs = [$
         'Soil',$
         'TrBE',$
         'TrBR',$
         'TeNE',$
         'TeBE',$
         'TeBS',$
         'BoNE',$
         'BoBS',$
         'BoNS',$
         'NC3',$
         'NC4',$
         'AC3',$
         'AC4']



;; -- Modeles/variables a visualiser 

variables_except = ['lat', 'lon', 'level', 'indice_col','indice_ligne' ,'mask']

IF N_ELEMENTS(modele) EQ 0 THEN modele  = ['sechiba'] 

; variables de SECHIBA
IF STRCMP(modele, 'sechiba') EQ 1 THEN BEGIN
    variables_visu = ['Snowf','Rainf','LAI','PotEvap','TVeg','GPP','NEE']
;,'SWnet','LWnet','Qh','Qle','Evap','DelSoilMoist','AvgSurfT','Albedo', $
;'LAI','PotEvap','TVeg','ESoil','GPP','agcan','NEE']
ENDIF

;; variables de STOMATE
IF STRCMP(modele, 'stomate') EQ 1 THEN BEGIN
    variables_visu =  ['TOTAL_SOM_C','LITTER_STR_AB_C','DEADLEAF_COVER','HET_RESP','CO2FLUX','CO2FLUX_MONTHLY', $
                       'LAI','NPP','GPP','TOTAL_BM_C','MAINT_RESP','GROWTH_RESP','BM_ALLOC_LEAF_C','','']
ENDIF


;; -- Fichiers a traiter

IF NOT KEYWORD_SET(files) THEN BEGIN

    ;; -- path
    ;;pathIN = '/home/users/cbacour/ORCHIDEE/source_cedric/test_version/outputs/'
    ;;pathIN = '/home/users/cbacour/ORCHIDEE/source_cedric/RUN/Results/'
    pathIN = '/home/users/cbacour/ORCHIDEE/source_cedric/RUN/Results/'

    
    ;; -- fichiers 
    ;;files = '/home/data02/cbacour/ORCHIDEE/forcage/BX23_alma.nc'
    ;;files = [pathIN + 'dev_' + modeles[0] + '_history_1.nc',$
    ;;         pathIN + 'dev_' + modeles[1] + '_history_1.nc']

    files = ['BX_ECMWF_2004_sechiba_history.nc','BX_ECMWF_2004_sechiba_history_snowf0.nc']
    ;;files = ['BX_ECMWF_stomate_history.nc', 'BX_ECMWF_stomate_history_snowf0.nc']
    
ENDIF




;; --------------------
;; LECTURE ET AFFICHAGE
;; --------------------

FOR ivar = 0,N_ELEMENTS(variables_visu)-1 DO BEGIN
    
    
    FOR ifile = 0, N_ELEMENTS(files)-1 DO BEGIN

        print, '## Traitement du fichier '+files[ifile]
        
        ;; -- Variables a traiter du fichier --
        ;; ------------------------------------
        PEYL_READNC, variable_names, FILE = pathIN+files[ifile], /ONLY_VAR_NAMES
        nvars = N_ELEMENTS(variable_names)
        
        last = nvars-1
        ivars = 0
        WHILE ivars LT last DO BEGIN
            idx = WHERE(variable_names[ivars] EQ variables_except)
            IF idx[0] NE -1 THEN BEGIN
                CASE ivars OF
                    0    : variable_names = variable_names[1:*]
                    last : variable_names = variable_names[0:last-1]
                    ELSE : variable_names = [ variable_names[0:ivars-1], variable_names[ivars+1:last] ]
                ENDCASE
                ivars = 0
                last = N_ELEMENTS(variable_names)-1
            ENDIF
            ivars = ivars + 1
        ENDWHILE
        nvars = 1 ;;N_ELEMENTS(variable_names)


        ;; -- Chargement des variables --
        ;; ------------------------------

        idxPOS = WHERE(STRCMP(variables_visu[ivar], variable_names) EQ 1)

        IF idxPOS[0] NE -1 THEN BEGIN            

            strVAR = {$         
                       name             : PTRARR(nvars,/ALLOCATE_HEAP), $
                       datatype         : PTRARR(nvars,/ALLOCATE_HEAP), $
                       ndims            : PTRARR(nvars,/ALLOCATE_HEAP), $
                       dim_id           : PTRARR(nvars,/ALLOCATE_HEAP), $
                       value            : PTRARR(nvars,/ALLOCATE_HEAP), $
                       attr_name        : PTRARR(nvars,/ALLOCATE_HEAP), $
                       attr_type        : PTRARR(nvars,/ALLOCATE_HEAP), $
                       attr_value       : PTRARR(nvars,/ALLOCATE_HEAP), $
                       title            : PTRARR(nvars,/ALLOCATE_HEAP), $
                       units            : PTRARR(nvars,/ALLOCATE_HEAP) $            
                     }

            
            PEYL_READNC, var_value, FILE = pathIN+files[ifile],  var_name = variable_names[idxPOS[0]], $
              var_info = var_info, attrib = var_attrib, gattrib = gattr
            
            *strVAR.name[0]       = variable_names[idxPOS[0]]
            *strVAR.datatype[0]   = var_info.datatype
            *strVAR.ndims[0]      = var_info.ndims
            *strVAR.dim_id[0]     = var_info.dim
            *strVAR.value[0]      = var_value
            *strVAR.attr_name[0]  = var_attrib.name
            *strVAR.attr_type[0]  = var_attrib.type
            *strVAR.attr_value[0] = var_attrib.value         



        ;; -- Visualisation --
        ;; -------------------
        

            WINDOW, ifile, XSIZE = 1100, YSIZE = 300, XPOS = 100, YPOS = ifile * 300+100 
            AFFICHE_TS, strVAR, gattr, [0], TITLE = files[ifile]   
        
        ENDIF
                


    ENDFOR ;; fin fichiers

    PAUSE
            

ENDFOR ;; fin boucle variables




END
;; ------------------------------------------------------------------------------








;;*******************************************************************************
PRO AFFICHE_TS, strVAR, attr, idxVAR, TITLE = title, POSITION = position


IF N_ELEMENTS(title) EQ 0 THEN title = ''
IF N_ELEMENTS(position) EQ 0 THEN position = SUBPLOT(1,1,1)

;; Determination du pas de temps
;; -----------------------------

;; Variables
vars_name = ['']
FOR i=0,N_ELEMENTS(strVAR.name)-1 DO vars_name = [vars_name, *strVAR.name[i]]
vars_name = vars_name[1:N_ELEMENTS(vars_name)-1]


;; Tests des cas
IF ( (WHERE(STRCMP(vars_name,'timestp') EQ 1))[0] NE -1 ) THEN BEGIN               ;; variable timestp

    str_year0 = *strVAR.attr_value[(WHERE(STRCMP(vars_name,'timestp') EQ 1))[0]]
    year0 = UINT(STRMID((str_year0[4])[0],0,5))
    tstep = UINT(str_year0[3])
    
    tstep_jour = (3600.*24.)/ tstep

    cas_time = 0
ENDIF ELSE BEGIN

    IF (WHERE(STRCMP(attr.name, 'year0') EQ 1))[0] NE -1  THEN BEGIN               ;; on regarde attributs globaux
        
        year0  = attr[WHERE(STRCMP(attr.name, 'year0') EQ 1)].value
        month0 = attr[WHERE(STRCMP(attr.name, 'month0') EQ 1)].value
        day0   = attr[WHERE(STRCMP(attr.name, 'day0') EQ 1)].value
        
        
        idxTSTEP = WHERE(STRCMP(attr.name,'tstep_sec') EQ 1 OR $
                         STRCMP(attr.name,'delta_tstep_sec') EQ 1)
        tstep_jour = ((3600.*24.)/(attr.value)[idxTSTEP])[0]
        
        cas_time = 0
    ENDIF ELSE BEGIN                           ;; pas de pseudo-date
        cas_time = 1
    ENDELSE
ENDELSE


;; Affichage
;; ---------
FOR i = 0, N_ELEMENTS(idxVAR)-1 DO BEGIN
    
    idx = idxVAR[i]
    

    IF N_ELEMENTS(*strVAR.value[idx]) GT 1 THEN BEGIN
    
        taille = SIZE(REFORM(*strVAR.value[idx]), /DIMENSION)
        taille = taille[N_ELEMENTS(taille)-1]
        
    
        idxUNITS = WHERE(STRCMP(*strVAR.attr_name[idx], 'units') EQ 1)
        ylabel = *strVAR.name[idx]+'('+(*strVAR.attr_value[idx])[idxUNITS]+')'
        
        CASE cas_time OF
            ;;0: date_time = FINDGEN(N_ELEMENTS(*strVAR.value[idx]))/(365.*tstep_jour)+FLOAT(year0)
            ;;1: date_time = TIMEGEN(N_ELEMENTS(*strVAR.value[idx]), UNITS = 'Days')
            0: date_time = FINDGEN(taille)/(365.*tstep_jour)+FLOAT(year0)
            1: date_time = FINDGEN(taille)
        ENDCASE
        
        MPLOT, date_time,REFORM(*strVAR.value[idx]), $
          YLABEL = ylabel, XLABEL = 'time', TITLE = title, POSITION = position
        
 
    ENDIF    
ENDFOR ;; fin boucle variables

END ;; fin AFFICHE_TS













