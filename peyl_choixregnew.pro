;+
;==============================================================
;  ////////////////////////////////////////////////////////////
;
;		Find indices for specific biome(s).
;
;  ////////////////////////////////////////////////////////////
;==============================================================

FUNCTION peyl_choixregnew, selection, grid_name = grid_name, $
                           box_coord = box_coord,noinfo = noinfo, $
                           extend_reg = extend_reg,$
                           grid_pixel = grid_pixel, indice_reg = indice_reg,$
                           reg = reg, help = help, check_map = check_map, $
                           fraction = fraction, limits = limits

!P.FONT=1
;-- Defines default map name and tag
map_names_all = ['regions_country','regions_std','regions_aerocarb','regions_carboeurope','regions_countrybis','regions_iasa-sib','regions_nov']
map_ind_all   = ['c','std','ae','ce','c2','iasa','nov']
n_map_all     = n_elements(map_ind_all)
path_main     =  '/bm/gdata2/fdelage/REGIONALISATION/'

;---- Define regions special groupement
goto, deb_define_groupement
fin_define_groupement:

;------------------------
; HELP
;------------------------
IF keyword_set(help) THEN goto, special_help


;------------------------
; Main Parameters
;------------------------

IF n_elements(check_map) eq 0 THEN check_map_flag = 0 else check_map_flag = 1
IF keyword_set(noinfo) THEN info = 0 else info = 1
IF not keyword_set(extend_reg) THEN extend_reg_loc=0 else extend_reg_loc=extend_reg
IF not keyword_set(grid_pixel) THEN grid_read=1 else grid_read=0
IF not keyword_set(indice_reg) THEN indice_read=1 else indice_read=0
IF not keyword_set(reg) THEN name_read=1 else name_read=0
IF not keyword_set(fraction) THEN fraction = 0

IF grid_read THEN BEGIN
    IF not keyword_set(grid_name) THEN message,'Il faut fournir le nom de la grille!'
    grid_pixel = peyl_choixgrid(strlowcase(grid_name),/noinfo)
ENDIF else begin
    IF keyword_set(grid_name) THEN BEGIN
        IF strlowcase(grid_name) ne strlowcase(grid_pixel.name) THEN BEGIN
            print,'WARNING :Conflit grid_name et grid_pixel, relecture selon grid_name..'
            grid_pixel = peyl_choixgrid(strlowcase(grid_name),/noinfo)
        ENDIF 
    ENDIF
ENDELSE
nlon=long(grid_pixel.nlon)
nlat=long(grid_pixel.nlat)


;----------------------------------------
; Defines selected biomes, regions, ...
;----------------------------------------

IF (n_elements(selection) gt 1) THEN BEGIN
    print,'!! on ne prend que la premiere region (summer les regions avec des +) !!'
    IF peyl_pause() THEN stop
ENDIF


;------ Cas des regions pre-definies...
comp = strlowcase(str_sep(selection(0),'+'))
nb_select = n_elements(comp)
components = ''
for n=0,nb_select-1 do begin
    foo = where(special_group(0,*) eq comp(n),cc)
    if cc ge 2 then message,'Hum problem dans def des groupements...'
    if cc eq 1 then begin
        print,'Special groupment : ',special_group(1,foo(0))
        add = strlowcase(str_sep(special_group(2,foo(0)),'+'))
    endif else add = comp(n)
    components = [components,add]
endfor
components = components(1:*)
nb_select = n_elements(components)


;------ Check redondance
foo = uniq(components, sort(components))
if n_elements(foo) lt nb_select then begin
    print,'Redondance dans les regions ',components
    if peyl_pause() then stop
endif

;------ Cas specifique Box
special_box = 0
str = strsplit(components(0),'_',/extract)
IF strlowcase(str(0)) eq 'box' THEN BEGIN
    special_box = 1
    if nb_select gt 1 then message,'Une seule composante possible avec BOX!!'
    if n_elements(str) eq 5 then begin
        box_coord = float(str(1:*))
    endif else begin
        IF not keyword_set(box_coord) THEN message,'Il faut fournir box_coord !!'
        IF n_elements(box_coord) ne 4 THEN message,'Box_coord doit contenir 4 elements!!'
    endelse
    if info eq 1 then print,'Selection boite rectangulaire [lon1,lat1,lon2,lat2] : ',box_coord
    nb_select = 1
    extend_reg_loc = 0
    if fraction then print,'Fraction impossible avec choix BOX: fraction reset to 0!!'
    fraction = 0
    goto, debut_selection
ENDIF

if fraction then extend_reg_loc = 0

;-- Search map tag in selection
indice = strarr(nb_select)
FOR i = 0, nb_select-1 DO BEGIN
    ind_tmp = str_sep(components(i),'_')
    indice(i) = ind_tmp(0)
    foo = where(map_ind_all eq indice(i),cc)
    if (n_elements(ind_tmp) lt 2 or cc ne 1) then begin
        print,'Nouveau nom avec prefix obligatoire : std_ ou c_ ou ae_ ou ce_ !! ',components(i)
        stop
    endif
ENDFOR
       
 
;-- Re-Defines map name and indices to read
ptr = -1
FOR i = 0,n_map_all-1 DO BEGIN
    ii = where(indice EQ map_ind_all(i),nn) 
    IF nn NE 0 THEN ptr = [ptr,i]
ENDFOR
ptr = ptr(1:*)
map_names = reform(map_names_all(ptr))
map_ind   = reform(map_ind_all(ptr))
n_map     = n_elements(map_names)


;----------------------------------------------------------------------
;  Lecture des fichiers de correspondance : NOM_REGION <---> INDICE
;----------------------------------------------------------------------

IF name_read THEN BEGIN

    ;-- Open region information files
    unit = intarr(n_map)
    nreg = intarr(n_map)

    FOR i = 0, n_map-1 DO BEGIN
        
        path = path_main+map_names(i)+'/'
        file = map_names(i)+'_code.txt'

        openr,u,path+file,/get_lun
        unit(i) = u
        lect = ''
        readf,u,lect
        nreg(i) = float(lect)
    ENDFOR
    nreg_max = max(nreg)

    ;-- Define structure for region information
    reg = {$
            map_ind:map_ind,$
            nreg:nreg,$
            title_reg:strarr(nreg_max,n_map),$   ;-- Full title
            name_reg:strarr(nreg_max,n_map),$    ;-- code name
            indice_reg:intarr(nreg_max,n_map),$  ;-- indice
            type_reg:intarr(nreg_max,n_map)$     ;-- Land:1 Oce:0
          }

    FOR i = 0, n_map-1 DO BEGIN
        ;-- Read regions information
        FOR k=0,nreg(i)-1 DO BEGIN
            readf,unit(i),lect
            str = str_sep(strcompress(strtrim(lect,2)), ' ')
            reg.name_reg(k,i) = strlowcase(str(0))
            reg.indice_reg(k,i) = fix(str(1))
            reg.type_reg(k,i) = fix(str(2))
            reg.title_reg(k,i) = peyl_concatstr(str(3:n_elements(str)-1),separator=' ')
        ENDFOR
        ;-- close ascii file
        free_lun,unit(i)
    ENDFOR

ENDIF ELSE BEGIN
    nn = n_elements(reg.map_ind) 
    if nn ne n_map then message,'Probleme structure reg fournie non compatible !'
    nreg_max = n_elements(reg.indice_reg(*,0))
ENDELSE


;------------------------
;  Cas specifiques....
;------------------------

ind_tmp = str_sep(components(0),'_')

;---- Cas GLO (all regions)...
IF ind_tmp(1) eq 'glo' THEN BEGIN
    if nb_select gt 1 then message,'Une seule composante possible avec GLO !!'
    ii = where(ind_tmp(0) EQ reg.map_ind,cc)
    if cc eq 0 then message,'big probleme avec GLO... '
    nb_select  = reg.nreg(ii(0))
    components = reg.name_reg(0:nb_select-1,ii(0))
    indice     = replicate(ind_tmp(0),nb_select)
    extend_reg_loc = 0
ENDIF

;---- Cas "c_land" or c2_land (all contries) or "std_oce" or "std_land"
IF (strlowcase(components(0)) eq 'c_land' or $ 
    strlowcase(components(0)) eq 'c2_land' or $ 
    strlowcase(components(0)) eq 'std_land' or $ 
    strlowcase(components(0)) eq 'std_oce') THEN BEGIN
    if nb_select gt 1 then message,'Une seule composante possible avec c_land/std_oce/std_land !!'
    ii = where(ind_tmp(0) EQ reg.map_ind,cc)
    if cc eq 0 then message,'big probleme avec c_land !!'
    nn = reg.nreg(ii(0))
    ll = 1 & if strlowcase(components(0)) eq 'std_oce' then ll = 0
                                ;-- BIZARRE SAN MARIN MAL PLACE!!
    if (strlowcase(components(0)) eq 'c_land' or $ 
        strlowcase(components(0)) eq 'c2_land') then $
      foo = where(reg.type_reg(0:nn-1,ii(0)) eq ll and $
                  strlowcase(reg.name_reg(0:nn-1,ii(0))) ne 'c_smr' and $
                  strlowcase(reg.name_reg(0:nn-1,ii(0))) ne 'c2_smr', nb_select) $
    else $
      foo = where(reg.type_reg(0:nn-1,ii(0)) eq ll, nb_select)

    components = reg.name_reg(foo,ii(0))
    indice     = replicate(ind_tmp(0),nb_select)
ENDIF



;------------------------
;  Lecture des fichiers 2D d'indices
;------------------------

IF indice_read THEN BEGIN

    if fraction then begin
        indice_reg = {map_ind:map_ind,frac:fltarr(nlon,nlat,nreg_max,n_map),$
                      reg:fltarr(nreg_max,n_map)}
    endif else indice_reg = {map_ind:map_ind,reg:fltarr(nlon,nlat,n_map)}

    FOR i = 0, n_map-1 DO BEGIN

        path = path_main+map_names(i)+'/'
        file = 'LEV_'+map_names(i)+'_'+strlowcase(grid_name)+'.nc'

        foo = findfile(path+file)
        IF foo(0) eq '' THEN BEGIN
            ccg_dirlist,dir=path+'LEV_'+map_names(i)+'_'+'*nc',list
            print,'Grille non possible...',file
            print,'   Choisir parmis : ',list
            stop
        ENDIF
    
        IF fraction THEN BEGIN

            peyl_readnc, frac_tmp, file=path+file, $
              var_name='BASIN_FRAC', $
              attrib=0
            indice_reg.frac(*,*,0:nreg(i)-1,i) = frac_tmp

            peyl_readnc, ind_tmp, file=path+file, $
              var_name='IND_REG', $
              attrib=0
            indice_reg.reg(0:nreg(i)-1,i) = ind_tmp

        ENDIF ELSE BEGIN
            peyl_readnc, indice_tmp, file=path+file, $
              var_name='BASIN', $
              attrib=0
            indice_reg.reg(*,*,i) = indice_tmp
            
        ENDELSE

    ENDFOR
    
ENDIF ELSE BEGIN
    nn = n_elements(indice_reg.map_ind) 
    if nn ne n_map then message,'Probleme structure indice_reg fournie non compatible !'
ENDELSE


;-----------------------------------------------------------------------------
;  Loop over all selected biomes, regions, country ...
;-----------------------------------------------------------------------------

debut_selection: 
name = ''
title = ''
type = 0
ii2d=-1   ;--- artificial for the loop...
ii2d_frac=-1.
ii2dtype = -1

FOR nn=0, nb_select-1 DO BEGIN

    IF special_box  THEN BEGIN

        if box_coord(0) gt box_coord(2) then begin
            ii1 = where(grid_pixel.lon ge box_coord(0) or $
                        grid_pixel.lon le box_coord(2), cc1)
        endif else begin
            ii1 = where(grid_pixel.lon ge box_coord(0) and $
                        grid_pixel.lon le box_coord(2), cc1)
        endelse
            
        ii2 = where(grid_pixel.lat ge box_coord(1) and $
                    grid_pixel.lat le box_coord(3), cc2)

        ;-- Set 1D indices for all boxes of
        ;   the rectangle 
        cc = cc1*cc2
        temp=lonarr(cc)
        FOR p=0,cc2-1 DO BEGIN
            temp(p*cc1:p*cc1+cc1-1) = replicate(ii2(p),cc1)
        ENDFOR
        ii2 = temp
        FOR p=0,cc2-1 DO BEGIN
            temp(p*cc1:p*cc1+cc1-1) = ii1
        ENDFOR
        ii1 = temp
        
        ;-- Set 2D indices
        ii3 = ii1 + ii2*nlon
        ii2d = [ii2d,ii3]
        ii2dtype = [ii2dtype,replicate(-1,cc)]
        name = name + 'box' 
        title = title + 'box'

    ENDIF ELSE BEGIN

                                ;-- Define type of map
        ii = where(reg.map_ind eq indice(nn), cc)
        if cc ne 1 then begin
            print,'Type de component non reconnu ',indice(nn),reg.map_ind
            stop
        endif
        ii = ii(0)
        if indice_reg.map_ind(ii) ne reg.map_ind(ii) then $
          message,'Probleme correspondance entre structures REG et INDICE_REG !'
        
                                ;-- Defines region to treat
        jj = where(reg.name_reg(0:reg.nreg(ii)-1,ii) eq components(nn), cc)
        IF cc ne 1 THEN BEGIN
            print,'Erreur choixreg : reg non trouvee ',components(nn)
            if cc gt 1 then begin
                print,'Regions dupliquee dans fichier !!!'
                stop
            endif
            for i=0,n_map-1 do $
              print,'       choisir parmis: ',reform(reg.name_reg(0:reg.nreg(i)-1,i))
            stop
        ENDIF
        jj = jj(0)

                                ;-- if fraction
        ok_reg = 1
        IF fraction THEN BEGIN

                                ;-- find region indice in netcdf region indice
            ii_reg = where(indice_reg.reg(0:reg.nreg(ii)-1,ii) eq reg.indice_reg(jj,ii), cc)

                                ;-- find fraction of this region
            ii_frac = where(indice_reg.frac(*,*,ii_reg(0),ii) gt 0., ccc)
            IF ccc EQ 0 THEN BEGIN
                if info then print,'Probleme, region non trouve dans indice_reg ',components(nn)
                ok_reg = 0
            ENDIF ELSE BEGIN
                ii2d = [ii2d,ii_frac]
                ii2d_frac = [ii2d_frac,(indice_reg.frac(*,*,ii_reg(0),ii))(ii_frac)]
            ENDELSE

        ENDIF ELSE BEGIN
            
                                ;-- find region indice in netcdf region indice
            ii_reg = where(indice_reg.reg(*,*,ii) eq reg.indice_reg(jj,ii), ccc)            
            IF ccc eq 0 then begin
                if info then PRINT,'Probleme, region non trouve dans indice_reg ',components(nn)
                ok_reg = 0
            endif else $
              ii2d = [ii2d,ii_reg]
        ENDELSE

        if ok_reg then begin
            type = type + reg.type_reg(jj,ii)
            ii2dtype = [ii2dtype,replicate(reg.type_reg(jj,ii),ccc)]
            name = name + reg.name_reg(jj,ii)
            title = title + reg.title_reg(jj,ii)
            IF nn lt (nb_select-1) THEN BEGIN
                name=name+'+'
                title=title+'+'
            ENDIF
        endif
        
    ENDELSE 
    
ENDFOR                       ;---- end of loop over selected biomes...

;----- Get ridd of the first value : -1 
IF n_elements(ii2d) gt 1 THEN BEGIN
    ii2d = ii2d(1:*)
    ii2dtype = ii2dtype(1:*)
    nb_point = n_elements(ii2d)
ENDIF ELSE BEGIN
   print,'NO biome defined...' 
   ind={lon:-1, lat:-1, $
        ii2d:-1, ii2d_inv:-1,$
        name:'', title:'', ii2dtype:-1, type:'glo' }
   return,ind
ENDELSE
if fraction then ii2d_frac  = ii2d_frac(1:*)

;------ Check redondance of grid point selected and for fraction case
;       Summ the fraction per grid point...

foo1 = sort(ii2d)
temp = ii2d(foo1)
foo2 = uniq(temp)    
if (n_elements(foo2) ne nb_point and fraction eq 0) then begin        
    print,'WARNING : redondance in grid point selection : ',nb_point,n_elements(foo2)
endif
ii2d = temp(foo2)
ii2dtype = ii2dtype(foo1(foo2))

nb_point = n_elements(ii2d)

if fraction then begin
    temp = ii2d_frac(foo1)
    ii2d_frac = temp(foo2)

    for i=0L,nb_point-1 do begin
        if i eq 0 then nn = foo2(i)+1 else nn = foo2(i)-foo2(i-1)
        if nn gt 1 then ii2d_frac(i) = total(temp( foo2(i)-indgen(nn) ))
    endfor

    foo = where(ii2d_frac gt 1., cc)
    if cc gt 0 then begin
        if info then begin
            print,'Warning fraction cumulees > 1 pour X/Y pts ',selection,' ',auto_string(cc,1),' ',auto_string(nb_point,1)
            print,'-> ',ii2d_frac(foo)
        endif
        ii2d_frac(foo) = 1.
    endif
endif 



;------------------------
;  SPECIAL for extending the region (add borders)
;------------------------

IF extend_reg_loc gt 0 THEN BEGIN
    if fraction then message,'Extension region impossible dans cas fraction..'
    if n_map gt 1 then message,'Extensiion region impossible si plusieur map!!!'

    ;---- Extension impossible si mix land/oce
    IF (extend_reg_loc eq 1) and (type ne 0 and type ne nb_select) THEN BEGIN
        print,'Extension of the regions with neighbours impossible : mix of land/oce !'
        goto,suite_extend
    ENDIF
    IF type eq 0 THEN type_loc = 0 else type_loc = 1

    if info then if extend_reg_loc eq 1 then print,'Extend with pixel of different type...' else $
      print,'Extend with pixel of all types...'

    ;----- Construct a 2D array of indices 1/0 for land/oce
    type_map = intarr(nlon,nlat)
    iland = where(reg.type_reg(*,0) eq 1, nland)
    FOR k=0,nland-1 DO BEGIN
        foo = where(indice_reg.reg(*,*) eq reg.indice_reg(iland(k),0), cc)
        if cc gt 0 then type_map(foo) = 1
    ENDFOR

    ;--- Loop over all points to extend regions..
    ii_extend      = [-1]
    ii_extend_type = [-1]
    iilat  = ii2d / nlon
    iilon  = ii2d - iilat*nlon
    FOR n=0L,nb_point-1 DO BEGIN

        ;--- Compute 8 extra points
        ilatp1 = min([iilat(n)+1,nlat-1])
        ilatm1 = max([iilat(n)-1,0])
        ilonp1 = iilon(n)+1 & IF (ilonp1 ge nlon) THEN ilonp1=0
        ilonm1 = iilon(n)-1 & IF (ilonm1 lt 0) THEN ilonm1=nlon-1
        ilatadd  = [iilat(n),ilatm1,iilat(n),ilatp1]
        ilonadd  = [ilonp1,iilon(n),ilonm1,iilon(n)]
        add_corner = 1
        IF add_corner THEN BEGIN
            ilatadd  = [ilatadd,ilatp1,ilatm1,ilatm1,ilatp1]
            ilonadd  = [ilonadd,ilonp1,ilonp1,ilonm1,ilonm1]
        ENDIF
        iiadd    = nlon*ilatadd + ilonadd

                                ;--- Dont care about type of points..
        if extend_reg_loc eq 2 then begin
            nadd = n_elements(iiadd)
            jj = indgen(nadd) 
                                ;--- Look if extra point are from other type and not already added...
        endif else $
          jj = where(type_map(iiadd) ne type_map(ii2d(n)), nadd) 

        ;--- Add extra points
        IF nadd gt 0 THEN BEGIN
            iiadd = iiadd(jj)
            FOR n2=0,nadd-1 DO BEGIN
                jj2 = where(ii_extend eq iiadd(n2), cc)
                IF cc le 0 THEN BEGIN
                    ii_extend = [ii_extend,iiadd(n2)]
                    ii_extend_type = [ii_extend_type,type_loc]
                ENDIF
            ENDFOR
        ENDIF
    ENDFOR 
    ;--- Add the new points to ii2d array and re-set 2D/1D indices
    IF (n_elements(ii_extend) gt 1) THEN BEGIN
        ii2d      = [ii2d,ii_extend(1:*)]
        ii2dtype  = [ii2dtype,ii_extend_type(1:*)]
        nb_point  = n_elements(ii2d)
    ENDIF

    suite_extend:

ENDIF 

;------- Set 1D indices 
iilat     = ii2d / nlon
iilon     = ii2d - iilat*nlon

if fraction then begin
    iilat_frac = ii2d_frac  / nlon
    iilon_frac = ii2d_frac  - iilat_frac*nlon
endif


;--- Restriction dans le cas de limites supplementaire fournies..

if n_elements(limits) eq 4 then begin
    if info then print,'Restriction de la regions aux limites ',limits
    if limits(0) gt limits(2) then message,'limits en long. non coherentes..'
    if limits(1) gt limits(3) then message,'limits en lat. non coherentes..'

    foo = where(grid_pixel.lat(iilat) ge limits(1) and $
                grid_pixel.lat(iilat) le limits(3) and $
                grid_pixel.lon(iilon) ge limits(0) and $
                grid_pixel.lon(iilon) le limits(2), cc)
    if cc eq 0 then begin
        print,'Probleme restriction selon limits.. ',limits
        stop
    endif
    iilat = iilat(foo)
    iilon = iilon(foo)
    ii2d  = ii2d(foo)
    if fraction then begin
        foo = where(grid_pixel.lat(iilat_frac) ge limits(1) and $
                    grid_pixel.lat(iilat_frac) le limits(3) and $
                    grid_pixel.lon(iilon_frac) ge limits(0) and $
                    grid_pixel.lat(iilon_frac) le limits(2), cc)
        if cc eq 0 then message,'Probleme restriction selon limits (fraction) ',limits
        iilat_frac = iilat_frac(foo)
        iilon_frac = iilon_frac(foo)
        ii2d_frac  = ii2d_frac(foo)
    endif 
endif


;------- Set 2D inverses indices (cas sans fraction)
foo       = lindgen(long(nlon)*long(nlat)) 
foo(ii2d) = -1
ii2d_inv  = where (foo ne -1) 

;------------------------
;  Set output
;------------------------

mean_type = 'glo'
IF type eq 0 THEN mean_type = 'oce'
IF type eq nb_select THEN mean_type = 'land'
IF special_box  THEN mean_type = 'glo'

IF fraction THEN BEGIN
    ind={lon:iilon, lat:iilat, $
         lon_frac:iilon_frac, lat_frac:iilat_frac, $
         ii2d:ii2d, ii2d_inv:ii2d_inv, $
         ii2d_frac:ii2d_frac, $+
         name:name, title:title, ii2dtype:ii2dtype, type:mean_type }
ENDIF ELSE BEGIN
    ind={lon:iilon, lat:iilat, $
         ii2d:ii2d, ii2d_inv:ii2d_inv, $
         name:name, title:title, ii2dtype:ii2dtype, type:mean_type }
ENDELSE


;---- INFO : Print the min/max of longitudes and latitudes
IF info and 0 THEN BEGIN
    print,ind.name,' lat mini : ',min(grid_pixel.lat(iilat)-grid_pixel.dlat(iilat)/2.)
    print,ind.name,' lat maxi : ',max(grid_pixel.lat(iilat)+grid_pixel.dlat(iilat)/2.)
    print,ind.name,' lon mini : ',min(grid_pixel.lon(iilon)-grid_pixel.dlon(iilon)/2.)
    print,ind.name,' lon maxi : ',max(grid_pixel.lon(iilon)+grid_pixel.dlon(iilon)/2.)
ENDIF

;--- CHEK MAP : plot selected region (indice or fraction)
IF check_map_flag THEN BEGIN
    foo = replicate(0.,nlon,nlat)
    IF fraction THEN BEGIN
        foo(ind.ii2d) = ind.ii2d_frac 
        colbar = 1
    ENDIF ELSE BEGIN
        foo(ind.ii2d) = 1
        colbar = 0
    ENDELSE

    pos = peyl_make_pos(1,RIGHTBORD=0.95,BOTBORD=0.05,$
                        LEFTBORD=0.05,TOPBORD=0.95,$
                        BOTFIX=0.,TOPFIX=0.,$
                        LEFTFIX=0.,RIGHTFIX=0.,$
                        BETWEEN=0,$
                        ORDER=order, Ncol = 1)

    if (size(check_map))(1) eq 7 then begin
        dev = 'psc'
        ccg_opendev,dev=dev,saveas=check_map(0)
        loadct,39
        col_text = 0 & col_missing = 255
    endif else begin
        WINDOW,0,TITLE='CHECK MAP',XSIZE=960,YSIZE=768;,colors=256
        CCG_RGBLOAD,file=path_main+'PROG/color_table'
;        loadct,39
        col_text = 255 & col_missing = 0
    endelse
    k = 2.
    PEYL_MAP2D,foo,$
      lons=grid_pixel.lon,$
      lats=grid_pixel.lat,$
      limits=[min(grid_pixel.lat(iilat)-k*grid_pixel.dlat(iilat)),$
             min(grid_pixel.lon(iilon)-k*grid_pixel.dlon(iilon)),$
             max(grid_pixel.lat(iilat)+k*grid_pixel.dlat(iilat)),$
             max(grid_pixel.lon(iilon)+k*grid_pixel.dlon(iilon))],$
      pos=pos,plot_axis=2,colbar=colbar,discr_pal=0,$
      nb_interval=10,leg_format='(F5.1)',leg_charsize=1.2,missing=0,$
      col_text=col_text,dlat_res=0.2,dlon_res=0.4,/grid,badcolor=col_missing
    if (size(check_map))(1) eq 7 then ccg_closedev,dev=dev
ENDIF

RETURN,ind



;------------------------------------------------------------------------------
; //////////////////// SPECIAL Pour le HELP /////////////////////////////////
;------------------------------------------------------------------------------

special_help:

print,''
print,'           |--------------------------|'
print,'           |--         HELP         --|'
print,'           |--------------------------|'
print,''
print,'  Which map do you want to see'
print,'  --> 1 : country map'
print,'  --> 2 : standard map'
print,'  --> 3 : aerocarb map'
print,'  --> 4 : carboeurope map'
print,'  --> 5 : country bis map'
print,'  --> 6 : iasa siberia'
print,'  --> 7 : new reg (nov)'
print,'  --> 8 : Special groupement!'
choice_map = get_kbrd(1)

;---- Parametres generaux
IF n_elements(grid_name) EQ 0 THEN BEGIN
    print,'--> Using default grid :: mask11'
    grid_name = 'mask11'
ENDIF

;--- lecture de la grille
grid_name_out = grid_name
grid_out = peyl_choixgrid(grid_name_out)
lon = grid_out.lon
lat = grid_out.lat
nlon = grid_out.nlon
nlat = grid_out.nlat


;--- lecture des indices et des noms de code
CASE 1 OF
    (choice_map eq '1' or choice_map eq '2' or choice_map eq '3' or $
     choice_map eq '4' or choice_map eq '5' or choice_map eq '6' or $
     choice_map eq '7') : BEGIN
        reg = map_names_all(choice_map-1)
        peyl_readnc,MAP,file=path_main+reg+'/'+'LEV_'+reg+'_'+strlowcase(grid_name)+'.nc',var_name='BASIN'
        tab = ''
        peyl_sread,tab,file=path_main+reg+'/'+reg+'_code.txt',skip=1,nl=n_reg
        
        CODE_NEW = reform(tab(0,*))
        CODEL_NEW = reform(tab(3,*))
    END
    (choice_map eq '8'): BEGIN
        CODE_NEW = reform(special_group(0,*))
        CODEL_NEW = reform(special_group(1,*))
    END
    else: goto,special_help
ENDCASE


;-- Defines positions of the plot in the page
nplot_page = 1
pos = fltarr(1,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.95,BOTBORD=0.05,$
                       LEFTBORD=0.05,TOPBORD=0.95,$
                       BOTFIX=0.,TOPFIX=0.,$
                       LEFTFIX=0.,RIGHTFIX=0.,$
                       BETWEEN=0,$
                       ORDER=order, Ncol = 1)

;-- parameters for global title
xx = 0.5
yy = 0.02

;-- Default value
csize = 1.2
cthick = 1
n_essai = 100

lat_mind=-90
lat_maxd=90
lon_mind=-180
lon_maxd=180

lat_min=lat_mind
lat_max=lat_maxd
lon_min=lon_mind
lon_max=lon_maxd

dev = ''
dev_flag = 'Ecran'

first_file  = 0
first_ecran = 0
debut_choix:
print,''
print,'           |--------------------------|'
print,'           |--   PLOT MAP OPTIONS   --|'
print,'           |--------------------------|'
print,''
print,'  --> 0 : PRINT CODE - REGION_NAME'
print,'  --> 1 : CHANGE limits [lonmin,lonmax,latmin,latmax] ',auto_string(lon_min,1),' ',auto_string(lon_max,1),' ',auto_string(lat_min,1),' ',auto_string(lat_max,1)
print,'  --> 2 : CHANGE CHARACTER SIZE ',csize
print,'  --> 3 : CHANGE DEVICE : ',dev_flag
print,'  --> 4 : PLOT MAP'
print,'  --> 5 : END'
print,''

;----- Read the answer
choice = get_kbrd(1)
CASE 1 OF 
                                ;-- Read longitude latitude
    choice eq '1':$ 
      BEGIN
        read,lon_min,lon_max,lat_min,lat_max
                                ;-- lon min
        IF lon_min LT -180 or lon_min GE 180 THEN BEGIN
            print,'--> Bad minimum longitude : using default value (180°W)'
            lon_min = lon_mind
        ENDIF
                                ;-- lon max
        IF lon_max LE -180 or lon_max GT 180 THEN BEGIN
            print,'--> Bad maximum longitude : using default value (180°E)'
            lon_max = lon_maxd
        ENDIF
                                ;-- lat min
        IF lat_min LT -90 or lat_min GE 90 THEN BEGIN
            print,'--> Bad minimum latitude : using default value (90°S)'
            lat_min = lat_mind
        ENDIF
                                ;-- lat max
        IF lat_max LE -90 or lat_max GT 90 THEN BEGIN
            print,'--> Bad maximum latitude : using default value (90°N)'
            lat_max = lat_maxd
        ENDIF
        goto,debut_choix
    END
                                ;-- Read character size
    choice eq '2':BEGIN
        read,csize
        goto,debut_choix
    END
    choice eq '3': $ 
      BEGIN
        if dev eq '' then begin
            dev = 'eps'
            dev_flag = 'File = temp_choixreg.ps'
        endif else begin
            dev = ''
            dev_flag = 'Ecran'
        endelse
        goto, debut_choix
    END
                                ;-- Plot map or Print code + name
    (choice eq '0' or choice eq '4'):$
      BEGIN
        if choice_map eq 6 and choice eq '4' then begin
            print,'MAP not possible for special groups! Use PRINT code option'
            goto, debut_choix
        endif
                                ;--- open output graphic file
        col_text=255
        IF first_ecran EQ 0 or first_file EQ 0 THEN BEGIN
            if first_file EQ 0 and (dev eq 'ps' or dev eq 'eps') then begin
                ccg_opendev,dev=dev,saveas='temp_choixreg.ps'
;device, set_font='Helvetica Bold',/TT_FONT
                loadct,40
                first_file = 1
                first_ecran = 0
            endif   
            if first_ecran EQ 0 and dev eq '' then begin
                WINDOW,0,TITLE='--- HELP ---',XSIZE=960,YSIZE=768
;device, set_font='Helvetica Bold',/TT_FONT
                CCG_RGBLOAD,file=path_main+'PROG/color_table'
                first_ecran = 1
                first_file = 0
            endif
        ENDIF        
                                ;-- Print code
        if choice eq '0' then begin
            plot,[0,1],[0,1],/nodata,col=0,xstyle=4,ystyle=4
            nn = n_elements(CODE_NEW)
;            ncol = 3
            if (choice_map eq '1' or choice_map eq '5') then begin
                charsize = 0.8
                ncol = 5 
            endif else begin
                charsize = 1.1
                ncol = 2
            endelse
            nb_lig = ceil(nn/float(ncol))
            for n=0,nn-1 do begin
                x = (n/nb_lig) * (1./ncol)
                y = 1. - (1./nb_lig/2.) - (n mod nb_lig)*(1./nb_lig)
                xyouts,x,y,code_new(n)+'='+codel_new(n),$
                  charsize=charsize,charthick=1.2,color=col_text
;                print,x,y
            endfor
            goto,debut_choix
        endif
    END
    choice eq '5':BEGIN
        if (dev eq 'ps' or dev eq 'eps') then ccg_closedev,dev=dev
        return,-1
    END
    else:goto, debut_choix
ENDCASE

;------------------------
; PLOT MAP
;------------------------
                                
;--- data selection
ii = where(grid_out.lon ge lon_min and grid_out.lon LT lon_max,n)
jj = where(grid_out.lat ge lat_min and grid_out.lat lt lat_max,o)
lon_new = reform(grid_out.lon(ii))
lat_new = reform(grid_out.lat(jj))

MAP_TMP = fltarr(n,o)
MAP_TMP_NEW = replicate(0.,n,o)
FOR kk = 0,o-1 DO BEGIN
    MAP_TMP(*,kk) = MAP(ii,jj(kk))
ENDFOR

;--- Sort and change first indice --> 1, second indice --> 2 ,...
ptrs = MAP_TMP(UNIQ(MAP_TMP,sort(MAP_TMP)))

FOR kk = 0, n_elements(ptrs)-1 DO BEGIN

    IF ptrs(kk) NE 0 THEN BEGIN
        
        ii = where(MAP_TMP EQ ptrs(kk),nn)
        IF nn NE 0 THEN MAP_TMP_NEW(ii)=kk+1
        
    ENDIF
    
ENDFOR

                                ;--- Plot map
PEYL_MAP2D,MAP_TMP_NEW,$
  lons=lon_new,$
  lats=lat_new,$
  pos=pos,plot_axis=2,missing=0,$
  col_text=col_text

openr,u,path_main+reg+'/'+'lat_lon_ref.txt',/GET_LUN
    
                                ;--- Plot code names
while not eof(u) do begin    
    lect=''
    readf,u,lect
    lect = str_sep(strcompress(strtrim(lect)), ' ')
    lon_ref = float(lect(2))
    lat_ref = float(lect(3))
    code = lect(1)
        
    IF (((lon_ref GE lon_min) and (lon_ref LE lon_max)) and ((lat_ref GE lat_min) and (lat_ref LE lat_max))) THEN $
      xyouts,lon_ref,lat_ref,code,orientation=0.,charsize=csize,charthick=cthick,alignment=0.5,color=col_text,/data
    
ENDWHILE

FREE_LUN,u
    
                                ;-- Plot main title
xyouts,xx,yy,strupcase(reg)+' CODE',orientation=0.,charsize=1.8,charthick=2.,alignment=0.5,/normal,color=col_text

goto, debut_choix


;=================================================================================
;  Definition des groupements specifiques..
;=================================================================================

deb_define_groupement:
special_group = [$
;
;----- Europe
;
                  ['c_eu15','Europe des 15','c_AUS+c_BEL+c_DEN+c_FIN+c_FRA+c_EGE+c_WGE+c_GRE+c_IRE+c_ITA+c_LUX+c_NET+c_POR+c_SWE+c_U.K+c_SPA'],$
                  ['c_eu_new','Nouveaux pays entrants','c_BUL+c_CYP+c_CZE+c_SLO+c_HUN+c_MALT+c_POL+c_ROM+c_EST+c_LAT+c_LIT'],$
                  ['c_eu27','Europe des 27','c_AUS+c_BEL+c_DEN+c_FIN+c_FRA+c_EGE+c_WGE+c_GRE+c_IRE+c_ITA+c_LUX+c_NET+c_POR+c_SWE+c_U.K+c_SPA+c_BUL+c_CYP+c_CZE+c_SLO+c_HUN+c_MALT+c_POL+c_ROM+c_EST+c_LAT+c_LIT'],$
                  ['c_eu_rest','Europe non EU','c_SWI+c_NOR+c_LIE+c_YUG+c_SAN+c_MOL+c_UKR+c_WHI+c_ALB'],$
                  ['c_eu_rest2','Europe far east','c_TUR+c_GEO+c_ARM+c_KAZ+c_UZB+c_TURM+c_AZE'],$
                  ['std_europe','Europe standard','std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_e'],$
                  ['ae_europe_trcom','Europe TRANSCOM','ae_Eur_farE+ae_Pol+ae_East_eur+ae_SEast_eur+ae_Scand+ae_Brit_Ire+ae_NWoceanic+ae_NEoceanic+ae_Noceanic+ae_Woceanic+ae_Wlowm+ae_Elowm+ae_Sibe+ae_Nibe+ae_SWoceanic+ae_Wmed+ae_Cmed+ae_Emed'],$
;
;----- Amerique
;
                  ['std_name','Amerique du Nord','std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid'],$
                  ['std_name_bor','Amerique du Nord boreale','std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop'],$
                  ['std_name_temp','Amerique du Nord temperee','std_nam_des+std_nam_for_e+std_nam_florid'],$
                  ['std_name_trcom','Amerique du nord transcom','ae_Name_bor+ae_Name_temp'],$
                  ['std_same','Amerique du sud','std_sam_for+std_sam_andes+std_eqam+std_sam_sav+std_sam_sud'],$
                  ['std_same_trop','Amerique du sud tropicale','std_sam_for+std_sam_andes+std_eqam'],$
                  ['std_same_temp','Amerique du sud temperee','std_sam_sav+std_sam_sud'],$
                  ['std_same_trcom','Amerique du sud transcom','ae_Same_trop+ae_Same_temp'],$
;
;------ Afrique
;
                  ['std_afr','Afrique','std_nafr_des+std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_safr_des+std_safr_e'],$
                  ['std_afr_north','Nord de l"Afrique','std_nafr_des'],$
                  ['std_afr_trop','Afrique tropicale','std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn'],$
                  ['std_afr_south','Sud de l"Afrique','std_safr_des+std_safr_e'],$
                  ['std_afr_trcom','Afrique transcom','ae_Nafr+ae_Safr+ae_Nafr_mag'],$
;
;------ Eurasie
;
                  ['std_euras_trop','Asie tropicale','std_eqas_o+std_eqas_e+std_nas_chines+std_nas_viet'],$
                  ['std_euras','Eurasie std','std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_inde+std_nas_japon+std_eur_turc+std_nafr_arab'],$
                  ['std_euras_bor','Eurasie boreale','std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe'],$
                  ['std_euras_temp','Eurasie temperee','std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_inde+std_nas_japon+std_eur_turc+std_nafr_arab'],$
                  ['ae_euras_trcom','Eurasie transcom','ae_euras_bor+ae_euras_temp+ae_Weur_temp'],$
                  ['ae_euras_temp_trcom','Eurasie temperee transcom','ae_euras_temp+ae_Weur_temp'],$
;
;------ Different Oceans
;
                  ['std_natl','North Atlan ocean','std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_medit'],$
                  ['std_npac','North Pacific ocean','std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct'],$
                  ['std_eqpac','Tropical pacific ocean','std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde'],$
                  ['std_eqind','Tropical indian ocean','std_eqind_o+std_eqind_e'],$
                  ['std_aust_oce','Austral ocean','std_spac_ause+std_satl_aus+std_sind_aus+std_spac_auso'],$
                  ['std_sub_trop_oce','Sub tropical ocean','std_spac_subo+std_spac_sube+std_satl_sub+std_sind_subo+std_sind_sube'],$
                  ['std_ant_oce','Sub antartic ocean','std_spac_mido+std_spac_mide+std_satl_mid+std_sind_mido+std_sind_mide'],$
;
;------ Groupement Nord
;
                  ['std_north','North total','std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid+std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_turc+std_eur_e+std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_chines+std_nas_inde+std_nas_japon+std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit'],$
                  ['ae_north_trcom','North transcom','ae_Eur_farE+ae_Pol+ae_East_eur+ae_SEast_eur+ae_Weur_temp+ae_Scand+ae_Brit_Ire+ae_NWoceanic+ae_NEoceanic+ae_Noceanic+ae_Woceanic+ae_Wlowm+ae_Elowm+ae_Sibe+ae_Nibe+ae_SWoceanic+ae_Wmed+ae_Cmed+ae_Emed+ae_Euras_bor+ae_Euras_temp+ae_Name_bor+ae_Name_temp+ae_Npac_temp+ae_Nocean+ae_Natl_temp+ae_Neur_oc+ae_NW_eur_oc+ae_Nsea+ae_Bal_sea+ae_Weur_oc+ae_Med_sea'],$
                  ['std_north_land','North Continents','std_nam_tun+std_nam_for_n+std_nam_for_o+std_nam_crop+std_nam_des+std_nam_for_e+std_nam_florid+std_eur_nordo+std_eur_norde+std_eur_o+std_eur_c+std_eur_med+std_eur_balk+std_eur_turc+std_eur_e+std_nas_tun+std_nas_sibn+std_nas_sibo+std_nas_sibc+std_nas_stepo+std_nas_stepn+std_nas_stepe+std_nas_steps+std_nas_sibe+std_nas_des+std_nas_chinen+std_nas_chines+std_nas_inde+std_nas_japon'],$
                  ['ae_north_land_trcom','Continents Nord transcom','ae_Eur_farE+ae_Pol+ae_East_eur+ae_SEast_eur+ae_Weur_temp+ae_Scand+ae_Brit_Ire+ae_NWoceanic+ae_NEoceanic+ae_Noceanic+ae_Woceanic+ae_Wlowm+ae_Elowm+ae_Sibe+ae_Nibe+ae_SWoceanic+ae_Wmed+ae_Cmed+ae_Emed+ae_Euras_bor+ae_Euras_temp+ae_Name_bor+ae_Name_temp'],$
                  ['std_north_ocean','North Oceans','std_natl_subo+std_natl_sube+std_natl_mido+std_natl_mide+std_natl_nordo+std_natl_nordc+std_natl_norde+std_npac_subo+std_npac_sube+std_npac_mido+std_npac_mide+std_npac_nordo+std_npac_norde+std_npac_arct+std_medit'],$
                  ['ae_north_ocean_trcom','Oceans nord transcom','ae_Npac_temp+ae_Nocean+ae_Natl_temp+ae_Neur_oc+ae_NW_eur_oc+ae_Nsea+ae_Bal_sea+ae_Weur_oc+ae_Med_sea'],$
;
;------ Groupement Tropiques
;
                  ['std_trop','Tropiques','std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_safr_sav+std_safr_madag+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_eqas_o+std_eqas_e+std_nas_viet+std_eqatl+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_eqind_o+std_eqind_e'],$
                  ['ae_trop_trcom','Tropiques transcom','ae_Trop_asia+ae_Same_trop+ae_Nafr+ae_Epac_trop+ae_Wpac_trop+ae_Atl_trop+ae_Ind_trop'],$
                  ['std_trop_land','Continents tropiques','std_sam_sav+std_sam_for+std_sam_andes+std_eqam+std_safr_sav+std_eqafr_for+std_eqafr_ethio+std_nafr_savs+std_nafr_savn+std_nafr_des+std_nafr_arab+std_eqas_o+std_eqas_e+std_nas_viet'],$
                  ['ae_trop_land_trcom','Continents tropiques transcom','ae_Trop_asia+ae_Same_trop+ae_Nafr'],$
                  ['std_trop_ocean','Oceans tropiques','std_eqatl+std_eqpac_o+std_eqpac_sudc+std_eqpac_eqc+std_eqpac_nordc+std_eqpac_sude+std_eqpac_eqe+std_eqpac_norde+std_eqind_o+std_eqind_e'],$
                  ['ae_trop_ocean_trcom','Oceans tropiques transcom','ae_Epac_trop+ae_Wpac_trop+ae_Atl_trop+ae_Ind_trop'],$
;
;------ Groupement Sud
;
                  ['std_south','Sud total','std_sam_sud+std_safr_des+std_safr_e+std_safr_madag+std_aus_des+std_aus_for+std_satl_aus+std_satl_mid+std_satl_sub+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube'],$
                  ['ae_south_trcom','Sud transcom','ae_Same_temp+ae_Safr+ae_Aust'],$
                  ['std_south_land','Continents sud','std_sam_sud+std_safr_des+std_safr_e+std_safr_madag+std_aus_des+std_aus_for'],$
                  ['ae_south_land_trcom','Continents sud transcom','ae_Same_temp+ae_Safr+ae_Aust'],$
                  ['std_south_ocean','Oceans sud','std_satl_aus+std_satl_mid+std_satl_sub+std_spac_ause+std_spac_auso+std_spac_mido+std_spac_mide+std_spac_subo+std_spac_sube+std_sind_aus+std_sind_mido+std_sind_mide+std_sind_subo+std_sind_sube'],$
                  ['ae_south_ocean_trcom','Oceans sud transcom','ae_Spac_temp+ae_Satl_temp+ae_Socean+ae_Sind_trop'],$
;
;------ Groupement de pays
;
                  ['c_chine','China','c_XIN+c_XIZ+c_QIN+c_GAN+c_YUN+c_SIC+c_HEI+c_NEI+c_NIN+c_JIL+c_LIA+c_GUAX+c_GUIZ+c_GUAN+c_HUNA+c_HUB+c_SHA+c_SHAX+c_HEN+c_JIA+c_FUJ+c_ZHE+c_ANH+c_JIAU+c_SHAD+c_HEB+c_BEI+c_TIA+c_SHAG']$
                ] 
goto,fin_define_groupement

END
;-




