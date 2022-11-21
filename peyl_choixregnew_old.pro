;+
;============================================================================
;		Find indices for specific biome(s).
;============================================================================
FUNCTION peyl_choixregnew_old, selection, grid_name=grid_name, $
                               box_coord = box_coord,$
                               noinfo=noinfo, extend_reg=extend_reg,$
                               special_aerocarb=special_aerocarb,$
                               grid_pixel = grid_pixel, indice_reg=indice_reg,$
                               reg = reg, nmon=nmon


;---------------------------------------------------------------
; Parametres generaux
;---------------------------------------------------------------


if not keyword_set(special_aerocarb) then special_aerocarb=0
if keyword_set(noinfo) then info = 0 else info = 1
if not keyword_set(extend_reg) then extend_reg_loc=0 else extend_reg_loc=extend_reg
if not keyword_set(grid_pixel) then grid_read=1 else grid_read=0
if not keyword_set(indice_reg) then indice_read=1 else indice_read=0
if not keyword_set(reg) then name_read=1 else name_read=0
if not keyword_set(nmon) then nmon=12

if grid_read then begin
    if not keyword_set(grid_name) then message,'Il faut fournir le nom de la grille!'
    grid_pixel = peyl_choixgrid(strlowcase(grid_name),/noinfo)
endif else begin
    if keyword_set(grid_name) then begin
        if strlowcase(grid_name) ne strlowcase(grid_pixel.name) then begin
            print,'WARNING :Conflit grid_name et grid_pixel, relectue selon grid_name..'
            grid_pixel = peyl_choixgrid(strlowcase(grid_name),/noinfo)
        endif 
    endif
endelse
nlon=grid_pixel.nlon
nlat=grid_pixel.nlat


;---------------------------------------------------------------
; Lecture du fichier de correspondance : NOM_REGION <---> INDICE
;---------------------------------------------------------------

if name_read then begin

    path = '/home/data02/peylin/REGIONALISATION_old/'
    if special_aerocarb then begin
        file = 'nom_reg_aerocarb.txt'
    endif else begin
        file = 'nom_reg.txt' 
    endelse
    openr,u,path+file,/get_lun
    lect = ''
    readf,u,lect
    nreg = float(lect)
    
    reg = {$
            title_reg:strarr(nreg),$ ;-- Full title
            name_reg:strarr(nreg),$
            indice_reg:intarr(nreg),$ 
            type_reg:intarr(nreg)$ ;-- Land:1 Oce:0
          }

    for k=0,nreg-1 do begin
        readf,u,lect
        str = str_sep(strcompress(strtrim(lect,2)), ' ')
        reg.name_reg(k) = str(0)
        reg.indice_reg(k) = fix(str(1))
        reg.type_reg(k) = fix(str(2))
        reg.title_reg(k) = peyl_concatstr(str(3:n_elements(str)-1),separator=' ')
    endfor

    free_lun,u
endif 



;---------------------------------------------------------------
;         Lecture du fichier 2D d'indices
;---------------------------------------------------------------

if indice_read then begin

    path = '/home/data02/peylin/REGIONALISATION_old/'
    if special_aerocarb then begin
        file = 'LEV_regions_aerocarb_'+strlowcase(grid_name)+'.nc'
    endif else begin
        file = 'LEV_regions_'+strlowcase(grid_name)+'.nc'
    endelse
    var_name = 'BASIN'
    foo = findfile(path+file)
    if foo(0) eq '' then begin
        ccg_dirlist,dir=path+'LEV_regions_*nc',list
        print,'Grille non possible...',file
        print,'   Choisir parmis : ',list
        stop
    endif
    
    peyl_readnc, indice_reg, file=path+file, $
      var_name=var_name, $
      attrib=0
    
endif

;help,indice_reg

;-----------------------------------------------------------------
;                 Loop over all selected biomes...
;---------------------------------------------------------------
;

if (n_elements(selection) gt 1) then begin
    print,'!! on ne prend que la premiere region (summer les regions avec des +) !!'
    if peyl_pause() then stop
endif
components = str_sep(selection(0),'+')
nb_select = n_elements(components)

if components(0) eq 'oce' then begin
    components = reg.name_reg(where(reg.type_reg eq 0))
    nb_select = n_elements(components)
    extend_reg_loc = 0
endif

if components(0) eq 'land' then begin
    components = reg.name_reg(where(reg.type_reg eq 1))
    nb_select = n_elements(components)
    extend_reg_loc = 0
endif

if components(0) eq 'glo' then begin
    components = reg.name_reg
    nb_select = n_elements(components)
    extend_reg_loc = 0
endif

if components(0) eq 'box' then begin
    if not keyword_set(box_coord) then message,'Il faut fournir box_coord !!'
    if n_elements(box_coord) ne 4 then message,'Box_coord doit contenir 4 elements!!'
    print,'Selection boite rectangulaire [lon1,lat1,lon2,lat2] : ',box_coord
    nb_select = 1
    extend_reg_loc = 0
endif

name = ''
title = ''
type = 0
ii=-1   ;--- artificial for the loop...
ii2dtype = -1

for nn=0, nb_select-1 do begin

    if components(nn) eq 'box' then begin
        ii1 = where(grid_pixel.lon ge box_coord(0) and $
                    grid_pixel.lon le box_coord(2), cc1)
        ii2 = where(grid_pixel.lat ge box_coord(1) and $
                    grid_pixel.lat le box_coord(3), cc2)
                                ;-- Set 1D indices for all boxes of
                                ;   the rectangle 
        temp=intarr(cc2*cc1)
        for p=0,cc2-1 do begin
            temp(p*cc1:p*cc1+cc1-1) = replicate(ii2(p),cc1)
        endfor
        ii2 = temp
        for p=0,cc2-1 do begin
            temp(p*cc1:p*cc1+cc1-1) = ii1
        endfor
        ii1 = temp
                                ;-- Set 2D indices
        ii3 = ii1 + ii2*nlon
        ii = [ii,ii3]
;;        type = type + 0
        ii2dtype = [ii2dtype,replicate(-1,cc)]
        name = name + 'box' 
        title = title + 'box'

    endif else begin

        jj = where(reg.name_reg eq components(nn), cc)
        if (cc ne 1) then begin
            print,'Erreur choixreg : non trouvee ',components(nn)
            stop
        endif 
        jj = jj(0)
        ii1 = where(indice_reg eq reg.indice_reg(jj), cc)
        ii = [ii,ii1]
        type = type + reg.type_reg(jj)
        ii2dtype = [ii2dtype,replicate(reg.type_reg(jj),cc)]
        
        name = name + reg.name_reg(jj)
        title = title + reg.title_reg(jj)
        if nn lt (nb_select-1) then begin
            name=name+'+'
            title=title+'+'
        endif
    endelse

endfor      ;---- end of loop over selected biomes...

;----- Get ridd of the first value : -1 
nb_point = n_elements(ii)-1
if nb_point le 0 then begin
   print,'no biome defined...' & stop
endif
ii=ii(1:*)
ii2dtype=ii2dtype(1:*)

;------- Set 2D indices 
ii2d      = ii

;------- Set 1D indices 
iilat     = ii2d / nlon
iilon     = ii2d - iilat*nlon


;----- SPECIAL for extending the region (add borders)

if extend_reg_loc then begin

                                ;---- Extension impossible si mix
                                ;     land/oce
    if (type ne 0 and type ne nb_select) then begin
        print,'Extension of the regions with neighbours impossible : mix of land/oce !'
        goto,suite_extend
    endif
    if type eq 0 then type_loc = 0 else type_loc = 1

                                ;----- Construct a 2D array of indices
                                ;      1/0 for land/oce
    type_map = intarr(nlon,nlat)
    iland = where(reg.type_reg eq 1, nland)
    for k=0,nland-1 do type_map( where(indice_reg eq reg.indice_reg(iland(k)) )) = 1

                                ;--- Loop over all points to extend
                                ;    regions..
    ii_extend      = [-1]
    ii_extend_type = [-1]
    for n=0,nb_point-1 do begin

                                ;--- Compute 8 extra points
        ilatp1 = min([iilat(n)+1,nlat-1])
        ilatm1 = max([iilat(n)-1,0])
        ilonp1 = iilon(n)+1 & if (ilonp1 ge nlon) then ilonp1=0
        ilonm1 = iilon(n)-1 & if (ilonm1 lt 0) then ilonm1=nlon-1
        ilatadd  = [iilat(n),ilatm1,iilat(n),ilatp1]
        ilonadd  = [ilonp1,iilon(n),ilonm1,iilon(n)]
        add_corner = 1
        if add_corner then begin
            ilatadd  = [ilatadd,ilatp1,ilatm1,ilatm1,ilatp1]
            ilonadd  = [ilonadd,ilonp1,ilonp1,ilonm1,ilonm1]
        endif
        iiadd    = nlon*ilatadd + ilonadd

                                ;--- Look if extra point are from
                                ;    other type and not already
                                ;    added...
        jj = where(type_map(iiadd) ne type_map(ii2d(n)), nadd)
        if nadd gt 0 then begin
            iiadd = iiadd(jj)
            for n2=0,nadd-1 do begin
                jj2 = where(ii_extend eq iiadd(n2), cc)
                if cc le 0 then begin
                    ii_extend = [ii_extend,iiadd(n2)]
                    ii_extend_type = [ii_extend_type,type_loc]
                endif
            endfor
        endif
    endfor 
                                ;--- Add the new points to ii array
                                ;    and re-set 2D/1D indices
    if (n_elements(ii_extend) gt 1) then begin
        ii2d      = [ii2d,ii_extend(1:*)]
        ii2dtype  = [ii2dtype,ii_extend_type(1:*)]
        iilat     = ii2d/nlon
        iilon     = ii2d - iilat*nlon    
        nb_point  = n_elements(ii2d)
    endif

    suite_extend:
endif 


;------- Set 2D inverses indices 
foo       = lindgen(long(nlon)*long(nlat)) 
foo(ii2d) = -1
ii2d_inv  = where (foo ne -1) 


;------- Set 3D indices 
foo2      = intarr(nlon,nlat)
foo2(ii2d)= -1
foo3      = intarr(nlon,nlat,nmon)
for n=0,nmon-1 do foo3(*,*,n)=foo2
ii3d      = where(foo3 eq -1)
foo       = lindgen(long(nlon)*long(nlat)*long(nmon))
foo(ii3d) = -1
ii3d_inv  = where(foo ne -1)


;---------------------------------------------------------------
;                         Set output
;---------------------------------------------------------------

mean_type = 'mixte'
if type eq 0 then mean_type = 'ocean'
if type eq nb_select then mean_type = 'land'


ind={lon:iilon, lat:iilat, $
     ii2d:ii2d, ii2d_inv:ii2d_inv, $
     ii3d:ii3d, ii3d_inv:ii3d_inv, $
     name:name, title:title, ii2dtype:ii2dtype, type:mean_type }


;---- INFO : Print the min/max of longitudes and latitudes
if info then begin
    print,ind.name,' lat mini : ',min(grid_pixel.lat(iilat)-grid_pixel.dlat(iilat)/2.)
    print,ind.name,' lat maxi : ',max(grid_pixel.lat(iilat)+grid_pixel.dlat(iilat)/2.)
    print,ind.name,' lon mini : ',min(grid_pixel.lon(iilon)-grid_pixel.dlon(iilon)/2.)
    print,ind.name,' lon maxi : ',max(grid_pixel.lon(iilon)+grid_pixel.dlon(iilon)/2.)
    print,' Indices 3D pour dimension (z) : ',nmon

;;;    for n=0,nb_point-1 do print,'lat ::: ',grid_pixel.lat(iilat(n))+grid_pixel.dlat(iilat(n))/2.
;;;    for n=0,nb_point-1 do print,'lon ::: ',grid_pixel.lon(iilon(n))+grid_pixel.dlon(iilon(n))/2.
endif


return,ind
end
;-








