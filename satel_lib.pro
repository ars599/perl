;+
;===================================================================
;         Special pour lire sorties TM3
;===================================================================
FUNCTION LECTURE_GLV,stat,nval_an=nval_an

if n_elements(nval_an) ne 1 then nval_an = 12
file ='/home/gcarb/contrib/globalview/globalview_06/'
file = file + strlowcase(stat) + '_00D0_ext.co2'
openr,u,file,/get_lun
lect = ''    
for j=0,15 do readf,u,lect      ;-- entete
out = replicate({new_date:0D,new_val:0.},2000)
k=0
while (not eof(u)) do begin
    readf,u,lect
    lect = strcompress(strtrim(lect,2))
    str=str_sep(lect,' ')
    out(k).new_date = double(str(0))
    out(k).new_val  = float(str(2))+float(str(3))
    k=k+1
endwhile    
free_lun,u
out = out(0:k-1)

if nval_an gt 0 then begin
    mlo = peyl_mean_cycle(out.new_val,out.new_date,nval_an,/noinfo)
endif else begin
    mlo = out
endelse

return,mlo
END

;===================================================================
;         Special pour lire sorties TM3
;===================================================================
FUNCTION LECTURE_TM3,date_int,trac_tag,tm3_new,$
                      add_offset=add_offset,$
                      correct_mlo=correct_mlo

if not keyword_set(add_offset) then add_offset=0
if not keyword_set(correct_mlo) then correct_mlo=0

offset = 343.
jour=[31,28,31,30,31,30,31,31,30,31,30,31]
jour_bis = jour
if ccg_leapyear(date_int(0)) then jour_bis(1)=29

if tm3_new eq 0 then begin
    nval_per_day_tm3 = 1L
    path = '/home/atmosphere/peylin/SIMUL_TM3/'
    file = 'TM3_dayly.an'+ auto_string(date_int(0),0) + $
      '.m' + auto_string(date_int(1),0,nb_digit=2) + '.nc'
    ntmax  = jour(date_int(1)-1)*nval_per_day_tm3
    iitime = indgen(ntmax)
endif else begin
    nval_per_day_tm3 = 4L
    if 0 then begin
        path = '/home/carbone/idelkadi/TM3/'
        file = 'mmix_'+ auto_string(date_int(0),0,nb_digit=4) + '.nc'
        ntmax  = jour(date_int(1)-1)*nval_per_day_tm3
        iitime = indgen(ntmax)
        if date_int(1) gt 1 then $
          iitime = iitime + total(jour_bis(0:date_int(1)-2))*nval_per_day_tm3
    endif else begin
        path = '/home/atmosphere/peylin/COCO_TM3/'
        file = 'mmix_tm3_'+ auto_string(date_int(0),0,nb_digit=4) + $
          '_m' + auto_string(date_int(1),0,nb_digit=2) + '.nc'
        ntmax  = jour(date_int(1)-1)*nval_per_day_tm3
        iitime = indgen(ntmax)
    endelse
endelse

idate_tm3 = intarr(ntmax,4)
idate_tm3(*,0) = date_int(0)
idate_tm3(*,1) = date_int(1)
idate_tm3(*,2) = (indgen(ntmax) / nval_per_day_tm3) + 1
idate_tm3(*,3) = (indgen(ntmax) mod nval_per_day_tm3) * (24/nval_per_day_tm3)
if nval_per_day_tm3 eq 1 then idate_tm3(*,3) = 12

print,'   file used : ',file
    
nlon = 72
nlat = 48
nlev = 19 
data_glo  = fltarr(nlon,nlat,nlev,ntmax)

var_tab = strsplit(trac_tag(0),'\+|\-',/extract)
foo = strsplit(trac_tag(0),'\+|\-')
if trac_tag(0) eq 'm001+m002+m003' then begin
    var_tab = 'mtot'
    foo = strsplit(var_tab,'\+|\-')
endif
for n=0,n_elements(var_tab)-1 do begin
    sign = 1
    if strmid(trac_tag(0),foo(n)-1,1) eq '-' then sign=-1
    peyl_readnc,temp,file=path+file,var_name=var_tab(n)
    data_glo(*,*,*,*) = data_glo(*,*,*,*) + temp(*,*,*,iitime) * sign 
    print,' ajout de ',var_tab(n),sign
endfor

                                ;--- Ajout de offset
if add_offset then data_glo = data_glo + offset

                                ;--- Correction selon les valeurs a
                                ;    MLO.. 
if correct_mlo eq 1 then begin
    print,'BIZARRE correction a MLO demandee pour TM3: Verifier altitude MLO !!!'
    stop

    mlo = LECTURE_GLV('MLO',nval_an=12)
    rr = peyl_station('MLO',/noinfo)
    grid_name = 'TM3'
    pp = peyl_getpos(grid_name,rr.lon,rr.lat,/no_interpol)
    ilev = 6
    dd =  date_int(0) + (date_int(1)-1+0.5)/12.  + (mlo.new_date(1)-mlo.new_date(0))/2.
    ii = where(mlo.new_date le dd, cc)
    if cc ge 1 then begin
        ii = ii(cc-1)
        correct = mlo.new_val(ii) - mean(data_glo(pp.lon,pp.lat,ilev,*))
    endif else message,'Humm problem de correction a mlo...'
    data_glo(*,*,*,*) = data_glo(*,*,*,*) + correct
endif

return,{val:data_glo,idate:idate_tm3,nval_per_day:nval_per_day_tm3}
END


;===================================================================
;         Special pour lire sorties LMDZ
;===================================================================
FUNCTION LECTURE_LMDZ,date_int,trac_tag,path_lmdz,$
                      add_offset=add_offset,$
                      correct_mlo=correct_mlo

if not keyword_set(add_offset) then add_offset=0
if not keyword_set(correct_mlo) then correct_mlo=0

offset = 334.9-1.3
if trac_tag(0) eq 'inv_ctrl' then offset = 335.

coef_lmdz = 1.e6*29./12.        ;-- coef pour avoir des ppm
jour=[31,28,31,30,31,30,31,31,30,31,30,31]
jour_bis = jour
if ccg_leapyear(date_int(0)) then jour_bis(1)=29
nval_per_day = 8L

                                ;--- Definition du fichier LMDz
file  = 'lmdz_96x72x19.coco2.histrac.an'+ auto_string(date_int(0),0) + $
  '.m' + auto_string(date_int(1),0,nb_digit=2) + '.nc'
print,' File used : ',file

                                ;--- Determination du path et du nb temps
for p=0,n_elements(path_lmdz)-1 do begin
    if findfile(path_lmdz(p)+file) ne '' then break
endfor 
peyl_readnc,time,file=path_lmdz(p)+file,var_name='time_counter'
ntmax = n_elements(time)

                                ;--- Definition des dates
jj = 0
if date_int(1) ge 2 then jj=total(jour(0:date_int(1)-2))
adate_lmdz = double(date_int(0)) + jj/365. + (dindgen(ntmax)+0.5)/(365*nval_per_day)
idate_lmdz = intarr(ntmax,4)
idate_lmdz(*,0) = date_int(0)
idate_lmdz(*,1) = date_int(1)
idate_lmdz(*,2) = (indgen(ntmax) / nval_per_day) + 1
idate_lmdz(*,3) = (indgen(ntmax) mod nval_per_day) * (24/nval_per_day)  
;    ccg_dec2date,adate_lmdz,yr,mo,dy,hr,mn

                                ;--- Lecture du fichier
nlon = 96
nlat = 73
nlev = 19
data_glo = fltarr(nlon,nlat,nlev,ntmax)
var_tab = strsplit(trac_tag(0),'\+|\-',/extract)
foo = strsplit(trac_tag(0),'\+|\-')
for n=0,n_elements(var_tab)-1 do begin
    sign = 1
    if strmid(trac_tag(0),foo(n)-1,1) eq '-' then sign=-1
    for p=0,n_elements(path_lmdz)-1 do begin
        if findfile(path_lmdz(p)+file) ne '' then break
    endfor 
    peyl_readnc,temp,file=path_lmdz(p)+file,var_name=var_tab(n)
    data_glo = data_glo + temp(*,*,*,*) * sign * coef_lmdz
    print,' ajout de ',var_tab(n),sign
endfor

                                ;--- Ajout de offset
if add_offset then data_glo = data_glo + offset

                                ;--- Correction selon les valeurs a
                                ;    MLO.. 
if correct_mlo eq 1 then begin
    print,'Correction seas a MLO'
    mlo = LECTURE_GLV('MLO',nval_an=12)
    rr = peyl_station('MLO',/noinfo)
    grid_name = 'LMDZ96'
    grid2 = peyl_choixgrid(grid_name,/noinfo)
    grid2.lat = reverse(grid2.lat)
    grid2.dlat = reverse(grid2.dlat)
    pp = peyl_getpos(grid_name,rr.lon,rr.lat,grid=grid2,/no_interpol)
    ilev = 6
    dd =  date_int(0) + (date_int(1)-1+0.5)/12.  + (mlo.new_date(1)-mlo.new_date(0))/2.
    ii = where(mlo.new_date le dd, cc)
    if cc ge 1 then begin
        ii = ii(cc-1)
        correct = mlo.new_val(ii) - mean(data_glo(pp.lon,pp.lat,ilev,*))
    endif else message,'Humm problem de correction a mlo...'
    data_glo(*,*,*,*) = data_glo(*,*,*,*) + correct

endif

                                ;--- Correction selon la tendance a
                                ;    MLO.. 
if correct_mlo eq 2 then begin
    print,'Correction trend a MLO'
    file_mlo='/home/carbone/peylin/SATELLITE/COMP_TOVS/COMP_DIRECT_TOVS/correction_mlo.txt'
    peyl_fread,file=file_mlo,nc=2,skip=1,mlo
    dd = date_int(0) + (date_int(1)-1+0.5)/12.  + (mlo(0,1)-mlo(0,0))/2.
    ii = where(mlo(0,*) le dd, cc)
    if cc ge 1 then begin
        ii = ii(cc-1)
        correct = mlo(1,ii)
    endif else message,'Humm problem de correction a mlo...'
    data_glo(*,*,*,*) = data_glo(*,*,*,*) + correct

endif

return,{val:data_glo,idate:idate_lmdz,nval_per_day:nval_per_day}
END
;-
