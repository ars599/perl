;+
;============================================================================
;		Find indices for specific biome(s).
;============================================================================
FUNCTION peyl_choixreg, selection, grid_name=grid_name, $
                        noinfo=noinfo, extend_reg=extend_reg

;---- Ne pas mettre le common sinon probleme dans les routines utilisatrices...
;@map2d_com 

if not keyword_set(grid_name) then grid_name='sib54'
if keyword_set(noinfo) then info = 0 else info = 1
if not keyword_set(extend_reg) then extend_reg=0

grid_name = strlowcase(grid_name)
if (grid_name ne 'sib54' and grid_name ne 'tm75' and grid_name ne 'echt21') then begin
    print,'Grille non valide...'
    stop
endif

if (grid_name ne 'sib54') then begin
    print,'!!!! Attention pour biosphere TM75 : regions non originale degradee !!!!'
endif

grid = peyl_choixgrid(grid_name,/noinfo)
nlon=grid.nlon
nlat=grid.nlat
nmon=12

;---------------------------------------------------------------
;              READ indices for specific biomes...
;---------------------------------------------------------------

path = '/home/users/peylin/idl/map/map_reg_inv/'
file = 'ind_allreg_inv_' + strlowcase(grid_name)

bb = fltarr(grid.nlon,grid.nlat)
peyl_gridread,path+file, bb, /noinfo


;---------------------------------------------------------------

ii_sa1=where(bb eq 1)
name_sa1 = 'S_AMERICA : Trop_forest'

ii_sa6=where(bb eq 12)
name_sa6 = 'S_AMERICA : tree+ground_cover'

ii_sa12=where(bb eq 8)
name_sa12 = 'S_AMERICA : Deciduous_forest'

ii_na10=where(bb eq 4)
name_na10 = 'N_AMERICA : tundra'

ii_na4=where(bb eq 5)
name_na4 = 'N_AMERICA : conif'

ii_na12=where(bb eq 9)
name_na12 = 'N_AMERICA : Deciduous_forest'

ii_af1=where(bb eq 2)
name_af1 = 'AFRIQUE : Trop_forest'

ii_af6n=where(bb eq 13)
name_af6n = 'AFRIQUE : nord-tree+ground_cover'

ii_af6s=where(bb eq 14)
name_af6s = 'AFRIQUE : sud-tree+ground_cover'

ii_eu4=where(bb eq 6)
name_eu4 = 'EUROPE : conif'

ii_eu12=where(bb eq 10)
name_eu12 = 'EUROPE : Deciduous_forest'

ii_as4=where(bb eq 7)
name_as4 = 'ASIE : conif'

ii_as12=where(bb eq 11)
name_as12 = 'ASIE : Deciduous_forest'

ii_as7=where(bb eq 16)
name_as7 = 'ASIE : tree+ground_cover'

ii_as1=where(bb eq 3)
name_as1 = 'ASIE : Trop_forest'

ii_au6=where(bb eq 15)
name_au6 = 'AUSTRALIE : tree+ground_cover'

;---------------------------------------------------------------

ii_aus=where(bb eq 20)
name_aus = 'Austral'

ii_spac=where(bb eq 31)
name_spac = 'South Pacific'

ii_satl=where(bb eq 27)
name_satl = 'South Atlantic'

ii_sindo=where(bb eq 23)
name_sindo = 'South Indian West'

ii_sinde=where(bb eq 24)
name_sinde = 'South Indian East'

ii_paceqo=where(bb eq 21)
name_paceqo = 'Pacific Equatorial West'

ii_paceqe=where(bb eq 28)
name_paceqe = 'Pacific Equatorial'

ii_atleq=where(bb eq 32)
name_atleq = 'Atlantic Equatorial'

ii_indeq=where(bb eq 33)
name_indeq = 'Indian Equatorial'

ii_npacs=where(bb eq 25)
name_npacs = 'North Pacific South'

ii_npacn=where(bb eq 29)
name_npacn = 'North Pacific North'

ii_natls=where(bb eq 26)
name_natls = 'North Atlantic South'

ii_natln=where(bb eq 30)
name_natln = 'North Atlantic North'

ii_nord=where(bb eq 22)
name_nord = 'North Artic'


;-----------------------------------------------------------------
;                 Loop over all selected biomes...
;---------------------------------------------------------------
;
if n_elements(selection) eq 1 then begin
    selection_all = str_sep(selection,'+')
endif else begin
    selection_all = selection
endelse
nb_select = n_elements(selection_all)

name=''
ii=-1   ;--- artificial for the loop...

for nn=0, nb_select-1 do begin

case selection_all(nn) of
    'sa1'   : begin & ii=[ii,ii_sa1] & name=name+name_sa1 & end 
    'sa6'   : begin & ii=[ii,ii_sa6] & name=name+name_sa6 & end
    'sa12'  : begin & ii=[ii,ii_sa12] & name=name+name_sa12 & end
    'na10'  : begin & ii=[ii,ii_na10] & name=name+name_na10 & end
    'na4'   : begin & ii=[ii,ii_na4] & name=name+name_na4 & end
    'na12'  : begin & ii=[ii,ii_na12] & name=name+name_na12 & end
    'af1'   : begin & ii=[ii,ii_af1] & name=name+name_af1 & end
    'af6n'  : begin & ii=[ii,ii_af6n] & name=name+name_af6n & end 
    'af6s'  : begin & ii=[ii,ii_af6s] & name=name+name_af6s & end
    'eu4'   : begin & ii=[ii,ii_eu4] & name=name+name_eu4 & end
    'eu12'  : begin & ii=[ii,ii_eu12] & name=name+name_eu12 & end
    'as4'   : begin & ii=[ii,ii_as4] & name=name+name_as4 & end
    'as12'  : begin & ii=[ii,ii_as12] & name=name+name_as12 & end
    'as1'   : begin & ii=[ii,ii_as1] & name=name+name_as1 & end
    'as7'   : begin & ii=[ii,ii_as7] & name=name+name_as7 & end
    'au6'   : begin & ii=[ii,ii_au6] & name=name+name_au6 & end
    'aus'   : begin & ii=[ii,ii_aus] & name=name+name_aus & end 
    'spac'  : begin & ii=[ii,ii_spac] & name=name+name_spac & end
    'satl'  : begin & ii=[ii,ii_satl] & name=name+name_satl & end
    'sindo' : begin & ii=[ii,ii_sindo] & name=name+name_sindo & end
    'sinde' : begin & ii=[ii,ii_sinde] & name=name+name_sinde & end
    'paceqo': begin & ii=[ii,ii_paceqo] & name=name+name_paceqo & end
    'paceqe': begin & ii=[ii,ii_paceqe] & name=name+name_paceqe & end
    'atleq' : begin & ii=[ii,ii_atleq] & name=name+name_atleq & end
    'indeq' : begin & ii=[ii,ii_indeq] & name=name+name_indeq & end 
    'npacs' : begin & ii=[ii,ii_npacs] & name=name+name_npacs & end
    'npacn' : begin & ii=[ii,ii_npacn] & name=name+name_npacn & end
    'natls' : begin & ii=[ii,ii_natls] & name=name+name_natls & end
    'natln' : begin & ii=[ii,ii_natln] & name=name+name_natln & end
    'nord'  : begin & ii=[ii,ii_nord] & name=name+name_nord & end
    else    : begin & print,'not a biome...',selection_all(nn) & stop & end
endcase

if nn lt (nb_select-1) then name=name+'+'
endfor      ;---- end of loop over selected biomes...

;----- Get ridd of the first value : -1 
ii=ii(1:*)
nb_point = n_elements(ii)
if nb_point le 0 then begin
   print,'no biome defined...' & stop
endif

;------- Set 1D indices 
iilat     = ii/nlon
iilon     = ii - iilat*nlon

;------- Set 2D indices 
ii2d      = ii
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

ind={lon:iilon, lat:iilat, $
     ii2d:ii2d, ii2d_inv:ii2d_inv, $
     ii3d:ii3d, ii3d_inv:ii3d_inv, $
     name:name}


;---- INFO : Print the min/max of longitudes and latitudes
if info then begin
    print,ind.name,' lat mini : ',min(grid.lat(iilat)-grid.dlat(iilat)/2.)
    print,ind.name,' lat maxi : ',max(grid.lat(iilat)+grid.dlat(iilat)/2.)
    print,ind.name,' lon mini : ',min(grid.lon(iilon)-grid.dlon(iilon)/2.)
    print,ind.name,' lon maxi : ',max(grid.lon(iilon)+grid.dlon(iilon)/2.)

;;;    for n=0,nb_point-1 do print,'lat ::: ',grid.lat(iilat(n))+grid.dlat(iilat(n))/2.
;;;    for n=0,nb_point-1 do print,'lon ::: ',grid.lon(iilon(n))+grid.dlon(iilon(n))/2.
endif


;---------------------------------------------------------------
;                         Visualisation...
;---------------------------------------------------------------

;---- Visualize biome selected
if 0 then begin
   data=fltarr(nlon,nlat)
   data(ii2d)=999
   peyl_initmap2d
   map2d
endif

;---- Visualize all biomes
if 0 then begin
   k=0
   data=fltarr(nlon,nlat)
   data(*)=0
   data(ii_sa1)=1  & k=k+n_elements(ii_sa1)
   data(ii_sa6)=12 & k=k+n_elements(ii_sa6)
   data(ii_sa12)=8 & k=k+n_elements(ii_sa12)
   data(ii_na10)=4 & k=k+n_elements(ii_na10)
   data(ii_na4)=5  & k=k+n_elements(ii_na4)
   data(ii_na12)=9 & k=k+n_elements(ii_na12)
   data(ii_af1)=2 & k=k+n_elements(ii_af1)
   data(ii_af6n)=13 & k=k+n_elements(ii_af6n)
   data(ii_af6s)=14 & k=k+n_elements(ii_af6s)
   data(ii_eu4)=6 & k=k+n_elements(ii_eu4)
   data(ii_eu12)=10 & k=k+n_elements(ii_eu12)
   data(ii_as4)=7 & k=k+n_elements(ii_as4)
   data(ii_as12)=11 & k=k+n_elements(ii_as12)
   data(ii_as1)=3 & k=k+n_elements(ii_as1)
   data(ii_as7)=16 & k=k+n_elements(ii_as7)
   data(ii_au6)=15 & k=k+n_elements(ii_au6)

;------------------------------------------------------
;   data(ii_sa1)=1 
;   data(ii_sa6)=4 
;   data(ii_sa12)=4 
;   data(ii_na10)=13 
;   data(ii_na4)=5 
;   data(ii_na12)=8 
;   data(ii_af1)=1 
;   data(ii_af6n)=3 
;   data(ii_af6s)=9
;   data(ii_eu4)=6
;   data(ii_eu12)=10
;   data(ii_as4)=14
;   data(ii_as12)=2
;   data(ii_as1)=7
;   data(ii_as7)=2
;   data(ii_au6)=11 

   peyl_initmap2d
   map2d
   stop
endif

return,ind
end
;-








