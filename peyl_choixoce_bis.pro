;==================================================================
;
;            Prgramme pour regenerer les indices...
;                 
;==================================================================
PRO INDICE_BUILD_OCE

;---------------------------------------------------------------
;                  Define the grid and the sea_land mask
;---------------------------------------------------------------

grid_name='HAMB25'

grid = peyl_choixgrid(grid_name,/noinfo)
nlon = grid.nlon
nlat = grid.nlat
nmon = 12

;--- See-Ice mask mensuelle : 12*(mois,sst)
; fraction de glace sur (de 1 a 0)
;;infile_ice_m=path+'ice/ice_mask_hamb25'
;;ice_mask=fltarr(nlon,nlat,nmon)
;;peyl_gridread,infile_ice_m,ice_mask,/date,/f77

;--- Ocean mask
; 1 sur la mer 0 sur terre avec des fractions
path='/crc/b/pxp/OCE/'
infile_see_m=path+'ice/sea_mask_hamb25.txt'
see_mask=fltarr(nlon,nlat)
peyl_gridread,infile_see_m,see_mask,/noinfo
ii_land = where(see_mask lt 0.5)



;---------------------------------------------------------------
;              Define Special points (edges of the regions...)
;---------------------------------------------------------------

;----- define special longitudes...

lon=[-100,-70,30,40,75,100,110,130]
lat=replicate(0,n_elements(lon))
pos = peyl_getpos(grid.name,float(lon),float(lat))
plon=pos.lon

;----- define special latitudes...

lat=[-50,-20,20,40,50,60]
lon=replicate(0,n_elements(lat))
pos = peyl_getpos(grid.name,float(lon),float(lat))
plat=pos.lat


;---------------------------------------------------------------
;              Define indices for specific biomes...
;---------------------------------------------------------------

bb=see_mask 
bb(*,0:plat(0)) = 999
bb(ii_land) = 0
ii_aus=where(bb eq 999)
name_aus = 'Austral'

;---------------------------------------------------------------

bb=see_mask 
bb(0:plon(1),plat(0)+1:plat(1)) = 999
bb(plon(7)+1:*,plat(0)+1:plat(1)) = 999
bb(ii_land) = 0
ii_spac=where(bb eq 999)
name_spac = 'South Pacific'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(1)+1:plon(2),plat(0)+1:plat(1)) = 999
bb(ii_land) = 0
ii_satl=where(bb eq 999)
name_satl = 'South Atlantic'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(2)+1:plon(4),plat(0)+1:plat(1)) = 999
bb(ii_land) = 0
ii_sindo=where(bb eq 999)
name_sindo = 'South Indian West'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(4)+1:plon(7),plat(0)+1:plat(1)) = 999
bb(ii_land) = 0
ii_sinde=where(bb eq 999)
name_sinde = 'South Indian East'

;---------------------------------------------------------------

bb=see_mask 
bb(0:plon(1),plat(1)+1:plat(2)) = 999
bb(ii_land) = 0
ii_paceqo=where(bb eq 999)
name_paceqo = 'Pacific Equatorial West'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(6)+1:*,plat(1)+1:plat(2)) = 999
bb(ii_land) = 0
ii_paceqe=where(bb eq 999)
name_paceqe = 'Pacific Equatorial East'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(1)+1:plon(2),plat(1)+1:plat(2)) = 999
bb(ii_land) = 0
ii_atleq=where(bb eq 999)
name_atleq = 'Atlantic Equatorial'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(2)+1:plon(6),plat(1)+1:plat(2)) = 999
bb(plon(3)+1:plon(5),plat(2)+1:plat(3)) = 999
bb(ii_land) = 0
ii_indeq=where(bb eq 999)
name_indeq = 'Indian Equatorial'

;---------------------------------------------------------------

bb=see_mask 
bb(0:plon(0),plat(2)+1:plat(3)) = 999
bb(plon(5)+1:*,plat(2)+1:plat(3)) = 999
bb(ii_land) = 0
ii_npacs=where(bb eq 999)
name_npacs = 'North Pacific South'

;---------------------------------------------------------------

bb=see_mask 
bb(0:plon(0),plat(3)+1:plat(5)) = 999
bb(plon(5)+1:*,plat(3)+1:plat(5)) = 999
bb(ii_land) = 0
ii_npacn=where(bb eq 999)
name_npacn = 'North Pacific North'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(0)+1:plon(3),plat(2)+1:plat(4)) = 999
bb(ii_land) = 0
ii_natls=where(bb eq 999)
name_natls = 'North Atlantic South'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(0)+1:plon(3),plat(4)+1:*) = 999
bb(ii_land) = 0
ii_natln=where(bb eq 999)
name_natln = 'North Atlantic North'

;---------------------------------------------------------------

bb=see_mask 
bb(0:plon(0),plat(5)+1:*) = 999
bb(plon(3)+1:*,plat(5)+1:*) = 999
bb(ii_land) = 0
ii_nord=where(bb eq 999)
name_nord = 'North Artic'


;---------------------------------------------------------------
;          Save indices in a file
;---------------------------------------------------------------
; 
openw,u,'/crc/b/pxp/idl/lib/PEYL/peyl_choixoce.ind',/get_lun
printf,u,name_aus
printf,u,format='(2000I5)',ii_aus

printf,u,name_spac
printf,u,format='(2000I5)',ii_spac

printf,u,name_satl
printf,u,format='(2000I5)',ii_satl

printf,u,name_sindo 
printf,u,format='(2000I5)',ii_sindo

printf,u,name_sinde
printf,u,format='(2000I5)',ii_sinde

printf,u,name_paceqo
printf,u,format='(2000I5)',ii_paceqo

printf,u,name_paceqe
printf,u,format='(2000I5)',ii_paceqe

printf,u,name_atleq
printf,u,format='(2000I5)',ii_atleq

printf,u,name_indeq
printf,u,format='(2000I5)',ii_indeq

printf,u,name_npacs
printf,u,format='(2000I5)',ii_npacs

printf,u,name_npacn
printf,u,format='(2000I5)',ii_npacn

printf,u,name_natls
printf,u,format='(2000I5)',ii_natls

printf,u,name_natln
printf,u,format='(2000I5)',ii_natln

printf,u,name_nord 
printf,u,format='(2000I5)',ii_nord

free_lun,u

END

;+
;============================================================================
;		Find indices for specific oceanic region(s)
;                     (the default grid is "HAMB25")
;============================================================================
FUNCTION peyl_choixoce, selection, grid_name=grid_mame
@map2d_com

if not keyword_set(grid_name) then grid_name='HAMB25'
if grid_name ne 'HAMB25' then begin
   print,'Erreur choix_oce : pas encore possible avec grille: ',grid_name
   stop
endif

;---------------------------------------------------------------
;                  Build the indices if desired...
;---------------------------------------------------------------

;;;INDICE_BUILD_OCE


;---------------------------------------------------------------
;              READ indices for specific biomes...
;---------------------------------------------------------------

nlon = 144
nlat = 72
nmon = 12

openr,u,'/crc/b/pxp/idl/lib/PEYL/peyl_choixoce.ind',/get_lun
lect=''

readf,u,lect
readf,u,format='(2000I5)',ii_aus
name_aus=lect
;name_aus = 'Austral'

stop
;---------------------------------------------------------------

readf,u,lect
readf,u,ii_spac
name_spac=lect
;name_spac = 'South Pacific'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_satl
name_satl=lect
;name_satl = 'South Atlantic'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_sindo
name_sindo=lect
;name_sindo = 'South Indian West'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_sinde
name_sinde=lect
;name_sinde = 'South Indian East'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_paceqo
name_paceqo=lect
;name_paceqo = 'Pacific Equatorial West'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_paceqe
name_paceqe=lect
;name_paceqe = 'Pacific Equatorial East'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_atleq
name_atleq=lect
;name_atleq = 'Atlantic Equatorial'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_indeq
name_indeq=lect
;name_indeq = 'Indian Equatorial'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_npacs
name_npacs=lect
;name_npacs = 'North Pacific South'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_npacn
name_npacn=lect
;name_npacn = 'North Pacific North'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_natls
name_natls=lect
;name_natls = 'North Atlantic South'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_natln
name_natln=lect
;name_natln = 'North Atlantic North'

;---------------------------------------------------------------

readf,u,lect
readf,u,ii_nord
name_nord=lect
;name_nord = 'North Artic'


free_lun,u

;-----------------------------------------------------------------
;                 Loop over all selected biomes...
;---------------------------------------------------------------
;
nb_select = n_elements(selection)
name=''
ii=-1   ;--- artificial for the loop...

for nn=0, nb_select-1 do begin

case selection(nn) of
    'aus' : begin & ii=[ii,ii_aus] & name=name+name_aus & end 
    'spac' : begin & ii=[ii,ii_spac] & name=name+name_spac & end
    'satl' : begin & ii=[ii,ii_satl] & name=name+name_satl & end
    'sindo' : begin & ii=[ii,ii_sindo] & name=name+name_sindo & end
    'sinde' : begin & ii=[ii,ii_sinde] & name=name+name_sinde & end
    'paceqo' : begin & ii=[ii,ii_paceqo] & name=name+name_paceqo & end
    'paceqe' : begin & ii=[ii,ii_paceqe] & name=name+name_paceqe & end
    'atleq' : begin & ii=[ii,ii_atleq] & name=name+name_atleq & end
    'indeq' : begin & ii=[ii,ii_indeq] & name=name+name_indeq & end 
    'npacs' : begin & ii=[ii,ii_npacs] & name=name+name_npacs & end
    'npacn' : begin & ii=[ii,ii_npacn] & name=name+name_npacn & end
    'natls' : begin & ii=[ii,ii_natls] & name=name+name_natls & end
    'natln' : begin & ii=[ii,ii_natln] & name=name+name_natln & end
    'nord' : begin & ii=[ii,ii_nord] & name=name+name_nord & end
    else : begin & print,'not a biome...' & stop & end
endcase

if nn lt  (nb_select-1) then name=name+'+'
endfor      ;---- end of loop over selected biomes...

;----- Get ridd of the first value : -1 
ii=ii(1:*)
nb_point = n_elements(ii)
if nb_point le 0 then begin
   print,'no biome defined...' & stop
endif


;---------------------------------------------------------------
;                         Define output
;---------------------------------------------------------------

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


;---------------------------------------------------------------
;                         Visualisation...
;---------------------------------------------------------------

;---- Visualize biome selected
if 0 then begin
   peyl_initmap2d
   data=see_mask
   data(ii)=999
   map2d
endif

;---- Visualize all biomes
if 0 then begin
   peyl_initmap2d
   k=0
   data=see_mask
   data(*)=0
   data(ii_aus)=11 & k=k+n_elements(ii_aus)
   data(ii_spac)=12 & k=k+n_elements(ii_spac)
   data(ii_satl)=8 & k=k+n_elements(ii_satl)
   data(ii_sindo)=4 & k=k+n_elements(ii_sindo)
   data(ii_sinde)=5 & k=k+n_elements(ii_sinde)
   data(ii_paceqo)=9 & k=k+n_elements(ii_paceqo)
   data(ii_paceqe)=2 & k=k+n_elements(ii_paceqe)
   data(ii_atleq)=13 & k=k+n_elements(ii_atleq)
   data(ii_indeq)=14 & k=k+n_elements(ii_indeq)
   data(ii_npacs)=6 & k=k+n_elements(ii_npacs)
   data(ii_npacn)=10 & k=k+n_elements(ii_npacn)
   data(ii_natls)=7 & k=k+n_elements(ii_natls)
   data(ii_natln)=11 & k=k+n_elements(ii_natln)
   data(ii_nord)=3 & k=k+n_elements(ii_nord)

   map2d
endif

return,ind
end
;-














