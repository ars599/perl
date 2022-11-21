;+
;============================================================================
;		Find indices for specific DEF/BBUR region(s)
;                     (the default grid is "TM75")
;============================================================================
FUNCTION peyl_choixdef, selection, grid_name=grid_name
@map2d_com

;---------------------------------------------------------------
;                  Define the grid and the sea_land mask
;---------------------------------------------------------------

if not keyword_set(grid_name) then grid_name='TM75'

grid = peyl_choixgrid(grid_name,/noinfo)
nlon = grid.nlon
nlat = grid.nlat
nmon = 12

;--- Ocean mask
; 1 sur la mer 0 sur terre avec des fractions

path='/home/gcarb/contrib/chgrid/'
infile_see_m=path+'land_mask_' + strlowcase(grid_name)

see_mask=fltarr(nlon,nlat)
peyl_gridread,infile_see_m,see_mask,/noinfo

;;ii_land = where(see_mask ge 0.5)
ii_oce = where(see_mask eq 0.)


;---------------------------------------------------------------
;              Define Special points (edges of the regions...)
;---------------------------------------------------------------

;----- define special longitudes...

lon=[-30,60]
lat=replicate(0,n_elements(lon))
pos = peyl_getpos(grid.name,float(lon),float(lat))
plon=pos.lon


;---------------------------------------------------------------
;              Define indices for specific biomes...
;---------------------------------------------------------------

bb=see_mask 
bb(0:plon(0),*) = 999
bb(ii_oce) = 0
ii_sam=where(bb eq 999)
name_sam = 'Amerique'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(0)+1:plon(1),*) = 999
bb(ii_oce) = 0
ii_afr=where(bb eq 999)
name_afr = 'Afrique-Europe'

;---------------------------------------------------------------

bb=see_mask 
bb(plon(1)+1:*,*) = 999
bb(ii_oce) = 0
ii_asi=where(bb eq 999)
name_asi = 'Asie'


;-----------------------------------------------------------------
;                 Loop over all selected biomes...
;---------------------------------------------------------------
;
nb_select = n_elements(selection)
name=''
ii=-1   ;--- artificial for the loop...

for nn=0, nb_select-1 do begin

case selection(nn) of
    'sam' : begin & ii=[ii,ii_sam] & name=name+name_sam & end 
    'afr' : begin & ii=[ii,ii_afr] & name=name+name_afr & end
    'asi' : begin & ii=[ii,ii_asi] & name=name+name_asi & end
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
   data(ii_sam)=1 & k=k+n_elements(ii_sam)
   data(ii_afr)=2 & k=k+n_elements(ii_afr)
   data(ii_asi)=3 & k=k+n_elements(ii_asi)
   map2d
   stop
endif

return,ind
end
;-














