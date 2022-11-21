PRO main

;--
path_in = '/home/satellites/delage/CARBOFOR/calcul_matrices/output/'
file_prefix = 'carbofor_'
pft_str = ['4','5','6']
RU_str  = ['200','400']
period_str=['1960','2001','2041','2081']
var_names = ['GPP','NPP','evap']
convert = [365/15.,365/15.,365*24/15.]
units   = [' gC/m2',' gC/m2',' mm/m2']
nvar = n_elements(var_names)
var_names_forc = ['tair','preciprain','Rh']
str_var = ['force_reg','force_reg','force_Rh']
convert_forc = [1/14.,365*24/14.,100/14.]
units_forc   = [' K',' mm',' %']
nvar_forc = n_elements(var_names_forc)

;--- open output graphic file
file_ps=path_in+'toto.ps'
ccg_opendev,dev='psc',saveas=file_ps,portrait=0
loadct,0
;-- Defines positions of the plots in the page
nplot_page = 1
pos = fltarr(4,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.02,$
                       LEFTBORD=0.02,TOPBORD=0.90,$
                       BOTFIX=0.,TOPFIX=0.,$
                       LEFTFIX=0.1,RIGHTFIX=0.1,$
                       BETWEEN=0,$
                       ORDER=order, Ncol = 1)

;-- Defines grid
grid_name = 'CARBOFOR'
grid_out = peyl_choixgrid(grid_name)

;-- Search country indice in the grid 
ind_country = peyl_choixregnew(grid_name=grid_name,'c_fra',fraction=1)
ind_country_sav = ind_country
;-- Search boxes indice in the grid 
box = {NW:[-5,46,3,52],NER:[3,46,8,52],SW:[-5,42,3,46],SE:[3,42,8,46]}
n_box = 3
openw,unit,'results_totalregion.txt',/GET_LUN
GREY_MAP = replicate(0. ,grid_out.nlon,grid_out.nlat)
for i=0,n_box+1 do begin
   
   if i le n_box then begin
      printf,unit,'=========================================================='
      printf,unit,'  REGION (lonmin,latmin,lonmax,latmax) --> ',box.(i)
      printf,unit,'=========================================================='
   endif else begin
      printf,unit,'=========================================================='
      printf,unit,'  REGION --> LOW TEMPERATURE'
      printf,unit,'=========================================================='
   endelse


   for nt=0,n_elements(period_str)-1 do begin

      printf,unit,'----------------------------------------------------------'
      printf,unit,'  Periode '+period_str(nt)
      printf,unit,'----------------------------------------------------------'
      
      if i le n_box then begin
         ind_region = peyl_choixregnew(grid_name=grid_name,'box',box_coord=box.(i))
         ind_country.ii2d(uniq([ind_country.ii2d,ind_region.ii2d],sort([ind_country.ii2d,ind_region.ii2d]))) = -9999.
         ii = where(ind_country.ii2d ne -9999.,nn)
         GREY_MAP(ind_country.ii2d(ii)) = i+1
      endif else begin
         ;-- Read data
         peyl_readnc,DATA,file=path_in + file_prefix+period_str(nt)+'_force_reg.nc',var_name='tair'
         DATA = DATA(*,reverse(indgen(50)),*)
         tot_tmp = ((total(DATA(*,*,0:13),3))/14.)(ind_country.ii2d)
         ii = where(tot_tmp lt 281.15 and tot_tmp gt 0,nn)
         GREY_MAP(ind_country.ii2d(ii)) = i+1
      endelse
      

      for nv = 0, nvar-1 do begin
         printf,unit,'VARIABLE :: ',var_names(nv)
         for np=0,n_elements(pft_str)-1 do begin
            for nru=0,n_elements(RU_str)-1 do begin
               
               ;-- Read data
               peyl_readnc,DATA,file=path_in + file_prefix+period_str(nt)+'_'+RU_str(nru)+'_'+pft_str(np)+'_reg.nc',var_name=var_names(nv)
               DATA = DATA(*,reverse(indgen(50)),*)
               tot_tmp = (total(DATA(*,*,135:149),3))(ind_country.ii2d(ii))
               jj = where( tot_tmp gt -1000,nn)
               total = total((tot_tmp)(jj)*(grid_out.dxyp(ind_country.ii2d(ii)))(jj)*(ind_country.ii2d_frac(ii))(jj))$
                       /total((grid_out.dxyp(ind_country.ii2d(ii)))(jj)*(ind_country.ii2d_frac(ii))(jj))*convert(nv)
               printf,unit, ' Reserve utile '+RU_str(nru)+' PFT '+pft_str(np),'         ',total, units(nv)

            endfor
         endfor
      endfor

      for nv = 0, nvar_forc-1 do begin
         printf,unit,'VARIABLE :: ',var_names_forc(nv)
         
         ;-- Read data
         peyl_readnc,DATA,file=path_in + file_prefix+period_str(nt)+'_'+str_var(nv)+'.nc',var_name=var_names_forc(nv)
         DATA = DATA(*,reverse(indgen(50)),*)
         tot_tmp = (total(DATA(*,*,0:13),3))(ind_country.ii2d(ii))
         jj = where( tot_tmp gt -10000 and tot_tmp lt 10000,nn)
         total = total((tot_tmp)(jj)*(grid_out.dxyp(ind_country.ii2d(ii)))(jj)*(ind_country.ii2d_frac(ii))(jj))$
                       /total((grid_out.dxyp(ind_country.ii2d(ii)))(jj)*(ind_country.ii2d_frac(ii))(jj))*convert_forc(nv)
         
         printf,unit,total, units_forc(nv)
      endfor

   endfor   

   ind_country = ind_country_sav
endfor
free_lun,unit



PEYL_MAP2D,GREY_MAP,$
           lons=grid_out.lon,$
           lats=grid_out.lat,pos=pos,$
           vmin=1.,vmax = 6.,$
           colbar=1,discr_pal=1,$
           nb_interval = 5, noerase = 1,$
           missing=0.,badcolor=255,plot_axis = 2,col_text=0,$
           leg_format = '(I1)',leg_charsize=1.5,verticbar=1,nb_col=220
;!VALUES.F_NAN


;--- Close output graphic and text files
ccg_closedev,dev='psc',saveas=file_ps

;-- help for peyl_choixregnew
;ind = peyl_choixregnew(/help)

end
