PRO MAIN

;---- Parametres generaux
path_in_nc = '/home/satellites/peylin/COCO2/'
path_out_nc = '/home/satellites/delage/PROJETS/FOSSILS/NC_FILE/' 
path_out_ps = '/home/satellites/delage/PROJETS/FOSSILS/PS_FILE/'
file_out = 'toto.nc'
path_restore = path_out_nc
restore_file = 1
;--
nlev = 19
nmon = 12
coeff = 1.e6*29./12.
time_name = ['JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY',$
             'AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER']

;-- parameters for global title
csize = 2.
cthick = 3
xx = 0.5
yy = 0.95

grid_name_out = 'LMDZ96'
grid_out = peyl_choixgrid(grid_name_out)
nlon = grid_out.nlon
nlat = grid_out.nlat

;--- GOTO // restore_session
IF restore_file THEN GOTO,restore_session

;------------------------
; READ DATA
;------------------------

file_in = 'lmdz_96x72x19.coco2.histrac.an1990.m'

DATA_NEW = fltarr(nlon,nlat,nlev,nmon)
FOR imon = 0,nmon-1 DO BEGIN
        
    peyl_readnc,DATA,file=path_in_nc+file_in+auto_string(imon+1,nb_digit=2)+'.nc',var_name='bbur_rander'
    DATA = DATA * coeff

    DATA_NEW(*,*,*,imon) = total(DATA,4)/n_elements(DATA(0,0,0,*))

ENDFOR

;--- Sauvegarde de toutes les variables de la session

restore_session:

file_save = path_restore+'out_toto.gz'
IF restore_file THEN BEGIN
    restore,file_save
ENDIF ELSE BEGIN
    print,''
    print,' --> Sauvegarde de la session dans : ',file_save
    save_cmde = "save, filename='"+file_save+"',/compress,"
    save_cmde = save_cmde+$
      'DATA_NEW'
    res = execute(save_cmde)
ENDELSE

;------------------------
;  SAUVEGARDE NETCDF
;------------------------

; dd = 1./365.
; ddd = 1./nval_per_day
; time = double(2003. + (jour_cumul(i) + dindgen(ntime_out) * ddd) * dd)

; dim_size = [grid_out.nlon,grid_out.nlat,ntime_out]
; dim_val = {D0:grid_out.lon, D1:grid_out.lat, D3:time}
; dim_name  = ['longitude','latitude','time']
; dim_title = ['longitude','latitude','time']
; dim_units = ['degrees_east','degrees_north','decimal date']

; var_name  = [varmain+'_SCIAM','ERROR_'+varmain,varmain+'_LMDZ']
; var_title = [long_varmain+' averaged mixing ratio from SCIAMACHY satellite','Error '+varmain+' (SCIAM)',$
;              long_varmain+' averaged mixing ratio from LMDZ model']
; var_units = ['ppbv','ppbv','ppbv']
; var_dim = {V0:[0,1,2],V1:[0,1,2],V2:[0,1,2]}

; fff = path_out_nc + file_out
; peyl_writenc,DATA,ERR_DATA,DATA_LMDZ,file=fff,var_dim=var_dim,var_name=var_name,$
;   var_title=var_title,var_units=var_units,dim_size=dim_size,dim_val=dim_val,$
;   dim_name=dim_name,dim_title=dim_title,dim_units=dim_units,var_misval=missval

;------------------------
; PLOT DATA
;------------------------

;-- Defines positions of the plots in the page
nplot_page = 12
pos = fltarr(4,nplot_page)
pos = peyl_make_pos(Nplot_page,RIGHTBORD=0.98,BOTBORD=0.1,$
                       LEFTBORD=0.02,TOPBORD=0.90,$
                       BOTFIX=0.05,TOPFIX=0.,$
                       LEFTFIX=0.,RIGHTFIX=0.,$
                       BETWEEN=0,$
                       ORDER=order, Ncol = 4)

;-- Defines level to plot
lev_2_plot = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
n_lev_2_plot = n_elements(lev_2_plot)

;--- open output graphic file
file_ps=path_out_ps+'toto.ps'
ccg_opendev,dev='psc',saveas=file_ps,portrait=0
loadct,39

vmin = 4
vmax = 16

DATA_NEW = reverse(DATA_NEW,2)

FOR ilev=0,n_lev_2_plot-1 DO BEGIN
    
    FOR imon = 0,nmon-1 DO BEGIN

        col = 0
        IF imon eq 0 THEN col = [0.02,0.04,0.98,0.08]

        vmin = min(DATA_NEW(*,*,ilev,imon))
        vmax = max(DATA_NEW(*,*,ilev,imon))
        
        PEYL_MAP2D,DATA_NEW(*,*,ilev,imon),$
          lons=grid_out.lon,$
          lats=grid_out.lat,pos=pos(*,imon),$
          vmin=vmin,vmax = vmax,tit=time_name(imon),$
          colbar=col,/sature_minmax,$
          nb_interval = 10, noerase = noerase

          noerase = 1
          
      ENDFOR


      noerase = 0

      xyouts,xx,yy,'Level '+auto_string(ilev+1,nb_digit=2) ,orientation=0.,charsize=csize/1.3,charthick=cthick,alignment=0.5,/normal
  

ENDFOR


;--- Close output graphic and text files
ccg_closedev,dev='psc',saveas=file_ps

stop
end
