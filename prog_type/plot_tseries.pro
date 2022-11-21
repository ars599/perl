PRO main

path_in = '/home/satellites/peylin/FOSSIL_EXP/OUTPUT_LMDZ96/'
path_out_ps = '/home/satellites/delage/PROJETS/PS_FILE/'
file_prefix = 'lmdz_an2002_m01_'

station_name = ['SAC','CMN','BAL','LEF011','SCH','PRS','PUY','MHD','HUN010']
ptr = [8,10,9,11]
nnmod = n_elements(ptr)

n_site = n_elements(station_name)
nmon = 12
nnobs = 1
nval_per_day = 24
missval = -500.

;-- time variables
jour      = [31,28,31,30,31,30,31,31,30,31,30,31]
n_time = nval_per_day * total(jour)
;-
jour_cumul = fltarr(nmon+1) & jour_cumul = replicate(0,nmon+1)
for n=1,nmon do jour_cumul(n) = jour_cumul(n-1)+jour(n-1)


method = 1

IF method EQ 0 THEN BEGIN       ; full year
    jdeb = [2002,1,1,1]
    jfin = [2002,12,31,24]
ENDIF
IF method EQ 1 THEN BEGIN       ; july
    jdeb = [2002,7,1,1]
    jfin = [2002,7,31,24]
ENDIF
IF method EQ 2 THEN BEGIN       ; january
    jdeb = [2002,1,1,1]
    jfin = [2002,1,31,24]
ENDIF

time_counter = 2002 + dindgen(n_time)/n_time

color = {IER_h : 254, ED_h : 170, Ed_m : 190, Tr_m : 1}

xmin = jdeb(0) + (jour_cumul(jdeb(1)-1) + jdeb(2))/365. + jdeb(3)/n_time
xmax = jfin(0) + (jour_cumul(jfin(1)-1) + jfin(2))/365. + jfin(3)/n_time
xlimit = [xmin,xmax]


;--------- Define structure "par_plot" passed to plot routine

par_plot = { $
;---- GENERAL
position:[-1.,-1.,-1.,-1.],$
;  xrange: [0.,0.],$             ;--- [0,0] for automatic definition else the range
  xrange: xlimit,$             ;--- [0,0] for automatic definition else the range
  yrange: [10.,25.],$             ;--- [0,0] for automatic definition else the range
  max_number:missval,$            ;--- maximum number to account for in the ranges
  ythick:5,$                    ;--- y thickness
  xthick:5,$                    ;--- x thickness
  cthick:5,$                    ;--- character thickness
  csize:2.5,$                   ;--- character size
  color_text:0,$                ;--- color of the text
  seuil:missval,$                 ;--- seuil maximum des err prise en compte (0 sinon)..
;
;---- AXIS
ystyle:1,$                      ;--- 1 exact yrange, 2 extended range
  xstyle:1,$                    ;--- 1 exact yrange, 2 extended range
  xtype:'',$                    ;--- 'month' for 12 monthly values else automatic
;  xticks:5,$                    ;--- number of major ticks per period (year, month...)
  xticks:xlimit(1)-xlimit(0),$                    ;--- number of major ticks per period (year, month...)
  xtickv:replicate(0,50),$      ;--- valeur correspondant aux ticks: (xticks + 1) valeurs
  xtickname:replicate('',50),$  ;--- text imprime pour chaque tick ('' pour automatique)
  xminor:2,$                    ;--- number or minor ticks
  yticks:0,$                    ;--- number of Y major ticks (0 for automatic)
  yminor:2,$                    ;--- number or Y minor ticks (0 for automatic)

;
;---- TITLES
xtitle:'',$                     ;--- Titre en X
  ytitle: 'PPM',$              ;--- Titre en Y
  title: '',$                   ;--- Major Title
  xpos_title:0.5,$              ;--- [0,1] : abscisse for title posi. if coord_title= Normal/data
  ypos_title:1.1,$              ;--- [0,1] : ordonnee for title posi. if coord_title= Normal/data
  coord_title:' ',$             ;--- 'Normal': for normal coordi, 'data' for data coord else standard above the graph
  csize_title:1.3,$             ;--- character size of major title
  cthick_title:3,$              ;--- character thick of major title
  footnote: '',$                ;--- String for footnote at bottom of page..
;
;---- MODEL
add_mod:replicate(1,nnmod),$    ;--- to add each component or not
  add_mod_err:replicate(0,nnmod),$ ;-- 1 to add model std_dev; 2 Mean error on the side !
  color_mod:[color.IER_h,color.ED_h,color.TR_m,color.ED_m],$
  lines_mod:replicate(0,nnmod),$
  thick_mod:replicate(4,nnmod),$   ;--- global thickness for model
  thick_mod_err:replicate(2,nnmod),$ ;--- thickness for model error bar
  symbol_mod: replicate(0,nnmod),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
  symtype_mod:replicate(1,nnmod),$ ;--- model symbol type
  symsize_mod:replicate(1.,nnmod),$              ;--- model symbol size
  symfill_mod:replicate(1,nnmod),$ ;--- model symbol filling
  add_mod_grise:0,$                ;-- 1: grise fournit dans structure model, 2 calculer avec courbes de mod_grise, 3: 1ere courbe de mod_grise + std_dev
  mod_grise:replicate(0,nnmod),$   ;-- Models utilise pour Option 2/3 de add_mod_grise
  color_grise:220,$

;
;---- OBSERVATIONS
add_obs: replicate(0,nnobs), $      ;-- to add observation component or not
  add_obs_err: replicate(0,nnobs),$ ;--- 1 to add error from std_dev
  color_obs:indgen(nnobs)*10,$
  lines_obs:replicate(0,nnobs),$
  thick_obs:replicate(6,nnobs),$   ;--- global thickness for obs
  thick_obs_err:replicate(2,nnobs),$ ;--- thickness for obs error bar
  symbol_obs: replicate(1,nnobs),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
  symtype_obs:replicate(3,nnobs),$ ;--- obser. symbol
  symsize_obs:replicate(1.5,nnobs),$ ;--- obser. symbol size ; 1.5
  symfill_obs:replicate(1,nnobs),$ ;--- obser. symbol filling
;
;---- LEGEND
add_leg_mod:replicate(1,nnmod),$
  add_leg_obs:replicate(0,nnobs),$
  mod_leg:['IER hourly','Edgard hourly','Transcom monthly','Edgard monthly'],$
  obs_leg:replicate('',nnobs),$
  xpos_leg: [0.01, 0.4, 0.7, 0.8],$        ;-- X position of the legs in Normal/DATA coord.
  ypos_leg: [0.03, 0.03, 0.03, 0.03],$        ;-- Y position of the legs "
  nleg_col: 3,$                           ;-- Nb of colone (xpos_leg) for the legs used
  coord_leg:'normal',$                    ;-- Normal or Data coordonates
  incr_text_leg:1.,$                      ;-- Scaling for incremental Dy between text leg
  csize_leg  : 1.,$                       ;-- character size
  cthick_leg : 3 $                        ;-- character thick
}


time_counter_mod = dblarr(n_time,nnmod)
time_counter_mod(*,0) = time_counter
time_counter_mod(*,1) = time_counter
time_counter_mod(*,2) = time_counter
time_counter_mod(*,3) = time_counter

file_ps = file_prefix +'site.ps'
ccg_opendev,dev='psc',saveas=path_out_ps + file_ps,portrait=0
loadct,39

FOR i = 0, n_site-1 DO BEGIN

    file_name = file_prefix + station_name(i)
    
    ;--- Read ASCII file
    peyl_sread,file = path_in+file_name,DATA_TMP,skip = 1

    ;--- Reform data
    DATA = reform(DATA_TMP(ptr,*))
    DATA = transpose(DATA)

    ;--- PLOT data

    obs_loc = {val:0.,date:0.}

    model_loc = {date:time_counter_mod, val:DATA}
    par_plot.add_mod_err = 0
    par_plot.add_leg_mod(*)=1

    par_plot.title = station_name(i)

    ;--- Plot proprement dit..

    peyl_plottime, model_loc, obs_loc, par_plot 


ENDFOR

ccg_closedev,dev='psc',saveas=file_ps

stop

END
            
    
