PRO main

path_in = '/bm/gdata1/fdelage/satellites/'
path_out_ps = ''
;--
nmon = 365
nnobs = 1
nnmod=2
missval = 1E+20
;--
jdeb = JULDAY(1,1,1979,1,0,0)
jfin = JULDAY(12,31,2007,24,0,0)

time_counter = TIMEGEN(final=jfin,start=jdeb,step_size=1,units='M')
station = ['tim','png']
nstat = n_elements(station)

;--------- Define structure "par_plot" passed to plot routine

par_plot = { $
;---- GENERAL
position:[-1.,-1.,-1.,-1.],$
  xrange: [0.,0.],$             ;--- [0,0] for automatic definition else the range
  yrange: [0,0],$             ;--- [0,0] for automatic definition else the range
  max_number:missval,$            ;--- maximum number to account for in the ranges
  ythick:5,$                    ;--- y thickness
  xthick:5,$                    ;--- x thickness
  cthick:5,$                    ;--- character thickness
  csize:2.,$                   ;--- character size
  color_text:0,$                ;--- color of the text
  seuil:missval,$                 ;--- seuil maximum des err prise en compte (0 sinon)..
;
;---- AXIS
ystyle:-3,$                      ;--- 1 exact yrange, 2 extended range
  xstyle:1,$                    ;--- 1 exact yrange, 2 extended range
  xlabel:0,$                    ;--- 1 use label_date,0 default label
  xtime_format:1,$ ;--- 1 to use standard time labelling of IDL, 0 default
  xtickformat:replicate('%Y',1),$ ;--- xticks format
  xticks:7,$                    ;--- number of major ticks per period (year, month...)
  xtickv:replicate(0,50),$      ;--- valeur correspondant aux ticks: (xticks + 1) valeurs
  xtickname:replicate('',50),$  ;--- text imprime pour chaque tick ('' pour automatique)
  xminor:3,$                    ;--- number or minor ticks
  yticks:0,$                    ;--- number of Y major ticks (0 for automatic)
  yminor:0,$                    ;--- number or Y minor ticks (0 for automatic)

;
;---- TITLES
xtitle:'',$                     ;--- Titre en X
  ytitle: 'mm/day',$              ;--- Titre en Y
  title: '',$                   ;--- Major Title
  xpos_title:0.5,$              ;--- [0,1] : abscisse for title posi. if coord_title= Normal/data
  ypos_title:1.1,$              ;--- [0,1] : ordonnee for title posi. if coord_title= Normal/data
  coord_title:' ',$             ;--- 'Normal': for normal coordi, 'data' for data coord else standard above the graph
  csize_title:2.,$             ;--- character size of major title
  cthick_title:3,$              ;--- character thick of major title
  footnote: '',$                ;--- String for footnote at bottom of page..
;
;---- MODEL
add_mod:replicate(1,nnmod),$    ;--- to add each component or not
  add_mod_err:replicate(0,nnmod),$ ;-- 1 to add model std_dev; 2 Mean error on the side !
  color_mod:[5,240],$
  lines_mod:replicate(0,nnmod),$
  thick_mod:replicate(2,nnmod),$   ;--- global thickness for model
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
  mod_leg:['CMAP','GPCP'],$
  obs_leg:replicate('',nnobs),$
  xpos_leg: [0.2, 0.6],$        ;-- X position of the legs in Normal/DATA coord.
  ypos_leg: [0.03, 0.03],$        ;-- Y position of the legs "
  nleg_col: 3,$                           ;-- Nb of colone (xpos_leg) for the legs used
  coord_leg:'normal',$                    ;-- Normal or Data coordonates
  incr_text_leg:1.,$                      ;-- Scaling for incremental Dy between text leg
  csize_leg  : 1.,$                       ;-- character size
  cthick_leg : 3 $                        ;-- character thick
}



file_ps = 'site.ps'
ccg_opendev,dev='psc',saveas=path_out_ps + file_ps,portrait=0
loadct,39
coeff = 3600 * 24
DATA = replicate(0.,2,n_elements(time_counter))

FOR ns = 0, nstat-1 DO BEGIN

    peyl_readnc,file=path_in+'cmap.precip.'+station[ns]+'.nc',var_name='pr',TMP
    peyl_readnc,file=path_in+'gpcp.precip.'+station[ns]+'.nc',var_name='pr',TMP2

    ;--- Reform data
    DATA(0,*) = TMP(0,0,0:347)*coeff
    DATA(1,*) = TMP2(0,0,0:347)*coeff
    ;--- PLOT data

    obs_loc = {val:0.,date:0.}

    model_loc = {date:time_counter, val:transpose(DATA)}
    par_plot.add_mod_err = 0
    par_plot.add_leg_mod(*)=1

    par_plot.title = station[ns]

    ;--- Plot proprement dit..

    peyl_plottime, model_loc, obs_loc, par_plot 


ENDFOR

ccg_closedev,dev='psc',saveas=file_ps

stop

END
            
    
