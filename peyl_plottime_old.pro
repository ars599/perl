;+
;===========================================================================
;			TIME SERIES PLOT
;
; inputs : 
;	  * model : structure containing :
;	     - val(ntime, nbcompo) : values to plot (ntime values)
;		                       and for nbcomponents...
;	     - date : decimal date of the values. 
;	     - pfit(ntime) : values of fitted polynome of first component
;
;         * obs  : structure containing :
;	     - val(ntime) : values to plot (ntime values)
;	     - date : decimal date of the values. 
;	     - pfit(ntime) : values of fitted polynome 
;
;	  * param : structure containing all parameters :
;	     - dev: '' or 'ps' or 'psc'
;	     - outfile: name of output file
;	     - portrait: 0/1 for land or portrait
;            - add_obs  : 0/1 for adding observation
;	     - add_pfit(nbcomp) : 0/1 for each component
;	     - yrange_manual : 0/1 
;	     - max_number : maximum value to be plotted
;	     - xtitle : string for xtitle, '' for automatic title
;	     - ytitle : string for ytitle, '' for automatic title
;	     - title  : string for title, '' for automatic title
;	     - footnote : for bottom page note
;	     - symbol : number of the symbol type on the curves
;	     - nb_component : number of component of data.val to plot
;	     - text_legend : legend for each component
;	     - annot : 0/1 for plotting legend..
;	     - pos_leg : from 0 (top) to 1 (bottom)
;
; Output : The graphs...
;
;===========================================================================
PRO peyl_plottime, model, obs, param_in

;tags = tag_names(param_in)



;---------------------------------------------------------------------------
;			ASPECT OF THE PLOT
;---------------------------------------------------------------------------


;--------- Define structure "par_plot" passed to plot routine
param = { $
;
position:[-1.,-1.,-1.,-1.],$
  add_obs: [0,0,0,0,0,0], $ ;-- to add observation or not (2=only obs)
  add_mod_err:0,$
  add_obs_err:0,$
  sel_obs:[0,0,0,0,0,0], $      ;-- to select types of observations
  yrange: [0.,0.],$
  xrange: [0.,0.],$
  ythick:2,$                    ;--- y thickness
  xthick:2,$                    ;--- x thickness
  cthick:2,$                    ;--- character thickness
  csize:1.4,$                   ;--- character size
  color_mod:indgen(30)*10,$
  color_obs:indgen(10)*10,$
  color_text:0,$
  lines_mod:replicate(0,30),$
  lines_obs:replicate(0,10),$
  thick_mod:replicate(4,30),$   ;--- global thickness for model
  thick_obs:replicate(4,10),$   ;--- global thickness for obs
  thick_mod_err:replicate(2,30),$ ;--- thickness for model error bar
  thick_obs_err:replicate(2,10),$ ;--- thickness for obs error bar
  symbol_mod: 0,$               ;--- 0 no symb else numb of symb  
  symtype_mod:replicate(1,30),$ ;--- model symbol type
  symsize_mod:1.,$              ;--- model symbol size  
  symfill_mod:replicate(1,30),$ ;--- model symbol filling  
  symbol_obs: replicate(100,10),$ ;-- 0 no symb, else nb symb +line, neg: only symb
  symtype_obs:replicate(3,10),$ ;--- obser. symbol 
  symsize_obs:replicate(1.5,6),$ ;--- obser. symbol size ; 1.5
  symfill_obs:replicate(1,10),$ ;--- obser. symbol filling  
  title: '',$
  ypos_title:1.1,$              ;--- [0,1] : ordonnee for title posi.
  xpos_title:0.5,$              ;--- [0,1] : abscisse for title posi.
  csize_title:1.5,$             ;--- character size
  cthick_title:2,$              ;--- character thick
  xtitle:'',$
  xtype:'',$
  ytitle: '',$
  add_leg: 0, $
  add_leg_obs:0,$
  nb_type_obs_fix:1,$
  nb_mod_fix:1,$
  text_legend:strarr(30),$
  obs_leg:['','','','','',''],$ 
  xpos_leg: [0.2, 0.4, 0.6,0.8],$ ;; Vero 0.1; [0.4, 0.6];[0.25, 0.45, 0.65];[0.,0.2, 0.4, 0.6,0.8]
  ypos_leg: [0.1, 0.1, 0.1, 0.1],$ ;; Vero 0.7; [0.1, 0.1]
  coord_leg:' ',$
  incr_text_leg:1.,$   ;-- Scaling for incremental Dy between text leg
  csize_leg:1.,$                ;--- character size
  cthick_leg:2,$                ;--- character thick
  max_number:1.e36,$
  footnote: '',$
  seuil:0.,$           ;-- seuil en ppm pour err non prise en compte..
  ystyle:1,$                    ;--- 1 exact yrange, 2 extended range
  xstyle:2,$                    ;--- 1 exact yrange, 2 extended range
  nb_tick_period:1,$ ;--- number of major ticks per period (year, month...)
  xtickname:replicate('',50),$
  nb_minor:1$                   ;--- number or minor ticks
}

nb_minor      = 1               ;--- number or minor ticks

STRUCT_ASSIGN, param_in, param, /nozero

;-------- Determine number model components

ss = size (model.val)
if ss(0) eq 2 then nb_component = ss(2) else nb_component=1
tagmodel = tag_names(model)

;-------- Define date/ndate for all models to plot
if nb_component ne n_elements(model.date(0,*)) then begin
    model_date = replicate(model.date(0),n_elements(model.date(*,0)),nb_component)
    for g=0,nb_component-1 do model_date(*,g) = model.date(*,0)
endif else model_date = model.date 

ii = where(tagmodel eq 'NDATE',cc)
if cc ne 1 then begin
    model_ndate = replicate(n_elements(model.date(*,0)),nb_component)
endif else begin
    if nb_component gt n_elements(model.ndate(*)) then begin
        model_ndate = replicate(model.ndate(0),nb_component)
    endif else model_ndate = model.ndate 
endelse 


;-------- Determine number observation types
ss2 = size (obs.val)
if ss2(0) eq 2 then nb_type_obs = ss2(2) else nb_type_obs=1
tagobs = tag_names(obs)

;-------- Define date/ndate for all obs to plot
if nb_type_obs ne n_elements(obs.date(0,*)) then begin
    obs_date = replicate(obs.date(0),n_elements(obs.date(*,0)),nb_type_obs)
    for g=0,nb_type_obs-1 do obs_date(*,g) = obs.date(*,0)
endif else obs_date = obs.date 

ii = where(tagobs eq 'NDATE',cc)
if cc ne 1 then begin
    obs_ndate = replicate(n_elements(obs.date(*,0)),nb_type_obs)
endif else begin
    if nb_type_obs gt n_elements(obs.ndate(*)) then begin
        obs_ndate = replicate(obs.ndate(0),nb_type_obs)
    endif else obs_ndate = obs.ndate 
endelse 



;---------------------------------------------------------------------------
;		DEFINE ABSCISSE RANGE
;---------------------------------------------------------------------------

;--------- Define abscisse range for model.

x_min = 99999. & x_max = -99999.
for g=1, nb_component-1 do begin
    if model_ndate(g) gt 0 then begin
        x_min      = min([model_date(0:model_ndate(g)-1,g),x_min])
        x_max      = max([model_date(0:model_ndate(g)-1,g),x_max])
    endif
endfor 
nb_tick_tot = param.nb_tick_period
;XV_model   = x_min+findgen(nb_tick_tot+1)/param.nb_tick_period
;XV_model   = x_min+findgen((x_max-x_min) * nb_tick_tot+1)/nb_tick_tot


;--------- Define abcisse range for obs.

ii=where(param.add_obs eq 1, kk)
if (kk ne 0) then begin
    obs_date    = obs.date
    range_obs = max(obs_date) - min(obs_date)
    x_max     = x_min + max([x_max-x_min,range_obs])
    XV_obs    = min(obs_date) + findgen(nb_tick_tot+1)/param.nb_tick_period
endif

;--------- Reset the obs to the same axes range ...
;; Vero if param.add_obs then begin
ii=where(param.add_obs eq 1, kk)
if (kk ne 0) then begin
    xmin1 = MIN(model_date, MAX=xmax1)
    xmin2 = MIN(obs_date, MAX=xmax2)
    if (xmin1 le xmin2) then x_min=xmin1 else x_min=xmin2
    if (xmax1 ge xmax2) then x_max=xmax1 else x_min=xmax2
endif

;-------- For manual definition of yrange.
if (param.xrange(0) ne param.xrange(1)) then begin
    x_min = param.xrange(0)
    x_max = param.xrange(1)
endif 



XV_model   = x_min + findgen(nb_tick_tot+1)*(x_max-x_min)/nb_tick_tot



;---------------------------------------------------------------------------
;		DEFINE ORDONNEE RANGE
;---------------------------------------------------------------------------


;--------- Model yrange..
y_min   = peyl_min(model.val, -param.max_number)
y_max   = peyl_max(model.val, param.max_number)

if param.add_mod_err then begin
    for g=1, nb_component-1 do begin
        ii = where (model.std_dev(*, g) ge param.max_number, cc)
        if cc gt 0 then model.std_dev(ii, g)=0.
        y_min = peyl_min([y_min,model.val(*, g)-model.std_dev(*, g)], -param.max_number)
        y_max = peyl_max([y_max,model.val(*, g)+model.std_dev(*, g)], param.max_number)
    endfor
endif

;--------- Obs yrange..

ii_o = where( param.add_obs gt 0, iio)
if iio gt 0 then begin
    for i=0,iio-1 do begin
        y_min = peyl_min([y_min,obs.val(*,ii_o(i))], -param.max_number)
        y_max = peyl_max([y_max,obs.val(*,ii_o(i))], param.max_number)
    endfor
endif

if param.add_obs_err then begin
    ii = where (obs.std_dev ge param.max_number, cc)
;   if cc gt 0 then obs.std_dev(ii)=0.
;print, 'ii =', ii
    y_min = peyl_min([y_min,obs.val-obs.std_dev], -param.max_number)
    y_max = peyl_max([y_max,obs.val+obs.std_dev], param.max_number)   
endif

;------- Pour la non prise en compte des valeurs pour le calcul des
;        ymin et ymax, si la barre d'erreur est superieure a param.seuil (en ppmv)
;; Vero if param.add_obs then begin
;; Vero if n_elements(param.seuil) ne 0 and param.add_obs then begin
ii=where(param.add_obs eq 1, kk)
if ((n_elements(param.seuil) ne 0) and (kk ne 0)) then begin
    ii = where (obs.std_dev lt param.seuil, cc)
    if (cc ne 0) then begin
        y_min = min([obs.val(ii)-obs.std_dev(ii), obs.val(*), model.val(*)])
        y_max = max([obs.val(ii)+obs.std_dev(ii), obs.val(*), model.val(*)])
    endif
endif

;-------- For manual definition of yrange.
if (param.yrange(0) ne param.yrange(1)) then begin
    y_min = param.yrange(0)
    y_max = param.yrange(1)
endif else if param.yrange(0) eq 1 then begin
    print,'Type y to change the min and max (else to continue) :',y_min,y_max
    junk=get_kbrd(1)
    if junk eq 'y' then begin
        read,'ENTER the minimum ? ',y_min & y_min=float(y_min)
        read,'ENTER the maximum ? ',y_max & y_max=float(y_max)
    endif
endif


;---------------------------------------------------------------------------
;		DEFINE TITLES
;---------------------------------------------------------------------------

;-------- Xtitle
xtitle    = param.xtitle
;xtitle2   = param.xtitle2
;if xtitle eq '' then xtitle = 'years_model'

;-------- Ytitle
ytitle    = param.ytitle
;if ytitle eq '' then ytitle = 'CO2'

;-------- Title
title    = param.title
;if title eq '' then title = param.stat_name
;titlebis = param.titlebis

;---------------------------------------------------------------------------
;		 PLOT MODEL CURVES
;---------------------------------------------------------------------------

;;-------- Determine number components
;ss = size (model.val)
;if ss(0) eq 2 then nb_component = ss(2) else nb_component=1

;-------- Plot first the Frame..

if (total(param.position) ne (param.position(0)*4)) then begin

    if (param.position(2) le param.position(0)) or  $
      (param.position(3) le param.position(1)) then begin
        print,'Error dans vecteur position : ',param.position
        stop
    endif


    PLOT,   [0,1],[0,1], $
      /YNOZERO, $
      /NODATA, $
      min_value=-param.max_number, $ ; Vero
      max_value=param.max_number, $
      xtitle='!5'+xtitle, $
      ytitle='!5'+ytitle,$
      YRANGE=[y_min,y_max], $
      XRANGE=[x_min,x_max], $
      color=param.color_text(0),$
      ystyle=param.ystyle, $
      thick=param.thick_mod(0), $
      ythick=param.cthick, $
      charsiz=param.csize,$
      charthick=param.cthick, $
      linestyle=param.lines_mod(0), $
      position=param.position,$
      XSTYLE=5

endif else begin

    PLOT,   [0,1],[0,1], $
      /YNOZERO, $
      /NODATA, $
      min_value=-param.max_number, $ ; Vero
      max_value=param.max_number, $
      xtitle='!5'+xtitle, $
      ytitle='!5'+ytitle,$
      YRANGE=[y_min,y_max], $
      XRANGE=[x_min,x_max], $
      color=param.color_text(0),$
      ystyle=param.ystyle, $
      thick=param.thick_mod(0), $
      ythick=param.cthick, $
      charsiz=param.csize,$
      charthick=param.cthick, $
      linestyle=param.lines_mod(0), $
      XSTYLE=5

endelse

;-------- Plot first component

if (model_ndate(0) ne 0) then begin

    iival = where(model_date(*,0) ne 0,kk)
    if (kk ne 0) then begin

        OPLOT,  [model_date(iival,0)],[model.val(iival,0)], $
          color=param.color_mod(0),$
          thick=param.thick_mod(0), $
          linestyle=param.lines_mod(0)
        
;------- Set array for legend
        larr = param.lines_mod(0)
        carr = param.color_mod(0)
        tarr = param.text_legend(0)
        sarr = 0                ;--- 0 for no symbol
        farr = 0
        
;-------- Plot symbols 
        
        if param.symbol_mod gt 0 then begin
            nn = n_elements(model_date(iival,0))
            ii_sym = fix(nn/abs(param.symbol_mod)) * indgen(abs(param.symbol_mod))
            if nn le abs(param.symbol_mod) then ii_sym=indgen(nn)
            ccg_symbol, sym=param.symtype_mod(0), $
              fill=param.symfill_mod(0)
            oplot,[model_date(iival(ii_sym),0)],[model.val(iival(ii_sym),0)],$
              col= param.color_mod(0),$
              psym=8, symsize=param.symsize_mod, $
              thick = param.thick_mod(0)
            sarr = param.symtype_mod(0)
            farr = param.symfill_mod(0)
        endif

    endif
endif

;-------- Overplot all components

for g=1, nb_component-1 do begin

    if(model_ndate(g) ne 0) then begin
        iival = where(model_date(*,g) ne 0,kk)
        if (kk ne 0) then begin

            OPLOT, [model_date(iival,g)], [model.val(iival,g)], $
              thick=param.thick_mod(g), $
              linestyle=param.lines_mod(g), $
              color=param.color_mod(g), $
              min_value = -param.max_number, $ ; Vero
              max_value = param.max_number
            
                                ;----- Add symbols.
            ss=0
            ff=0
            if param.symbol_mod gt 0 then begin
                ccg_symbol, sym=param.symtype_mod(g), $
                  fill=param.symfill_mod(g)
                OPLOT, [model_date(iival(ii_sym),g)], [model.val(iival(ii_sym),g)], $
                  color=param.color_mod(g), $
                  psym=8, symsize=param.symsize_mod, $
                  thick = param.thick_mod(g)
                ss=param.symtype_mod(g)
                ff=param.symfill_mod(g)
            endif

            if (n_elements(tarr) ne 0) then begin
                tarr = [tarr,param.text_legend(g)]
                larr = [larr,param.lines_mod(g)]
                carr = [carr,param.color_mod(g)]
                sarr = [sarr,ss]
                farr = [farr,ff]
            endif else begin
                tarr = param.text_legend(g)
                larr = param.lines_mod(g)
                carr = param.color_mod(g)
                sarr = ss
                farr = ff
            endelse

        endif
    endif

endfor 

;; Vero
;-------- Fill array for legend
if ((n_elements(tarr) ne param.nb_mod_fix) and (param.add_leg eq 1)) then begin
    for g=0, param.nb_mod_fix-1 do begin
        if (g eq 0) then begin
            tarr = param.text_legend(0)
            larr = param.lines_mod(0)
            carr = param.color_mod(0)
            sarr = 0            ;--- 0 for no symbol
            farr = 0
        endif else begin
            tarr = [tarr,param.text_legend(g)]
            larr = [larr,param.lines_mod(g)]
            carr = [carr,param.color_mod(g)]
            sarr = [sarr,param.symtype_mod(g)]
            farr = [farr,param.symfill_mod(g)]
        endelse
    endfor
endif


;---------------------------------------------------------------------------
;		 PLOT OBSERVATIONS
;---------------------------------------------------------------------------

ii=where(param.add_obs eq 1, kk)
if (kk eq 0) then goto, suite_obs

;-------- Plot obs (curve if param.symbol_obs positive)
for g=0,nb_type_obs-1 do begin
    if ((param.symbol_obs(g) ge 0) and (param.add_obs(g) eq 1) and $
        (obs_ndate(g) gt 0)) then begin
        iival = indgen(obs_ndate(g))
        OPLOT, [obs_date(iival,g)], [obs.val(iival,g)],$
          psym=0,$
          symsiz=param.symsize_obs(g),$
          color=param.color_obs(0),$
          linestyle=param.lines_obs(0),$
          thick=param.thick_obs(0)
    endif
endfor

;-------- Plot symbols 
ss = 0
ff = 0
for g=0, nb_type_obs-1 do begin
    if (param.add_obs(g) eq 1 and param.symbol_obs(g) ne 0 and $
        obs_ndate(g) gt 0) then begin 

        ii_sym = fix(obs_ndate(g)/abs(param.symbol_obs(g)))*indgen(abs(param.symbol_obs(g)))
        if obs_ndate(g) le abs(param.symbol_obs(g)) then ii_sym=indgen(obs_ndate(g))

        ccg_symbol, sym=param.symtype_obs(g), $
          fill=param.symfill_obs(g)
        oplot,[obs_date(ii_sym,g)],[obs.val(ii_sym,g)], $
          psym=8, symsize=param.symsize_obs(g), $
          color=param.color_obs(g),$
          thick=param.thick_obs(g)
        ss = param.symtype_obs(g)
        ff = param.symfill_obs(g)
        
;-------- Fill array for legend
        ii=where(param.add_obs eq 1, kk)
        jj=where(param.sel_obs ne 0,ww)
        if ((kk eq ww) and (param.add_leg_obs eq 1)) then begin
            if (n_elements(tarr) ne 0) then begin
                tarr = [tarr,param.obs_leg(g)]
                larr = [larr,param.lines_obs(g)]
                carr = [carr,param.color_obs(g)]
                sarr = [sarr,ss]
                farr = [farr,ff]
            endif else begin
                tarr = param.obs_leg(g)
                larr = param.lines_obs(g)
                carr = param.color_obs(g)
                sarr = ss
                farr = ff
            endelse
        endif
        
    endif
endfor

;-------- Add error bar

if param.add_obs_err then begin
    ii = where (obs.std_dev ge param.max_number or obs.std_dev ge param.seuil, cc)
    if cc gt 0 then obs.std_dev(ii)=0.
    ii_err = indgen(n_elements(obs_date))
    peyl_errplot, obs_date(ii_err), $
      obs.val(ii_err)+obs.std_dev(ii_err), $
      obs.val(ii_err)-obs.std_dev(ii_err), $
      color=param.color_obs(0),$
      thick=param.thick_obs_err(0)
endif


suite_obs:

;; Vero
ii=where(param.add_obs ne 0,kk)
jj=where(param.sel_obs ne 0,ww)
if ( ((kk eq 0) or (kk ne ww)) and (param.add_leg_obs eq 1) ) then begin
    for g=0, param.nb_type_obs_fix-1 do begin
        if (param.sel_obs(g) ne 0) then begin
            ccg_symbol, sym=param.symtype_obs(g), $
              fill=param.symfill_obs(g)
            ss = param.symtype_obs(g)
            ff = param.symfill_obs(g)
            if (n_elements(tarr) ne 0) then begin
                tarr = [tarr,param.obs_leg(g)]
                larr = [larr,param.lines_obs(g)]
                carr = [carr,param.color_obs(g)]
                sarr = [sarr,ss]
                farr = [farr,ff]
            endif else begin
                tarr = param.obs_leg(g)
                larr = param.lines_obs(g)
                carr = param.color_obs(g)
                sarr = ss
                farr = ff
            endelse
        endif
    endfor
endif


;---------------------------------------------------------------------------
;		 PLOT TITLES, LEGEND, AXIS
;---------------------------------------------------------------------------

;-------- Plot main title
new = convert_coord(param.xpos_title,param.ypos_title,/NORMAL,/TO_DATA)

;xyouts,new(0),new(1), $
xyouts,!x.crange(0)+(!x.crange(1)-!x.crange(0))*param.xpos_title,!y.crange(0)+(!y.crange(1)-!y.crange(0))*param.ypos_title, $
  title,ali=.5,$
  col=param.color_text(0),$
  charthick=param.cthick_title,$
  charsize=param.csize_title,$
  /DATA


;-------- Plot x bottom axis

if (param.xtickname(0) eq '') then begin

xtickname = auto_string(xv_model,1) 
;;string(format='(f4.1)',XV_model)

;print,xv_model

if XV_model(n_elements(XV_model)-1) - XV_model(0) le 1. then begin
;    xtickname=month_sign
;    nb_tick=12
    s=size(model.val)
    case param.xtype of
        'hour' : begin
            nb_hours=s(1)
            int_time=intarr(nb_hours)
            int_time=INDGEN(nb_hours)+1
            carac_time=strarr(nb_hours)
            for ind=0, nb_hours-1 do begin
                if (int_time(ind) lt 10) then $
                  carac_time(ind)=STRING(int_time(ind),FORMAT='(I1.1)') $
                else if ((int_time(ind) ge 10) and (int_time(ind) lt 100)) then $
                  carac_time(ind)=STRING(int_time(ind),FORMAT='(I2.2)') $
                else if ((int_time(ind) ge 100) and (int_time(ind) lt 1000)) then $
                  carac_time(ind)=STRING(int_time(ind),FORMAT='(I3.3)') $
                else carac_time(*)=STRING(int_time(*))
            endfor
;            xtickname=carac_time+'/24'
            xtickname=carac_time
            nb_tick_tot=nb_hours-1
            csize=0.9
        end
        'day' : begin
            nb_days=s(1)
            int_time=intarr(nb_days)
            int_time=INDGEN(nb_days)+1
            carac_time=strarr(nb_days)
            for ind=0, nb_days-1 do begin
                if (int_time(ind) lt 10) then $
                  carac_time(ind)=STRING(int_time(ind),FORMAT='(I1.1)') $
                else if ((int_time(ind) ge 10) and (int_time(ind) lt 100)) then $
                  carac_time(ind)=STRING(int_time(ind),FORMAT='(I2.2)') $
                else if ((int_time(ind) ge 100) and (int_time(ind) lt 1000)) then $
                  carac_time(ind)=STRING(int_time(ind),FORMAT='(I3.3)') $
                else carac_time(*)=STRING(int_time(*))
            endfor
            xtickname=carac_time
            if (n_elements(param.nb_tick_period) eq 0) then nb_tick_tot=nb_days-1 $
            else xtickname=carac_time((indgen(nb_tick_tot+1))*nb_days/nb_tick_tot-1)
            csize=0.9
        end
        'month' : begin
            xtickname=['J','F','M','A','M','J','J','A','S','O','N','D']
            nb_tick_tot=12-1
        end
        'year' : begin
            nb_years=s(1)
            int_time=intarr(nb_years)
            int_time=INDGEN(nb_years)+1
            carac_time=strarr(nb_years)
            carac_time(*)=STRING(int_time(*),FORMAT='(I1.1)')
            xtickname=carac_time
            nb_tick_tot=nb_years-1
        end
        else : begin
;            print, 'Pb avec le choix de la legende de l axe des temps, param.xtitle = ',  param.xtitle
        end 
    endcase
endif 
endif else begin
    xtickname = param.xtickname
endelse

;print,'cccc ',xtickname
;stop

axis,xaxis=0,xticks=nb_tick_tot,xminor=param.nb_minor,$
  xtitle='!5'+xtitle,$
  charthick=param.cthick ,$
  col=param.color_text(0),$
  xtickname=xtickname,$
  charsize=param.csize,$
  xthick=param.xthick

;-------- Plot x top axis
axis,xaxis=1,xticks=nb_tick_tot,xminor=param.nb_minor,$
;     xtitle='!5!'+xtitle2,$
charsiz=param.csize,$
  col=param.color_text(0),$
  xtickname = replicate(' ',30),$
  charthick=param.cthick,$
  xthick=param.xthick

;-------- Legend for components..
;

if param.add_leg then begin
    n_deb=0 ;; Boucle Vero
    n_fin=0
    for g=0, n_elements(param.xpos_leg)-1 do begin
        if (g ne (n_elements(param.xpos_leg)-1)) then begin
            nb_leg_loc=ceil(float(n_elements(tarr))/$
                            float(n_elements(param.xpos_leg)))
            if (g eq 0) then n_deb=n_fin else n_deb=n_fin+1
            n_fin=n_deb+nb_leg_loc-1
        endif else begin
            nb_leg_loc=floor(float(n_elements(tarr))/$
                             float(n_elements(param.xpos_leg)))
            n_deb=n_fin+1
            n_fin=n_deb+nb_leg_loc-1
        endelse
        tarr_loc = strarr(nb_leg_loc)
        larr_loc = intarr(nb_leg_loc)
        carr_loc = intarr(nb_leg_loc)
        sarr_loc = intarr(nb_leg_loc)
        farr_loc = intarr(nb_leg_loc)
        print, ' nb_leg_loc = ', nb_leg_loc
        tarr_loc(0:nb_leg_loc-1) = tarr(n_deb:n_fin)
        larr_loc(0:nb_leg_loc-1) = larr(n_deb:n_fin)
        carr_loc(0:nb_leg_loc-1) = carr(n_deb:n_fin)
        sarr_loc(0:nb_leg_loc-1) = sarr(n_deb:n_fin)
        farr_loc(0:nb_leg_loc-1) = farr(n_deb:n_fin)
        if param.coord_leg eq 'data' then begin
            x=!x.crange(0)+(!x.crange(1)-!x.crange(0))*(param.xpos_leg(g))
            y=!y.crange(0)+(!y.crange(1)-!y.crange(0))*(param.ypos_leg(g))
            peyl_llegend,x=x,y=y,$
              tarr=tarr_loc,larr=larr_loc,carr=carr_loc,$
              charthick=param.cthick_leg,$
              charsize=param.csize_leg,$
              thick=param.cthick,$
              sarr=sarr_loc,$
              farr=farr_loc,$
              incr_text=param.incr_text_leg,$
              /data
        endif else begin
            x=param.xpos_leg(g)
            y=param.ypos_leg(g)
            peyl_llegend,x=x,y=y,$
              tarr=tarr_loc,larr=larr_loc,carr=carr_loc,$
              charthick=param.cthick_leg,$
              charsize=param.csize_leg,$
              thick=param.cthick,$
              sarr=sarr_loc,$
              farr=farr_loc,$
              incr_text=param.incr_text_leg
        endelse
    endfor
endif

if (!p.multi(0) eq 0) then begin
                                ;---- Footnote at the bottom page
    if param.footnote ne '' then begin
        peyl_footnote,param.footnote
        print,param.footnote
    endif
endif

end
;-





