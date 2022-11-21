;+
;===========================================================================
;			TIME SERIES PLOT
;
; inputs : 
;	  * model : structure containing :
;	     - val(ntmax, nbcomp) : values to plot (ntime values)
;		                       and for nbcomponents...
;	     - date(ntmax, nbcomp) : decimal date of the values.
;            - ndate(nbcomp) : OPTIONAL to give the nb of true
;                                values per components
;            - std_dev(ntmax, nbcomp) : OPTIONAL for the std deviations 
;            - grise_date(ntmax): OPTIONAL for the date of grey zone
;            - grise_min(ntmax): OPTIONAL for minimum of grey zone
;            - grise_max(ntmax): OPTIONAL for maximum of grey zone
;
;         * obs  : structure containing :
;	     - val(ntimemax, nbcomp) : values to plot (ntime values)
;		                       and for nbcomponents...
;	     - date(ntimemax, nbcomp) : decimal date of the values.
;            - nbdate(nbcomp) : OPTIONAL to give the nb of true
;                                values per components
;            - std_dev(ntimemax, nbcomp) : OPTIONAL for the std deviations 
;
;	  * param : structure containing all parameters (see below)
;
; Output : The graphs...
;
;===========================================================================
PRO peyl_plottime, model, obs, param_in

;---------------------------------------------------------------------------
;			ASPECT OF THE PLOT
;---------------------------------------------------------------------------

;--------- Define structure "param" passed to plot routine
nnobs = 40
nnmod = 50
npos_leg = n_elements(param_in.xpos_leg) > 4

param = { $
;---- GENERAL
noerase:0,$                   ;--- 1 for overplotting          
position:[-1.,-1.,-1.,-1.],$
xrange: [0.,0.],$             ;--- [0,0] for automatic definition else the range
xlimit: [0.,0.],$             ;--- [0,0] for automatic definition else the range
yrange: [0.,0.],$             ;--- [0,0] for automatic definition else the range
max_number:1.e20,$            ;--- maximum number to account for in the ranges
ythick:2,$                    ;--- y thickness
xthick:2,$                    ;--- x thickness
cthick:2,$                    ;--- character thickness
csize:1.2,$                   ;--- character size
color_table:0,$              ;--- color table to load
color_text:0,$                ;--- color of the text
seuil:1.e36,$                 ;--- seuil maximum des err prise en compte (0 sinon)..
;
;---- AXIS 
ystyle:1,$                      ;--- 1 exact yrange, 2 extended range
xstyle:1,$                    ;--- 1 exact yrange, 2 extended range
xlabel:0,$                    ;--- 1 use label_date,0 default label
xtime_format:0,$              ;--- 1 to use standard time labelling of IDL, 0 default
xtickformat:replicate('%M!C%Y',1),$ ;--- xticks format
xticks:3,$                    ;--- number of major ticks per period (year, month...)
xtickv:replicate(0L,50),$      ;--- valeur correspondant aux ticks: (xticks + 1) valeurs
xtickname:replicate('',50),$  ;--- text imprime pour chaque tick ('' pour automatique)
xminor:5,$                    ;--- number or minor ticks
yticks:0,$                    ;--- number of Y major ticks (0 for automatic)
yminor:0,$                    ;--- number or Y minor ticks (0 for automatic)
xcharsize:1.,$
ycharsize:1.,$

;
;---- TITLES
xtitle:'',$                     ;--- Titre en X
ytitle: '',$                  ;--- Titre en Y
title: '',$                   ;--- Major Title
xpos_title:0.5,$              ;--- [0,1] : abscisse for title posi. if coord_title= Normal/data
ypos_title:1.1,$              ;--- [0,1] : ordonnee for title posi. if coord_title= Normal/data
coord_title:' ',$             ;--- 'Normal': for normal coordi, 'data' for data coord else standard above the graph 
csize_title:1.5,$             ;--- character size of major title
cthick_title:4,$              ;--- character thick of major title
footnote: '',$                ;--- String for footnote at bottom of page..
;
;---- MODEL
add_mod:replicate(1,nnmod),$    ;--- to add each component or not
  mod_order:indgen(nnmod),$     ;--- to give the order to plot the curves.. 
  add_mod_err:replicate(0,nnmod),$ ;-- 1 to add model std_dev; 2 Mean error on the side!; 3: 1+2
  color_mod:indgen(nnmod)*10,$
  lines_mod:replicate(0,nnmod),$
  thick_mod:replicate(4,nnmod),$   ;--- global thickness for model
  thick_mod_err:replicate(2,nnmod),$ ;--- thickness for model error bar
  symbol_mod: replicate(0,nnmod),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
  symtype_mod:replicate(1,nnmod),$ ;--- model symbol type
  symsize_mod:replicate(1.,nnmod),$              ;--- model symbol size  
  symfill_mod:replicate(1,nnmod),$ ;--- model symbol filling  
  add_mod_grise:0,$                ;-- 1: grise fournit dans structure model, 2 calculer avec courbes de mod_grise, 3: 1ere courbe de mod_grise  + std_dev
  mod_grise:replicate(0,nnmod),$   ;-- Models utilise pour Option 2/3 de add_mod_grise
  color_table_grise:0,$              ;--- color table to load for grey
  color_grise:220,$

;
;---- OBSERVATIONS 
add_obs: replicate(0,nnobs), $      ;-- to add observation component or not
add_obs_err: replicate(0,nnobs),$ ;--- 1 to add error from std_dev
color_obs:indgen(nnobs)*10,$
lines_obs:replicate(0,nnobs),$
thick_obs:replicate(4,nnobs),$   ;--- global thickness for obs
thick_obs_err:replicate(2,nnobs),$ ;--- thickness for obs error bar
symbol_obs: replicate(1,nnobs),$ ;-- 0 no symb, >0 nb step betw symb +line, <0 only symb
symtype_obs:replicate(3,nnobs),$ ;--- obser. symbol 
symsize_obs:replicate(1.5,nnobs),$ ;--- obser. symbol size ; 1.5
symfill_obs:replicate(1,nnobs),$ ;--- obser. symbol filling  
;
;---- LEGEND
add_leg_mod:replicate(0,nnmod),$
add_leg_obs:replicate(0,nnobs),$
mod_leg:replicate('',nnmod),$
obs_leg:replicate('',nnobs),$ 
xpos_leg: 0.15+findgen(npos_leg)/npos_leg,$        ;-- X position of the legs in Normal/DATA coord.
ypos_leg: replicate(0.1,npos_leg),$        ;-- Y position of the legs         "
nleg_col: 1,$                           ;-- Nb of colone (xpos_leg) for the legs used
coord_leg:'normal',$                    ;-- Normal or Data coordonates      
incr_text_leg:1.,$                      ;-- Scaling for incremental Dy between text leg
csize_leg  : 1.,$                       ;-- character size
cthick_leg : 2 $                        ;-- character thick
}


STRUCT_ASSIGN, param_in, param, /nozero
tags     = tag_names(param)
ntags    = n_elements(tags)
tags_in  = tag_names(param_in)
ntags_in = n_elements(tags_in)
;-- go through the param_in fields list
for n = 0, ntags_in-1 do begin
                                ;-- get correspnding field in param
    ii = where(tags eq tags_in(n), cc)
    if(cc eq 1) then begin
                                ;-- compute n1 and n2 the sizes of the
                                ;   field inside param and param_in 
        res = execute('n1 = n_elements(param.'+tags_in(n)+')')
        res = execute('n2 = n_elements(param_in.'+tags_in(n)+')')
                                ;-- if the field is defined in param
                                ;   and not defined in param_in --
                                ;   assign the param field with that
                                ;   of param_in. Normally this case
                                ;   should not occur
        if((n1 gt 0) and (n2 eq 0)) then begin
            cmde = 'param.' + tags_in(n) + '= param_in.' + tags_in(n) + '(0)'
            res = execute(cmde)
        endif
    endif
endfor

;-------- Determine number model components
if((size(model))[0] ne 0) then begin ;-- model is not a scalar or undefined type
    ss = size(model.val)
    if(ss(0) eq 2) then nb_mod = ss(2) else nb_mod=1
    tagmodel = TAG_NAMES(model)

;-------- Define date/ndate for all models to plot
    if (nb_mod ne n_elements(model.date(0,*))) then begin
        model_date = replicate(model.date(0),n_elements(model.date(*,0)),nb_mod)
        for g=0,nb_mod-1 do model_date(*,g) = model.date(*,0)
    endif else model_date = model.date 
    
    ii = where(tagmodel eq 'NDATE',cc)
    if (cc ne 1) then begin
        model_ndate = replicate(n_elements(model.date(*,0)),nb_mod)
    endif else begin
        if(nb_mod gt n_elements(model.ndate(*))) then begin
            model_ndate = replicate(model.ndate(0),nb_mod)
        endif else model_ndate = model.ndate
    endelse 
endif else nb_mod = 0


;-------- Determine number observation types
if((size(obs))(0) ne 0) then begin
    ss2 = size (obs.val)
    if(ss2(0) eq 2) then nb_obs = ss2(2) else nb_obs=1
    tagobs = TAG_NAMES(obs)

;-------- Define date/ndate for all obs to plot
    if nb_obs ne n_elements(obs.date(0,*)) then begin
        obs_date = replicate(obs.date(0),n_elements(obs.date(*,0)),nb_obs)
        for g=0,nb_obs-1 do obs_date(*,g) = obs.date(*,0)
    endif else obs_date = obs.date 

    ii = where(tagobs eq 'NDATE',cc)
    if cc ne 1 then begin
        obs_ndate = replicate(n_elements(obs.date(*,0)),nb_obs)
    endif else begin
        if nb_obs gt n_elements(obs.ndate(*)) then begin
            obs_ndate = replicate(obs.ndate(0),nb_obs)
        endif else obs_ndate = obs.ndate 
    endelse 
endif else nb_obs = 0

;---------------------------------------------------------------------------
;		DEFINE ABSCISSE RANGE
;---------------------------------------------------------------------------

;--------- Define abscisse range for model.

x_min  = 1.e34 
x_max  = -1.e34
ok_mod = 0
for g=0,nb_mod-1 do begin
    if (param.add_mod(g) eq 1 and model_ndate(g) gt 0) then begin
        ii     = indgen(model_ndate(g))
        x_min  = min([model_date(ii,g),x_min])
        x_max  = max([model_date(ii,g),x_max])
        ok_mod = 1
    endif
endfor 

;--------- Extend abscisse range to account for obs

ok_obs = 0
for g=0,nb_obs-1 do begin
    if (param.add_obs(g) eq 1 and obs_ndate(g) gt 0) then begin
        ii     = indgen(obs_ndate(g))
        x_min  = min([obs_date(ii,g),x_min])
        x_max  = max([obs_date(ii,g),x_max])
        ok_obs = 1
    endif
endfor 

;-------- For manual definition of yrange.
if (param.xrange(0) ne param.xrange(1)) then begin
    x_min = param.xrange(0)
    x_max = param.xrange(1)
endif 

if ok_mod eq 0 and ok_obs eq 0 then message,'BIG probleme, no mod, no obs valid...'


;---------------------------------------------------------------------------
;		DEFINE ORDONNEE RANGE
;---------------------------------------------------------------------------

;--------- Model yrange..
y_min = 1.e34 
y_max = -1.e34

for g=0, nb_mod-1 do begin
    if (param.add_mod(g) eq 1 and model_ndate(g) gt 0) then begin
        iival = indgen(model_ndate(g))
        xx    = model_date(iival,g)
        foo   = where(xx ge x_min and xx le x_max, cc)
        if cc gt 0 then ii = iival(foo) else ii = iival 
        y_min = peyl_min([y_min,model.val(ii, g)], mask=param.max_number)
        y_max = peyl_max([y_max,model.val(ii, g)], mask=param.max_number)    
    endif
    if (param.add_mod_err(g) eq 1 and model_ndate(g) gt 0) then begin
        ii = where (model.std_dev(*, g) ge param.max_number, cc)
        if cc gt 0 then model.std_dev(ii, g)=0.

                                ;--- Pour la non prise en compte des
                                ;    valeurs pour le calcul des 
                                ;    ymin et ymax, si la barre
                                ;    d'erreur est superieure a 
                                ;    param.seuil (en ppmv) 
        if (n_elements(param.seuil) ne 0) then begin
            ii = where (model.std_dev(0:model_ndate(g)-1,g) lt param.seuil, cc)
        endif else begin
            ii = indgen(model_ndate(g))
            cc = model_ndate(g)
        endelse
        if cc gt 0 then begin
            y_min = peyl_min([y_min,model.val(ii, g)-model.std_dev(ii, g)], mask=param.max_number)
            y_max = peyl_max([y_max,model.val(ii, g)+model.std_dev(ii, g)], mask=param.max_number)
        endif
    endif
endfor

;--------- Obs yrange..

for g=0, nb_obs-1 do begin
    if (param.add_obs(g) eq 1 and obs_ndate(g) gt 0) then begin
        iival = indgen(obs_ndate(g))
        xx    = obs_date(iival,g)
        foo   = where(xx ge x_min and xx le x_max, cc)
        if cc gt 0 then ii = iival(foo) else ii = iival 
        y_min = peyl_min([y_min,obs.val(ii, g)], mask=param.max_number)
        y_max = peyl_max([y_max,obs.val(ii, g)], mask=param.max_number)        
    endif
    if (param.add_obs_err(g) eq 1 and obs_ndate(g) gt 0) then begin
        ii = where (obs.std_dev(*, g) ge param.max_number, cc)
        if cc gt 0 then obs.std_dev(ii, g)=0.

                                ;--- Pour la non prise en compte des
                                ;    valeurs pour le calcul des 
                                ;    ymin et ymax, si la barre
                                ;    d'erreur est superieure a 
                                ;    param.seuil (en ppmv) 
        if (n_elements(param.seuil) ne 0) then begin
            ii = where (obs.std_dev(0:obs_ndate(g)-1,g) lt param.seuil, cc)
        endif else begin
            ii = indgen(obs_ndate(g))
            cc = obs_ndate(g)
        endelse
        if cc gt 0 then begin
            y_min = peyl_min([y_min,obs.val(ii, g)-obs.std_dev(ii, g)], mask=param.max_number)
            y_max = peyl_max([y_max,obs.val(ii, g)+obs.std_dev(ii, g)], mask=param.max_number)
        endif
    endif
endfor

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
xtitle = param.xtitle

;-------- Ytitle
ytitle = param.ytitle

;-------- Title
title  = param.title

;-------- Some initialisations
tarr = ''
larr = -1
carr = -1
sarr = -1
farr = -1

;---------------------------------------------------------------------------
;		Gestion des positions
;---------------------------------------------------------------------------

;---- Keyword position passe ou non!!
special_position = 0
if total(param.position) ne param.position(0)*4 then begin
    position = param.position
    special_position = 1
endif else begin
    special_position = 0
    position = [0.,0.,0.,0.]
endelse

;------ Erreur plottees a part sur le cote..
foo = where(param.add_mod_err(*) eq 2, cc_side_err)
if cc_side_err gt 0 then begin
    if !p.multi(1) ne 2 then print,'Probleme de nb de plot en y: !p.muli(1) DOIT ETRE 2!! '

                                ;--- On doit creer le tableau position
                                ;    (non fournit) 
    if total(position) eq position(0)*4 then begin
        yplot = !p.multi(2)
        xmarge=0.08
        ymarge=0.01
        ddy = 0.12
        xx = (1.-2.*xmarge)       ;--- dimension en x de chaque plot
        yy = (1.-2.*ymarge-(yplot-1)*ddy)/yplot ;--- dimension en y de chaque plot
        pp = !p.multi(0) 
        if pp gt 0 then pp = ((!p.multi(1)*!p.multi(2))-pp)/!p.multi(1)
        position = [xmarge,1.-ymarge-(pp+1)*yy-pp*ddy,xmarge+xx,1.-ymarge-pp*yy-pp*ddy]
    endif 

    xx1 = (position(2)-position(0)) * 7./8.
    xx2 = (position(2)-position(0)) - xx1
    xp  = position(0) + indgen(2)*xx1           ;--- position en x de chaque plot
    xp2 = xp(1) + indgen(2)*xx2                 ;--- position en x de chaque plot
    position     = [xp(0),position(1),xp(1),position(3)]
    position_bis = [xp2(0),position(1),xp2(1),position(3)]
    special_position = 1
endif


;---------------------------------------------------------------------------
;		 PLOT MODEL CURVES
;---------------------------------------------------------------------------

;-------- Plot first the Frame..

 if special_position then begin

     if (position(2) le position(0)) or (position(3) le position(1)) then begin
         print,'Error dans vecteur position : ',position
         stop
     endif

     PLOT,   [0,1],[0,1], $
       /YNOZERO, $
       /NODATA, $
       NOERASE=param.noerase, $
       min_value=-abs(param.max_number)+1, $ 
       max_value=abs(param.max_number)-1, $
       xtitle=xtitle, $
       ytitle=ytitle,$
       YRANGE=[y_min,y_max], $
       XRANGE=[x_min,x_max], $
       color=param.color_text(0),$
       ystyle=4+param.ystyle, $
       yticks=param.yticks,$
       yminor=param.yminor,$
       thick=param.thick_mod(0), $
       xthick=param.xthick, $
       ythick=param.ythick, $
       charsize=param.csize,$
       charthick=param.cthick, $
       xcharsize=param.xcharsize, $
       ycharsize=param.ycharsize, $
       position=position,$
       XSTYLE=4+param.xstyle

 endif else begin

     PLOT,   [0,1],[0,1], $
       /YNOZERO, $
       /NODATA, $
       NOERASE=param.noerase, $
       min_value=-abs(param.max_number)+1, $ 
       max_value=abs(param.max_number)-1, $
       xtitle=xtitle, $
       ytitle=ytitle,$
       YRANGE=[y_min,y_max], $
       XRANGE=[x_min,x_max], $
       color=param.color_text(0),$
       ystyle=4+param.ystyle, $
       yticks=param.yticks,$
       yminor=param.yminor,$
       thick=param.thick_mod(0), $
       xthick=param.xthick, $
       ythick=param.ythick, $
       charsiz=param.csize,$
       charthick=param.cthick, $
       xcharsize=param.xcharsize, $
       ycharsize=param.ycharsize, $
       XSTYLE=4+param.xstyle

 endelse

;-------- PLOT grise
if param.add_mod_grise gt 0 then begin

                                ;--- On a fournit les valeurs min et
                                ;    max du grise
    if param.add_mod_grise eq 1 then begin
        nn = n_elements(model.grise_date)
        if nn le 0 then message,'probleme avec Grise date !!'
        if nn ne n_elements(model.grise_max) then message,'NB Grise max different NB grise date'
        if nn ne n_elements(model.grise_min) then message,'NB Grise min different NB grise date'
        grise_date = model.grise_date
        grise_min  = model.grise_min
        grise_max  = model.grise_max

                                ;--- On recalcule les valeurs pour le
                                ;    grise a partir des donnees model..
    endif else begin
        ii = where(param.mod_grise eq 1,nii)
        if nii eq 0 then message,'No model selected for the grise...'
        nn = max(model_ndate(ii),inn)
        if total(model_ndate(ii)) ne (nn*nii) then $
          print,'Warning: Not all same date for model grise...'
        
        grise_date = dblarr(nn)
        grise_min  = replicate(param.max_number,nn)
        grise_max  = replicate(-param.max_number,nn)
                                ;- cas plusieurs model : enveloppe
        if (param.add_mod_grise eq 2 or param.add_mod_grise eq 21) then begin
            dd = (model_date(1,ii(inn))-model_date(0,ii(inn)))/10.
            for n=0,nn-1 do begin
                grise_date(n) = model_date(n,ii(inn))
                for l=0,nii-1 do begin
                    jj = where(abs(model_date(0:model_ndate(ii(l))-1,ii(l))-grise_date(n)) le dd,cc)
                    if cc ne 1 then continue
                    grise_min(n) = min([grise_min(n),model.val(jj,ii(l))])
                    grise_max(n) = max([grise_max(n),model.val(jj,ii(l))])
                endfor
            endfor
                                ;- cas 1 seul model +/- std_dev
        endif else begin
            for n=0,nn-1 do begin
                grise_date(n) = model_date(n,ii(0))
                grise_min(n) = model.val(n,ii(0)) - model.std_dev(n,ii(0))
                grise_max(n) = model.val(n,ii(0)) + model.std_dev(n,ii(0))
            endfor
        endelse
;        print,grise_min,grise_max
;        stop
    endelse

                                ;--- PLOT du grise lui meme...
    xxx = fltarr(2*nn+1)
    yyy = fltarr(2*nn+1)
    for kk=0,nn-1 do begin
        xxx(kk) = grise_date(kk)
        yyy(kk) = grise_min(kk)
    endfor
    for kk=nn,2*nn-1 do begin
        xxx(kk) = grise_date(2*nn-1-kk) 
        yyy(kk) = grise_max(2*nn-1-kk)
    endfor
    xxx(2*nn) = xxx(0)
    yyy(2*nn) = yyy(0)
    if param.color_table ge 0 then zp_loadct,param.color_table_grise
    polyfill,xxx,yyy,color=param.color_grise,noclip=0
    if param.color_table ge 0 then zp_loadct,param.color_table
endif         


;-------- Overplot all components

for gg=0,nb_mod-1 do begin
    g = param.mod_order(gg)

                                ;--- Special pour mettre en grise les
                                ;    courbes de la zone grise
    if (param.add_mod_grise eq 21 and param.mod_grise(g) eq 1 and $
        param.color_table ge 0) then begin
        zp_loadct,param.color_table_grise
        print,'coucou',param.color_table_grise,param.color_mod(g)
    endif

    if(param.add_mod(g) eq 1 and model_ndate(g) gt 0) then begin
        iival = indgen(model_ndate(g))

                                ;--- Plot the curve at least
        if param.symbol_mod(g) ge 0 then begin
            OPLOT, [model_date(iival,g)], [model.val(iival,g)], $
              thick     = param.thick_mod(g), $
              linestyle = param.lines_mod(g), $
              color     = param.color_mod(g), $
              min_value = -abs(param.max_number)*1.01, $ 
              max_value = abs(param.max_number)*0.99
        endif

                                ;----- Add symbols.
        if param.symbol_mod(g) ne 0 then begin
            ccg_symbol, sym=param.symtype_mod(g), $
              fill=param.symfill_mod(g)
            nnn = abs(param.symbol_mod(g)) < model_ndate(g)
            ii_sym = indgen(model_ndate(g)/nnn) * nnn


            OPLOT, [model_date(iival(ii_sym),g)],[model.val(iival(ii_sym),g)], $
              color   = param.color_mod(g), $
              symsize = param.symsize_mod(g), $
              thick   = param.thick_mod(g),$
              psym    = 8
        endif

                                ;-----  Add error bar
        if (param.add_mod_err(g) eq 1 or param.add_mod_err(g) eq 3) then begin 

            ii = where(model.std_dev(*,g) ge param.max_number or $
                       model.std_dev(*,g) ge param.seuil, cc)
            if cc gt 0 then model.std_dev(ii,g)=0.
        
            ii_err = indgen(model_ndate(g))
            if param.symbol_mod(g) ne 0 then $
              ii_err = indgen(model_ndate(g)/abs(param.symbol_mod(g))) * $
              abs(param.symbol_mod(g))
        
            peyl_errplot, [model_date(ii_err,g)], $
              model.val(ii_err,g)+model.std_dev(ii_err,g), $
              model.val(ii_err,g)-model.std_dev(ii_err,g), $
              color = param.color_mod(g),$
              thick = param.thick_mod_err(g)
        endif 
    endif 

                                ;---- Fill legend
    if param.add_leg_mod(g) then begin
        if param.symbol_mod(g) ne 0 then begin
            ss = param.symtype_mod(g)
            if param.symbol_mod(g) gt 0 then ss = -ss
        endif else ss = 0
        tarr = [tarr,param.mod_leg(g)]
        larr = [larr,param.lines_mod(g)]
        carr = [carr,param.color_mod(g)]
        sarr = [sarr,ss]
        farr = [farr,param.symfill_mod(g)]
    endif

                                ;--- Special pour mettre en grise les
                                ;    courbes de la zone grise
    if (param.add_mod_grise eq 21 and param.mod_grise(g) eq 1 and $
        param.color_table ge 0) then zp_loadct,param.color_table
endfor 


;---------------------------------------------------------------------------
;		 PLOT OBSERVATIONS
;---------------------------------------------------------------------------

for g=0,nb_obs-1 do begin

                                ;--- Plot obs (curve if param.symbol_obs positive)
    if (param.add_obs(g) eq 1 and obs_ndate(g) gt 0) then begin
        iival = indgen(obs_ndate(g))

                                ;--- Plot the curve at least
        if param.symbol_obs(g) ge 0 then begin 
            OPLOT, [obs_date(iival,g)], [obs.val(iival,g)],$
              color     = param.color_obs(g),$
              linestyle = param.lines_obs(g),$
              thick     = param.thick_obs(g),$
              min_value = -abs(param.max_number)*1.01,$ 
              max_value = abs(param.max_number)*0.99,$
              psym      = 0
        endif
        
                                ;---- Add symbols 
        if param.symbol_obs(g) ne 0 then begin 
            nnn = abs(param.symbol_obs(g)) < obs_ndate(g)
            ii_sym = indgen(obs_ndate(g)/nnn) * nnn
            
            ccg_symbol, sym=param.symtype_obs(g), $
              fill=param.symfill_obs(g)
            oplot,[obs_date(ii_sym,g)],[obs.val(ii_sym,g)], $
              symsize = param.symsize_obs(g), $
              color   = param.color_obs(g),$
              thick   = param.thick_obs(g),$
              psym    = 8        
        endif 
        
                                ;-----  Add error bar
        if param.add_obs_err(g) eq 1 then begin 
            
            ii = where (obs.std_dev(*,g) ge param.max_number or $
                        obs.std_dev(*,g) ge param.seuil, cc)
            if cc gt 0 then obs.std_dev(ii,g)=0.
            
            ii_err = indgen(obs_ndate(g))
            if param.symbol_obs(g) ne 0 then begin
                nnn = abs(param.symbol_obs(g)) < obs_ndate(g)
                ii_err = indgen(obs_ndate(g)/nnn) * nnn
            endif
            
            peyl_errplot, [obs_date(ii_err,g)], $
              obs.val(ii_err,g)+obs.std_dev(ii_err,g), $
              obs.val(ii_err,g)-obs.std_dev(ii_err,g), $
              color = param.color_obs(g),$
              thick = param.thick_obs_err(g)
        endif 

    endif 
        
                                ;----- Fill array for legend
    if param.add_leg_obs(g) eq 1 then begin
        if param.symbol_obs(g) ne 0 then begin
            ss = param.symtype_obs(g)
            if param.symbol_obs(g) gt 0 then ss = -ss
        endif else ss = 0
        tarr = [tarr,param.obs_leg(g)]
        larr = [larr,param.lines_obs(g)]
        carr = [carr,param.color_obs(g)]
        sarr = [sarr,ss]
        farr = [farr,param.symfill_obs(g)]
    endif 
        
endfor


;---------------------------------------------------------------------------
;		 PLOT TITLES, LEGEND, AXIS
;---------------------------------------------------------------------------

;-------- Plot main title
if strlowcase(param.coord_title) eq 'normal' then begin
    new = convert_coord(param.xpos_title,param.ypos_title,/NORMAL,/TO_DATA)
endif else if strlowcase(param.coord_title) eq 'data' then begin
    new = [param.xpos_title,param.ypos_title]
endif else begin
    new = [(!x.crange(1)+!x.crange(0))/2.,!y.crange(1)+(!y.crange(1)-!y.crange(0))/20.]
endelse

xyouts,new(0),new(1), $
  title,$
  ali       = 0.5,$
  col       = param.color_text(0),$
  charthick = param.cthick_title,$
  charsize  = param.csize_title,$
  /DATA


;-------- Plot x bottom axis

xticks = param.xticks
                                ;--- Si xtickv contient les valeurs a
                                ;    ploter (n valeurs differentes
                                ;    avec n >1) on les prend
if total(param.xtickv) ne (n_elements(param.xtickv)*param.xtickv(0)) then begin
    xtickv = param.xtickv
    xticks = n_elements(xtickv)+1
endif 

if (param.xtickname(0) ne '') then begin
    if n_elements(param.xtickname) lt xticks then $
      print,'! Warning : nb de xtickname < nb de ticks...'
    xtickname = param.xtickname
endif

;--- Define X-axis label from IDL if requested..
IF (param.xlabel EQ 1 or param.xtime_format eq 1) THEN BEGIN
    xtickname = replicate(' ',50)
    foo = where(param.xtickformat ne '',cc)
    if cc eq 0 then message,'probleme xtickformat non definit!!'
    dummy = LABEL_DATE(DATE_FORMAT=param.xtickformat(foo))
    xtickunits = replicate('Time',cc)
    XTICKFORMAT = 'LABEL_DATE'
ENDIF

axis,xaxis=0,xticks=xticks,xminor=param.xminor,xtickv=xtickv,$
  xtickname = xtickname,$
  xtitle    = xtitle,$
  charthick = param.cthick ,$
  col       = param.color_text(0),$
  charsize  = param.csize,$
  xcharsize = param.xcharsize,$
  xthick    = param.xthick,$
  xrange    = param.xlimit,$
  xstyle    = param.xstyle,$
;  xtickinterval=xtickinterval,$
  xtickformat=xtickformat,$
  xtickunits=xtickunits


;-------- Plot x top axis
axis,xaxis=1,xticks=xticks,xminor=param.xminor,xtickv=xtickv,$
  charsiz   = param.csize,$
  col       = param.color_text(0),$
  xtickname = replicate(' ',30),$
  charthick = param.cthick,$
  xthick    = param.xthick,$
  xrange    = param.xlimit,$
  xstyle    = param.xstyle


;-------- Legend for components..

if n_elements(tarr) gt 1 then begin
    tarr = tarr(1:*)
    larr = larr(1:*)
    carr = carr(1:*)
    sarr = sarr(1:*)
    farr = farr(1:*)
    nleg = n_elements(tarr)
    nnn  = ceil(float(nleg)/float(param.nleg_col))
    kk = 0
    for g=0,param.nleg_col-1 do begin
                                ;--- Determination du nbr de leg par
                                ;    groupe de legende
	if kk eq nleg then break
        nb_leg_loc = nnn < (nleg - (kk))
        tarr_loc = tarr(kk:kk+nb_leg_loc-1)
        larr_loc = larr(kk:kk+nb_leg_loc-1)
        carr_loc = carr(kk:kk+nb_leg_loc-1)
        sarr_loc = sarr(kk:kk+nb_leg_loc-1)
        farr_loc = farr(kk:kk+nb_leg_loc-1)
        kk = kk + nb_leg_loc

        if strlowcase(param.coord_leg) eq 'data' then begin
            x = !x.crange(0)+(!x.crange(1)-!x.crange(0))*(param.xpos_leg(g))
            y = !y.crange(0)+(!y.crange(1)-!y.crange(0))*(param.ypos_leg(g))
            ddd = 1
        endif else begin
            x = param.xpos_leg(g)
            y = param.ypos_leg(g)
            ddd = 0
        endelse
        peyl_llegend,x=x,y=y,$
          tarr = tarr_loc,$
          larr = larr_loc,$
          carr = carr_loc,$
          sarr = sarr_loc,$
          farr = farr_loc,$
          charthick = param.cthick_leg,$
          charsize  = param.csize_leg,$
          thick     = param.cthick_leg,$
          incr_text = param.incr_text_leg,$
          data=ddd
    endfor 
endif

                                ;---- Footnote at the bottom page
if (param.footnote ne '' and !p.multi(0) eq 0) then begin
    peyl_footnote,param.footnote
    print,'Foot note : ',param.footnote
endif


;---------------------------------------------------------------------------
;	SPECIAL SIDE PLOT OF ERROR BAR....
;---------------------------------------------------------------------------

                                ;--- Graphe sur le cote pour les
                                ;    erreurs.. 
if cc_side_err gt 0 then begin

                                ;--- On centre les erreurs...
    y_min2 = y_min - (y_min+y_max)/2.
    y_max2 = y_max - (y_min+y_max)/2.

      PLOT,[0,1],[0,1], $
        /YNOZERO, $
        /NODATA, $
        position=position_bis,$
        max_value=param.max_number, $
        xtitle='Error', $
        YRANGE=[y_min2,y_max2], $
        xrange=[0,1],$
        color=param.color_text(0),$
        ystyle=param.ystyle, $
        thick=param.thick_mod(0), $
        ythick=param.cthick, $
        charsiz=param.csize,$
        charthick=param.cthick, $
        xtickname = replicate(' ',30),$
        ytickname = replicate(' ',30),$
        XSTYLE=param.xstyle        
    
     for g=0,nb_mod-1 do begin

         if (param.add_mod(g) eq 0 or model_ndate(g) eq 0 or $
             param.add_mod_err(g) lt 2) then continue
        
         ii = where (model.std_dev(*,g) ge param.max_number or $
                     model.std_dev(*,g) ge param.seuil, cc)
         if cc gt 0 then model.std_dev(ii,g)=0.

         ii_err = indgen(model_ndate(g))
         XX = (g+0.5) / nb_mod  
         width = (1./nb_mod) /4.
         YY1 = mean(model.std_dev(ii_err,g))
         YY2 = -YY1
         peyl_errplot, XX, YY1, YY2, $
           thick=param.thick_mod_err(g),Width =width,color=param.color_mod(g)
    endfor
endif


end
;-





