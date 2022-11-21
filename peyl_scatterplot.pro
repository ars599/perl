;+
PRO PEYL_scatterplot,X,Y,$
                     title=title,xtitle=xtitle,ytitle=ytitle, $
                     xrange=xrange,yrange=yrange,xticks=xticks, $
                     xstyle=xstyle,ystyle=ystyle,$
                     xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
                     background=background,charsize=charsize,charthick=charthick,lcharsize=lcharsize,$
                     thick=thick,pos=pos,add_regress=add_regress,show_coef=show_coef,$
                     symbol=sym,fill=fill,symsize=symsize,symthick=symthick,color=color,$
                     regress_thick=regress_thick,regress_line=regress_line,regress_col=regress_col,$
                     px=pxIn,py=pyIn,add_diag=add_diagIn,noerase=noerase,overplot=overplot

if n_elements(pxIn) gt 0 then px = pxIn
if n_elements(pyIn) gt 0 then py = pyIn
if n_elements(add_diagIn) eq 0 then add_diag = 0 else add_diag = add_diagIn
if n_elements(overplot) eq 0 then overplot=0
if n_elements(symthick) eq 0 then symthick=1.5
if n_elements(ADD_REGRESS) eq 0 then ADD_REGRESS=0

;if overplot eq 0 then begin
if n_elements(pos) eq 4 then begin
    plot,[0,1],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      noerase=noerase,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=xstyle,ystyle=ystyle,/data,background=background,$
      charsize=charsize,charthick=charthick,$
      xthick=thick,ythick=thick,$
      pos=pos,xminor=1,yminor=1,ticklen=-0.02
endif else begin
    plot,[0,1],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      noerase=noerase,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=ystyle,ystyle=ystyle,/data,background=background,$
      charsize=charsize,charthick=charthick,$
      xthick=thick,ythick=thick,xminor=1,yminor=1,ticklen=-0.02
endelse
;endif

if add_diag then oplot,!x.crange,!y.crange,linestyle=1,col=0,thick=regress_thick

ccg_symbol, sym=sym,fill=fill, thick= symthick
if n_elements(color) ge n_elements(X) then begin
    for n=0,n_elements(X)-1 do begin
        ;ccg_symbol, sym=sym[n],fill=fill, thick= symthick
        oplot,[X(n)],[Y(n)],$
          color   = color(n), $
          symsize = symsize, $
          thick   = 1.5,$
          psym = 8
    endfor
endif else $
  oplot,[X],[Y],$
  color   = color(0), $
  symsize = symsize, $
  thick   = 1.5,$
  psym = 8
 
if add_regress then begin
    coef=regress(X,Y,mcor=corel,const=cst,/double)
    xmin = !x.crange(0) & xmax = !x.crange(1) 
    ymin = !y.crange(0) & ymax = !y.crange(1) 
;    if n_elements(xrange) eq 2 then begin
;        xmin = xrange(0) & xmax = xrange(1) 
;    endif
;    if n_elements(yrange) eq 2 then begin
;        ymin = yrange(0) & ymax = yrange(1) 
;    endif
    y0=coef[0]*xmin +cst
    y1=coef[0]*xmax +cst

    oplot,[xmin,xmax],[y0,y1],color=regress_col,thick=regress_thick,linestyle=regress_line
    if show_coef then begin
        ALIGNMENT= 0.0
        if n_elements(format) eq 0 then format='(F5.2)'
        if n_elements(px) eq 0 then px = xmin + (xmax-xmin)*[0.05,0.25]
        if n_elements(py) eq 0 then py = ymax - (ymax-ymin)*[0.08,0.16,0.24]
        xyouts,px(0),py(0),'slope :',color=regress_col,ALIGNMENT=ALIGNMENT,charthick=2.,charsize=lcharsize,/data
        xyouts,px(1),py(0),string(coef[0],format=format),color=regress_col,ALIGNMENT=ALIGNMENT,charthick=2.5,charsize=lcharsize,/data
        xyouts,px(0),py(1),'correl :',color=regress_col,ALIGNMENT=ALIGNMENT,charthick=2.,charsize=lcharsize,/data
        xyouts,px(1),py(1),string(corel,format=format),color=regress_col,ALIGNMENT=ALIGNMENT,charthick=2.5,charsize=lcharsize,/data
        xyouts,px(0),py(2),'interc :',color=regress_col,ALIGNMENT=ALIGNMENT,charthick=2.,charsize=lcharsize,/data
        xyouts,px(1),py(2),string(cst,format=format),color=regress_col,ALIGNMENT=ALIGNMENT,charthick=2.5,charsize=lcharsize,/data
    endif
endif

END
;-
