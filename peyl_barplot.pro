;+
;========================================================================
; NAME:
;       PEYL_XXXX
;
; PURPOSE:
;       
;
; KEYWORDS:
;
;
; CALLING SEQUENCE:
;       
;
; INPUTS:
;     
;
; OPTIONAL INPUT PARAMETERS:
; 
;
; OUTPUTS:
;
;       
; RESTRICTIONS:
; 
;
;========================================================================
PRO peyl_barplot,values,$
                 baselines=baselines,$
                 colors=colors,$
                 barnames=barnamesIn,$
                 title=title,$
                 xtitle=xtitle,$
                 ytitle=ytitle,$
                 baserange=baserange,$
                 barspace=barspaceIn,$
                 barwidth=barwidth,$
                 baroffset=baroffset,$
                 outline=outline,$
                 overplot=overplot,$
                 background=background,$
                 rotate=rotate,$
                 range=rangeIn,$
                 ystyle=ystyle,$
                 pos=pos
;

nbars=n_elements(values)                ; Determine number of bars
; Baselines (bars extend from baselines through values); default=0
if not(keyword_set(baselines)) then baselines=intarr(nbars)
; Default colors spaced evenly in current color table
if not(keyword_set(colors)) then $
   colors=fix(((!d.n_colors < 256)/float(nbars))*(Lindgen(nbars)+0.5))
; Labels for the individual bars; none by default
barnames = (N_Elements(barnamesIn) gt 0) ? barnamesIn : strarr(nbars)+' '
; Main title
if not(keyword_set(title)) then title=''
; Centered title under X-axis
if not(keyword_set(xtitle)) then xtitle=''
; Title for Y-axis
if not(keyword_set(ytitle)) then ytitle=''
; Fraction (0-1) of full X range to use
if not(keyword_set(baserange)) then baserange=1.0
; Space betw. bars, taken from nominal bar widths; default is none
barspace = (N_Elements(barspaceIn) gt 0) ? Float(barspaceIn) : 0.2
; Bar width scaling factor, relative to nominal
if not(keyword_set(barwidth)) then barwidth=1.0 - barspace - barspace / nbars
; Initial X offset, in scaled bar widths; default is none
if not(keyword_set(baroffset)) then baroffset=barspace/barwidth
; Outline of bars; default is none
outline = keyword_set(outline)
; Overplot (do not erase the existing display); default is to create new plot
overplot = keyword_set(overplot)
; Background color index; defaults to 0 (usually black) if not specified
if not(keyword_set(background)) then background=0
; Rotate (make horizontal bars); default is vertical bars
rotate = keyword_set(rotate)

compute_range = 1
if n_elements(rangeIn) gt 1 then begin
    compute_range = 0
    if rangeIn(0) eq rangeIn(1) then compute_range = 1
endif 
if compute_range then begin
    mnB = MIN(baselines, MAX=mxB, /NAN)
    mnV = MIN(values, MAX=mxV, /NAN)
    range=[mnB < mnV, $         ;Minimum of bases & values
           mxB > mxV]           ;Maximum of bases & values
endif else range=rangeIn

if (rotate) then begin                             ;Horizontal bars
   if (!x.range[0] eq 0) and (!x.range[1] eq 0) $  ;Determine range for X-axis
      then xrange=range $
      else xrange=!x.range                         ;Or, use range specified
   if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Plot will calculate
      then $                                       ; defaults for X, but not
        yrange = [0, n_elements(values)] $         ; for Ys, so fill in here.
      else $
        yrange=!y.range                            ;Axis perpend. to bars
   yticks=1                                        ;Suppress ticks in plot
   ytickname=strarr(2)+' '
   xticks=0
   xtickname=strarr(1)+''
endif else begin                                   ;Vertical bars
   if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Determine range for Y-axis
      then yrange=range $
      else yrange=!y.range                         ;Or, use range specified
   xrange=!x.range                                 ;Axis perpend. to bars
   xticks=1                                        ;Suppress ticks in plot
   xtickname=strarr(2)+' '
   yticks=0
   ytickname=strarr(1)+''
endelse
;if (overplot eq 0) then $                          ;Create new plot,
;no data

if n_elements(pos) eq 4 then begin
    plot,[values],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      noerase=overplot,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=1,ystyle=ystyle,/data,background=background,$
      thick=3.,charsize=1.7,xthick=3.,ythick=3.,charthick=2.,pos=pos
endif else begin
    plot,[values],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      noerase=overplot,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=1,ystyle=ystyle,/data,background=background,$
      thick=3.,charsize=1.7,xthick=3.,ythick=3.,charthick=2.
endelse

if (rotate) then begin                             ;Horizontal bars
   base_win=!y.window                              ;Window range in Y
   scal_fact=!x.s                                  ;Scaling factors
   tick_scal_fact=!y.s                             ;Tick scaling factors
endif else begin                                   ;Vertical bars
   base_win=!x.window                              ;Window range in X
   scal_fact=!y.s                                  ;Scaling factors
   tick_scal_fact=!x.s                             ;Tick scaling factors
endelse
winrange=baserange*(base_win[1]-base_win[0])       ;Normal. window range
barsize=barwidth*winrange/nbars                    ;Normal. bar width
winoffset=base_win[0]+(baroffset*barsize)          ;Normal. first offset
bases=scal_fact[0]+(scal_fact[1]*baselines)        ;Baselines, in normal coor.
normal=scal_fact[0]+(scal_fact[1]*values)          ;Values, in normal coor.
barstart=Lindgen(nbars)*(barsize+barspace*(winrange/nbars)) ;Coor. at left edges
tickv=winoffset+barstart+(0.5*barsize)             ;Tick coor. (centered)

for i=0,nbars-1 do begin                           ;Draw the bars
   width=winoffset+[barstart[i],barstart[i], $     ;Compute bar width
     (barstart[i]+barsize),(barstart[i]+barsize)]
   length=[bases[i],normal[i],normal[i],bases[i]]  ;Compute bar length
   if (rotate) then begin                          ;Horizontal bars
      x=length                                     ;X-axis is "length" axis
      y=width                                      ;Y-axis is "width" axis
   endif else begin                                ;Vertical bars
      x=width                                      ;X-axis is "width" axis
      y=length                                     ;Y-axis is "length" axis
   endelse
   polyfill,x,y,color=colors[i],/normal            ;Polyfill with color
   if (outline) then plots,x,y,/normal             ;Outline using !p.color
endfor

tickv=(tickv-tick_scal_fact[0])/tick_scal_fact[1]  ;Locations of the ticks
if (rotate) then begin                             ;Label the bars (Y-axis)
  axis,yaxis=0,ystyle=1,yticks=(nbars-1),ytickv=tickv,ytickname=barnames, $
    yticklen=0.0
endif else begin                                           ;Label the bars (X-axis)
  axis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickv=tickv,xtickname=barnames, $
    xticklen=0.0
endelse

END
;-
