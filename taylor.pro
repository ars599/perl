@arcs
;+
; NAME:
;	TAYLOR
;
; PURPOSE:
;	Makes Taylor Plot
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	TAYLOR, x1, y1, x2, y2, undef=undef, position=position $
;	         , sigma=sigma, half=half, code=code $
;                , tickinterval=tickinterval $
;                , thick=thick, charthick=charthick, charsize=charsize $
;                , csize=csize, minor=minor, symbol=symbol, symsize=symsize
;
; INPUTS:
;	x1:  sigma model or normalised sigma model
;	y1:  cos1(R) between data and model
;       x2:  higher value of (normalised) sigma model
;       y2:  higher value of cos1(R)
;
; OPTIONAL INPUT PARAMETERS:
;       undef:     Disregard these values.
;                  (default=-999.)
;       position:  4 element array with ploting position
;                  [xl,yl,xr,yr]
;       sigma:     sigma data if not normalised
;       half:      Make polar plot a half circle
;       code:      Station names if they should be printed instead of
;                  symbol
;       tickinterval: Interval between major ticks on x and y axis
;       thick:     Linethickness
;       charthick: Character thickness
;       charsize:  Character size
;       minor:     # of minor ticks between major ticks on x and y axes
;       csize:  Size of the station names plotted if station names
;                  are given in keyword code
;       symbol:    Symbol number of symbol for plotting symbols
;       symsize:   Size of plot symbols
;
; OUTPUTS:
;       Taylor Plot.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;	None.	
;
; MODIFICATION HISTORY:
;	Written, MC, April 2002.
;
PRO TAYLOR, x1, y1, x2, y2, undef=undef, position=position $
               , sigma=sigma, half=half, code=code $
               , tickinterval=tickinterval $
               , thick=thick, charthick=charthick, charsize=charsize $
               , csize=csize, minor=minor $
               , symbol=symbol, symsize=symsize, color = color $
               , title = title,otitle=otitle $
               , xtit = xtit, ytit = ytit

;
q=!QUIET
!QUIET=1
COMPILE_OPT idl2
ON_ERROR, 2
;
;-------------------------------------- Constants
;
default=-999.D
;-------------------------------------- check input parameters


;
if n_params() lt 2 then begin
    print,' '
    print,'ERROR TAYLOR: SigmaModel and cos-1(R) must be given.',string(7B)
    print,'  ex: taylor, sigmhat, acosR, undef=undef'
    !QUIET=q
    return
endif
if n_params() eq 3 then begin
    print,' '
    print,'ERROR TAYLOR: 3 input variables given. # must be 2 or 4.',string(7B)
    print,'  ex: taylor, sigmhat, acosR, undef=undef'
    !QUIET=q
    return
endif
if n_params() gt 4 then begin
    print,' '
    print,'ERROR TAYLOR: For error plot, low and high values must be given.',string(7B)
    print,'  ex: taylor, sigmhat1, acosR1, sigmhat2, acosR2, undef=undef'
    !QUIET=q
    return
endif
if n_params() eq 2 then erro=0
if n_params() eq 4 then erro=1
undef = n_elements(undef) ne 0 ? undef : default
pos = n_elements(position) ne 0 ? 1 : 0
if pos then begin
    npos=n_elements(position)
    if npos ne 4 then begin
        print,'WARNING TAYLOR: Position is wrong.',string(7B)
        print,'                   Suppress optional input.'
        print,'ex: taylor, sigmhat, acosR, undef=undef, position=[xl,yl,xr,yr]'
        pos=0
    endif
endif   
norm = n_elements(sigma) ne 0 ? 0 : 1
half = keyword_set(half) ? 1 : 0
names = n_elements(code) ne 0 ? 1 : 0
thick = n_elements(thick) ne 0 ? thick : 2.
charthick = n_elements(charthick) ne 0 ? charthick : 2.
charsize = n_elements(charsize) ne 0 ? charsize : 2.
minor = n_elements(minor) ne 0 ? fix(minor) : 2
codesize = n_elements(csize) ne 0 ? csize : 0.4
symbol = n_elements(symbol) ne 0 ? fix(symbol) : 1
symsize = n_elements(symsize) ne 0 ? symsize : 0.5
title   = n_elements(title) ne 0 ? title : 'XXX'
otitle  = n_elements(otitle) ne 0 ? otitle : 'XXX'
color = n_elements(color) ne 0 ? color : replicate(1,n_elements(x1))
if erro and names then begin
    print,' '
    print,'WARNING TAYLOR: Code and Err mutually exclusive. Take Code, no Error.',string(7B)
    erro=0
    names=1
endif
if (norm eq 0) and (names eq 0) then begin
    print,' '
    print,'ERROR TAYLOR: If Non normalised data is given then names have to be given.',string(7B)
    print,'ex: taylor, sigmhat, acosR, undef=undef, sigma=sigma, code=code'
    !QUIET=q
    return
endif
if erro and (norm eq 0) and names then begin
    print,' '
    print,'ERROR TAYLOR: Error, Not normalised data and Station names:'
    print,'                 this feature is not yet supported.',string(7B)
    print,'ex: taylor, sigmhat, acosR, undef=undef, sigma=sigma, code=code'
    !QUIET=q
    return
endif

;-------------------------------------- Define x and y
;

if erro then begin
    ii=where(x1 ne undef and x2 ne undef and y1 ne undef and y2 ne undef,count)
    if count gt 0 then begin
        xx1=x1[ii]
        xx2=x2[ii]
        yy1=y1[ii]
        yy2=y2[ii]
    endif
endif else begin
    ii=where(x1 ne undef and y1 ne undef,count)
    if count gt 0 then begin
        xx1=x1[ii]
        yy1=y1[ii]
        if names then name=code[ii]
        if norm eq 0 then xxstd=code[ii]
    endif
endelse
if count eq 0 then begin
    print,' '
    print,'ERROR TAYLOR: All input undefined.',string(7B)
    !QUIET=q
    return
endif
;
;-------------------------------------- Determine maximum etc.
;
if (n_elements(xtit) eq 0) then begin
    if norm then xtit='Normalised Standard Deviation' else xtit='Standard Deviation'
endif
if n_elements(ytit) eq 0 then ytit='Correlation (R)'

;if erro then maxi=max([xx1,xx2],min=mini) else maxi=max(xx1,min=mini)

maxi= max(x2)
;maxi=1.5


;maxi=max([1.2,maxi])
; fix to 0.5 value above maxi
;maxi=double(fix(maxi*3))/2.+0.1
;
;-------------------------------------- Define Plot
;
if half then mini=-maxi else mini=0
if pos then begin
    plot, /polar, [0.],[0.], xrange=[-mini,maxi], yrange=[0,maxi], xstyle=5, ystyle=5, /NODATA $
      , xminor=minor, yminor=minor, xtickinterval=tickinterval, ytickinterval=tickinterval $
      , position=position, /normal, thick=thick, charthick=charthick, charsize=charsize
endif else begin
    plot, /polar, [0.],[0.], xrange=[-mini,maxi], yrange=[0,maxi], xstyle=5, ystyle=5, /NODATA $
      , xminor=minor, yminor=minor, xtickinterval=tickinterval, ytickinterval=tickinterval $
      , /device, thick=thick, charthick=charthick, charsize=charsize
endelse

;
;-------------------------------------- Plot values
;
if names then begin
    xxx=xx1*cos(yy1)
    yyy=xx1*sin(yy1)
    xyouts, xxx, yyy, name, charsize=codesize, alignment=0.5, charthick=charthick, /data,color=color
    if norm eq 0 then begin
        xxx=xxstd
        yyy=xxstd
        yyy[*]=0.1*maxi
        xyouts, xxx, yyy, name, charsize=codesize, alignment=0.5, charthick=charthick, /data,color=color
    endif        

    ccg_symbol, sym=2, fill=1, thick=4
    oplot, /polar,[1], [0.02], psym=8, symsize=1.5,color=0
    xyouts,0.5*maxi,maxi,title,charsize = 1.2,/data,charthick=thick
    xyouts,0.4,1.1,otitle,charsiz=1.5,/normal,charthick=thick

endif else begin
    erro = 0
    if erro eq 0 then begin
        ccg_symbol, sym=2, fill=1, thick=4
        oplot, /polar,[1], [0.02], psym=8, symsize=1.5,color=0
        fill=0
        nx=n_elements(xx1)
        for k=0, nx-1 do begin
            ;symm=(k+1) mod 12
            ;ccg_symbol, sym=symm, fill=fill, thick=4
            oplot, /polar, [xx1[k]], [yy1[k]], psym=8, symsize=symsize,color=color[k]
        endfor
    endif else begin
        ccg_symbol, sym=2, fill=1, thick=4
        oplot, /polar,[1], [0.02], psym=8, symsize=1.5,color=0
        fill=0
        symm=symbol
        ccg_symbol, sym=symm, fill=fill, thick=4
        nx=n_elements(xx1)
        for k=0, nx-1 do oplot, /polar, [xx1[k],xx2[k]], [yy1[k],yy2[k]], linestyle=0, thick=4,color=color[k]
    endelse

    xyouts,0.5*maxi,maxi,title,charsize = 1.2,/data
    xyouts,0.4,1.1,otitle,charsiz=1.5,/normal

endelse
;
;-------------------------------------- Determine tickinterval if not given
;
if n_elements(tickinterval) ne 0 then begin
    tickinterval=tickinterval

endif else begin
    ns=fix(maxi)
    if ns le 3 then begin
        tickinterval=0.5
    endif else begin
        nss=fix(alog10(ns))+1
        if nss eq 1 then tickinterval=1.0
        if nss eq 2 then tickinterval=5.0
        if nss eq 3 then tickinterval=50.0
        if nss gt 3 then tickinterval=500.0
    endelse
endelse
;
;-------------------------------------- Plot Axes
;
axis, 0, 0, xaxis=0, xrange=[mini,maxi], yrange=[0,maxi], xstyle=1, ystyle=1 $
    , xminor=minor, yminor=minor, xtickinterval=tickinterval, ytickinterval=tickinterval $
    , xthick=thick, charsize=charsize, charthick=charthick, xtitle=''
axis, 0, 0, yaxis=1, xrange=[mini,maxi], yrange=[0,maxi], xstyle=1, ystyle=1 $
    , xminor=minor, yminor=minor, xtickinterval=tickinterval, ytickinterval=tickinterval $
    , ytickname=replicate(' ',30), ythick=thick, charsize=charsize
axis, 0, 0, yaxis=0, xrange=[mini,maxi], yrange=[0,maxi], xstyle=1, ystyle=1 $
    , xminor=minor, yminor=minor, xtickinterval=tickinterval, ytickinterval=tickinterval $
    , ytickformat='(F3.1," ")', ythick=thick $
    , charsize=charsize, charthick=charthick, ytitle=xtit
;
;-------------------------------------- Plot Main reference circle
;
if half then part=2 else part=4
steps=part*100
steps2=steps/part
one=circle(0,0,1,steps=steps)
plots,one[0,0:steps2-1],one[1,0:steps2-1],linestyle=1,thick=thick
;--

;-------------------------------------- Draw isolines for rmse - dashed
 p=1.
 ox=0.
 oy=0.
 dxc=0.25
 for n=1,7 do begin
     rmse=n*dxc
     rad=rmse*p
     xold=ox+p
     if rmse gt  maxi - p then begin
          ;alpha=180-!radeg*acos(((4.)-rmse*rmse)/(-2*rmse))
          alpha=180-!radeg*acos(((2.*(maxi-p)+dxc*2^((maxi-p)/dxc-2))-rmse*rmse)/(-2*rmse))
     endif else begin
         alpha=0.0
     endelse
     if rmse gt  p then begin
         alpha2=180-!radeg*acos((p)/(rmse))
     endif else begin
         alpha2=180.
     endelse
     ARCS ,rad,alpha,alpha2,xold,oy,/data,/noclip,linestyle=2
 endfor

;-------------------------------------- Plot other reference circle
;
ns=fix(maxi)

if ns le 3 then begin
    stepns=tickinterval
   ; if maxi-ns ge 0.1 then ns=2*ns+1 else ns=2*ns
   ;     if maxi-ns ge 0.1 then ns=2*ns+1 else ns=4
     ns=fix(maxi/stepns) 
        
endif else begin
    nss=fix(alog10(ns))+1
    if nss eq 1 then stepns=1.0
    if nss eq 2 then stepns=5.0
    if nss eq 3 then stepns=50.0
    if nss gt 3 then stepns=500.0
   ; if ((maxi-ns) ge (maxi*0.1)) then ns=ns else ns=ns-1
        if ((maxi-ns) ge (maxi*0.1)) then ns=tickinterval else ns=tickinterval
endelse
for i=1, ns do begin
    ik=i*stepns
    if ik ne 1 and ik lt maxi then begin
        one=circle(0,0,ik,steps=steps)
        plots,one[0,0:steps2-1],one[1,0:steps2-1],linestyle=1,thick=thick
    endif
endfor
;
;-------------------------------------- Plot Correlation Axis (outer circle)

;
steps=part*300+1
steps2=steps/part
one=circle(0,0,maxi,steps=steps)
plots,one[0,0:steps2],one[1,0:steps2],linestyle=0,thick=thick
; Plot outer circle major ticks
steps=part*5+1
out=acos(indgen(steps)/10.-float((part-2)/2))
td=maxi*!P.TICKLEN
for i=0, steps-1 do oplot, /polar, [maxi, maxi-td], [out[i],out[i]],thick=thick
; Plot outer circle minor ticks
steps=part*5
out=acos(indgen(steps)/10.-float((part-2)/2)+0.05)
td=2.*td/3.
;for i=0, steps-1 do oplot, /polar, [maxi, maxi-td], [out[i],out[i]],thick=thick
; Plot outer circle fine minor ticks
steps=4
out=acos(indgen(steps)/100.+0.91)
td=2.*td/3.
for i=0, steps-1 do oplot, /polar, [maxi, maxi-td], [out[i],out[i]],thick=thick
steps=4
out=acos(indgen(steps)/100.+0.96)
for i=0, steps-1 do oplot, /polar, [maxi, maxi-td], [out[i],out[i]],thick=thick
; Plot outer circle major tick marks
steps=11
val=indgen(steps)/10.
x=1.03*maxi*val
y=1.03*maxi*sin(acos(val))
outori=asin(val)*(-180./!PI)
outname=auto_string(val,1)
for i=0, steps-1,2 do xyouts, x[i], y[i], outname[i], orientation=outori[i], alignment=0.5 $
                          , charthick=charthick, charsize = charsize
; Plot outer circle minor tick marks
val=[0.95,0.99]
x=1.03*maxi*val
y=1.03*maxi*sin(acos(val))
outori=asin(val)*(-180./!PI)
outname=auto_string(val,2)
for i=0, n_elements(val)-1,2 do xyouts, x[i], y[i], outname[i], orientation=outori[i], alignment=0.5 $
                                    , charthick=charthick, charsize = charsize
; Plot outer circle title
angle=-45
x=-1.1*maxi*cos(angle*(180./!PI))
y=-1.15*maxi*sin(angle*(180./!PI))
outname=ytit
xyouts, x, y, outname, orientation=angle, alignment=0., charthick=charthick, charsize = charsize
;
;-------------------------------------- END



!QUIET=q
;
RETURN
;
END
;-
