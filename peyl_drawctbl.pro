;+
;========================================================================
; NAME:
;       PEYL_DRAWCTBL
;
; PURPOSE:
;       DRAWS THE COLOR PALETTE HORIZONTALY(DEFAULT)
;       OR VERTICALY IF KEYWORD VERTICBAR SET
;
; CALLING SEQUENCE:
;       PEYL_DRAWCTBL [,VMIN=vmin,VMAX=vmax,POS=pos,NB_COL=nb_col,FIRST_COL=first_col,TIT=tit, $
;       VERTICBAR=verticbar,NB_INTERVAL=nb_interval, $
;       TICKVAL=tickval]
;
; INPUTS:
;       NONE.
;
; OPTIONAL INPUT PARAMETERS:
;       VMIN, VMAX: VALUES MIN AND MAX OF tab_. ARRAY CONVERTED TO A MAP
;                   BY PEYL_MAP2D, WHEN PEYL_DRAWCTBL IS CALLED
;                   FROM THE PROCEDURE PEYL_MAP2D.
;                   THEY ARE USED TO CONVERT THE PHYSICAL DATA
;                   TO THE PALETTE SCALE.
;       POS: POSITION OF THE MAP ON THE WINDOW
;       NB_COL: NUMBER OF COLORS TO BE USED TO DRAW THE MAP
;       FIRST_COL: COLOR VALUE OF THE FIRST COLOR OF THE PALETTE TO BE USED
;       TIT: ALLOWS TO PUT A TITLE
;       THICK: DEFINES TITLE AND BOX THICKNESS
;       VERTICBAR: ALLOWS TO DRAW A VERTICAL COLOR BAR TO PRESENT THE
;                   PALETTE (DEFAULT=HORIZONTAL)
;       DISCR_PAL: TO PERFORM A DISCRETE PALETTE
;       COLOR_VAL: FOR DISCRETE COLORTABLE, ARRAY CONTAINING THE VALUES OF THE COLOR
;              TO USE FOR EACH INTERVAL.
;       NB_INTERVAL: NUMBER OF INTERVALS SEPARING THE TICKS ON THE COLORBAR.
;                    THE NUMBER OF VALUES OF THE TICKS IS NB_INTERVAL+1 AND 
;                    THEY ARE SPACED REGULARLY ALONG THE COLORBAR AXIS. 
;                    IF TICKVAL IS SPECIFIED, THESE TICKS ARE PUT ON THE AXIS
;                    AND NB_INTERVAL IS NOT TAKEN INTO ACCOUNT.
;       TICKVAL: VALUES OF THE TICKS.
;       CHARTHICK: DEFINES COLORBAR TICKNAME CHARTHICK
;       CHARSIZE: DEFINES COLORBAR TICKNAME CHARSIZE
;       FORMAT: DEFINES COLORBAR TICKNAME FORMAT
;       TICKNAME_INTERVAL: VALUE OF COLORBAR  TICKNAME INTERVAL
;                              EX -->  LEG_TICKNAME_INTERVAL = 2 MEANS
;                                      THAT ONLY ONE TICK OUT OF TWO HAS
;                                      A TICKNAME.
;       COL_TEXT: DEFINES TEXT COLOR
;
; OUTPUTS:
;       A COLORBAR DRAWN ON THE CURRENT WINDOW (ON SPECIFIED DEVICE:
;       SCREEN, FILE.PS,...)
;       
; RESTRICTIONS:
; 
;
;========================================================================

PRO peyl_drawctbl,VMIN = vmin, VMAX = vmax, POS = pos, $
                  NB_COL = nb_col, FIRST_COL = first_col,$
                  TIT = tit, THICK = thick, $
                  VERTICBAR = verticbar, DISCR_PAL = discr_pal, $
                  NB_INTERVAL = nb_interval_, COLOR_VAL = color_val, TICKVAL = tickval,$
                  CHARTHICK = charthick, CHARSIZE = charsize,$
                  FORMAT = format, TICKNAME_INTERVAL = tickname_interval,$
                  COL_TEXT = col_text

;==== Initialisations diverses

IF NOT KEYWORD_SET(nb_col) or (n_elements(nb_col) eq 0 )THEN nb_col = !D.N_COLORS-1
IF NOT KEYWORD_SET(first_col) THEN first_col = 0
IF NOT KEYWORD_SET(col_text) THEN col_text = 0
IF (n_elements(vmin) eq 0) THEN vmin=0
IF (n_elements(vmax) eq 0) THEN vmax=nb_col-1
IF (n_elements(discr_pal) eq 0) THEN discr_pal=0
IF NOT KEYWORD_SET(TIT) then tit=''
IF NOT KEYWORD_SET(NB_INTERVAL_) then nb_interval=0 else nb_interval=nb_interval_
IF n_elements(charsize) eq 0 then charsize=0.5
IF n_elements(charthick) eq 0 then charthick=2
IF n_elements(tickname_interval) eq 0 then tickname_interval=1
IF n_elements(format) eq 0 then format = '(F5.2)'

;--- Si le nombre d'intervalles est nul ou n'est pas specifie, valeur
;    par defaut = 10
IF (NB_INTERVAL eq 0) THEN BEGIN
    IF ((nb_col LE 10) AND (nb_col NE 0)) THEN NB_INTERVAL=nb_col $
    ELSE NB_INTERVAL=10
ENDIF

;--- Si la valeur des interval est definit, c'est elle qui prime!
IF (n_elements(tickval) NE 0) THEN nb_interval = n_elements(tickval)-1

;-- check color_val
IF (n_elements(color_val) ne NB_INTERVAL) and (n_elements(color_val) ne 0) THEN BEGIN
    print,'The number of elements of color_val must be equal to nb_interval'
    STOP
ENDIF

;==== Dessin des couleurs de la barre
IF !D.NAME EQ 'PS' THEN BEGIN
    IF NOT(discr_pal) THEN BEGIN
        V = BYTARR(nb_col,2)
        V(*,0) = BYTE(INDGEN(nb_col)+first_col)
    ENDIF ELSE BEGIN

        ;-- Defines bytes array
        IF n_elements(color_val) GT 0 THEN BEGIN
            TMP = BYTE(color_val)
        ENDIF ELSE BEGIN
            TMP = BYTE((INDGEN(nb_interval)+1)*(nb_col)/(nb_interval)+first_col)
        ENDELSE
        ;-- Defines parts of the discrete palette   
        ;IF (n_elements(tickval) NE 0) THEN BEGIN
        ;    nint = 5000
        ;    V = BYTARR(nint,2)
        ;    bound1 = 0
        ;    FOR i=0,nb_interval-1 DO BEGIN
        ;        ;ratio = float((tickval(i+1)-tickval(i)))/(tickval(nb_interval)-tickval(0))
        ;        ratio = float((tickval(i+1)-tickval(i)))/(vmax-vmin)
        ;        bound2 = bound1 + fix(nint*ratio)-1
        ;        V(bound1:bound2,0) = TMP(i)
        ;        bound1 = bound2 + 1
        ;    ENDFOR
        ;ENDIF ELSE BEGIN
            V = BYTARR(nb_interval,2)
            V(*,0) = TMP
        ;ENDELSE
    ENDELSE
    V(*,1) = V(*,0)
    IF (verticbar ne 0) THEN BEGIN
        V=rotate(V, 1)
    ENDIF
    tv,V,pos(0),pos(1),/NORMAL,xsize=pos(2)-pos(0),ysize=pos(3)-pos(1)
ENDIF ELSE BEGIN
    pxsize = LONG((POS(2)-POS(0))*!D.X_SIZE + 0.5)
    pysize = LONG((POS(3)-POS(1))*!D.Y_SIZE + 0.5)
    V = BYTARR(pxsize,pysize)
    Vv = BYTE(INDGEN(pxsize)*(nb_col-1.)/float(pxsize-1)+first_col)
    FOR i=0,pysize-1 DO V(*,i) = Vv
    tv,V,pos(0),pos(1),/NORMAL
ENDELSE
;
;-------------------------------------------------------
;  Dessin du cadre, des ticks et ecriture des valeurs
;-------------------------------------------------------
;
wtickname = replicate(' ',60)
;---
yticklen=0.2
xticklen=0.3
IF discr_pal EQ 1 THEN BEGIN 
    yticklen=0.5
    xticklen=0.6
ENDIF

;---
IF (verticbar ne 0) THEN BEGIN
    charsize_=charsize/2.
    ;--- Define y-axis ticks names
    YTICKNAME = replicate(' ',nb_interval+1)
    if n_elements(tickval) eq 0 then begin
        YTICKNAME_TMP = vmin +  float((indgen(nb_interval+1))) * (vmax-vmin)/(nb_interval)
    endif else begin
    ;    if  n_elements(tickval) eq nb_interval+1 then begin
    ;        YTICKNAME_TMP = string(tickval,format=format)
        YTICKNAME_TMP = tickval(0) + float((indgen(nb_interval+1))) * (tickval(nb_interval)-tickval(0))/(nb_interval)
    ;    endif else begin
    ;        print,'error : the number of interval values you give must be equal to '+auto_string(nb_interval+1)
    ;        stop
    ;    endelse
    endelse
    if n_elements(tickval) eq 0 then tickval=YTICKNAME_TMP
    for jj = 0,nb_interval,tickname_interval do begin
        if (round(tickval(jj))-tickval(jj)) ne 0 then begin
            YTICKNAME(jj) = string(tickval(jj),format=format) ;XTICKNAME_TMP(jj)
        endif else begin
            YTICKNAME(jj) = strcompress(string(tickval(jj),format='(I)'),/remove_all)
        endelse
    endfor 
    ;--- Cadre
     plot,[0.,1.],[0.,1.],yrange=[vmin,vmax],POS=pos,/NODATA,/NORMAL,$
       charthick = charthick, XSTYLE=1,YSTYLE=1,XTICKS=1,XMINOR=1,/NOERASE, XTICKINTERVAL=nb_interval+1,$
       YTICKNAME=wtickname, XTICKNAME=replicate(' ',10),col=col_text,YTICKS=nb_interval,yticklen=yticklen,$
       ytitle=tit,XTHICK=thick,YTHICK=thick,YTICKV=YTICKNAME_TMP;tickval
    ;--- Ticks value
;        AXIS, YAXIS=1,$
;          charthick = charthick, XSTYLE=1,YSTYLE=1,/NOERASE,$
;          YTICKS=nb_interval,charsize=charsize,col=col_text,$
;          XTHICK=thick,YTHICK=thick,YTICKNAME = YTICKNAME,YTICKV=YTICKNAME_TMP;tickval
    ;---
      yticks=float(YTICKNAME_TMP)
      ; CONVERT THE XTICK VALUES TO NORMALIZED POSITION...
      ytickpos = !y.s[0] + !y.s[1]*yticks
      ; GET THE CHARACTER HEIGHT AND WIDTH IN NORMALIZED COORDINATES...
      y_ch_size = charsize_ * float(!d.y_ch_size) / !d.y_vsize
      x_ch_size = charsize_ * float(!d.x_ch_size) / !d.x_vsize
      ; PUT THE LABELS AT THE APPROPRIATE TICK POSITIONS...
      FOR jj = 0,nb_interval,tickname_interval DO BEGIN
          ;xyouts, !x.window[1]+0.01+1.5*x_ch_size,ytickpos(jj)-0.25*y_ch_size, /NORM,YTICKNAME(jj), ALIGN=0.5, ORIENTATION=0, CHARSIZE=charsize_,col=col_text
          xyouts, !x.window[1]+1.5*x_ch_size,ytickpos(jj)-0.25*y_ch_size, /NORM,YTICKNAME(jj), ALIGN=0.5, ORIENTATION=0, CHARSIZE=charsize_,col=col_text
      ENDFOR
ENDIF ELSE BEGIN
    charsize_=charsize/2.
    ;--- Define x-axis ticks names
    XTICKNAME = replicate(' ',nb_interval+1)
    if n_elements(tickval) eq 0 then begin
        XTICKNAME_TMP = vmin +  float((indgen(nb_interval+1))) * (vmax-vmin)/(nb_interval)
    endif else begin
        ;XTICKNAME_TMP = tickval(1) + (indgen(nb_interval+1)) * (tickval(nb_interval-1)-tickval(1))/(nb_interval)
        XTICKNAME_TMP = tickval(0) +  float((indgen(nb_interval+1))) * (tickval(nb_interval)-tickval(0))/(nb_interval)
    ;    if  n_elements(tickval) eq nb_interval+1 then begin
    ;        XTICKNAME_TMP = string(tickval,format=format)
    ;    endif else begin
    ;        print,'error : the number of interval values you give must be equal to '+auto_string(nb_interval+1)
    ;        stop
    ;    endelse
    endelse
    if n_elements(tickval) eq 0 then tickval=XTICKNAME_TMP
    for jj = 0,nb_interval,tickname_interval do begin
        ;if (round(tickval(jj))-tickval(jj)) ne 0 then begin
            XTICKNAME(jj) = string(tickval(jj),format=format) ;XTICKNAME_TMP(jj)
        ;endif else begin
        ;    XTICKNAME(jj) = strcompress(string(tickval(jj),format='(I)'),/remove_all)
        ;endelse
    endfor 
    
    ;--- Cadre
     plot,[0.,1.],[0.,1.],xrange=[vmin,vmax],POS=pos,/NODATA,/NORMAL,$
       charthick = charthick, XSTYLE=1,YSTYLE=1,YMINOR=1,/NOERASE, YTICKINTERVAL=nb_interval+1,$
       XTICKNAME=wtickname, YTICKNAME=replicate(' ',10),col=col_text,XTICKS=nb_interval,xticklen=xticklen,$
       title=tit,charsize=charsize_,XTHICK=thick,YTHICK=thick;,XTICKV=XTICKNAME_TMP;tickval
;      ;--- Ticks value
;        AXIS, XAXIS=0,$
;          charthick = charthick, XSTYLE=1,YSTYLE=1,/NOERASE,$
;          XTICKS=nb_interval,charsize=charsize,col=col_text,$
;          XTHICK=thick,YTHICK=thick,XTICKNAME = XTICKNAME;,XTICKV=XTICKNAME_TMP;tickval
    ;---
     xticks=float(XTICKNAME_TMP)
     ; CONVERT THE XTICK VALUES TO NORMALIZED POSITION...
     xtickpos = !x.s[0] + !x.s[1]*xticks
     ; GET THE CHARACTER HEIGHT AND WIDTH IN NORMALIZED COORDINATES...
     y_ch_size = charsize_ * float(!d.y_ch_size) / !d.y_vsize
     x_ch_size = charsize_ * float(!d.x_ch_size) / !d.x_vsize
     ; PUT THE LABELS AT THE APPROPRIATE TICK POSITIONS...
     FOR jj = 0,nb_interval,tickname_interval DO BEGIN
         xyouts, xtickpos(jj), !y.window[0]-y_ch_size, /NORM,XTICKNAME(jj), ALIGN=0.5, ORIENTATION=0, CHARSIZE=charsize_,col=col_text
     ENDFOR
ENDELSE

END
;-

