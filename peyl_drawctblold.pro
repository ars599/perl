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
;       VERTICBAR=verticbar,AXISTITLE=axistitle,NB_INTERVAL=nb_interval, $
;       VAL_INTERVAL=val_interval]
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
;       VERTICBAR: ALLOWS TO DRAW A VERTICAL COLOR BAR TO PRESENT THE
;                   PALETTE (DEFAULT=HORIZONTAL)
;       NB_INTERVAL: NUMBER OF INTERVALS SEPARING THE TICKS ON THE COLORBAR.
;                    IF SPECIFIED WITHOUT ANY VALUES (OR ZEROS) PUT IN
;                    THE ARRAY VAL_INTERVAL, THE NUMBER OF VALUES OF THE TICKS
;                    IS NB_INTERVAL NUMBER AND THE TICKS ARE SPACED REGULARLY
;                    ALONG THE COLORBAR AXIS. 
;                    IF SPECIFIED AND THE ARRAY VAL_INTERVAL FILLED WITH
;                    NB_INTERVAL+1 VALUES, THESE TICKS ARE PUT ON THE AXIS
;                    AND A TICK '0' IS ADDED.
;       VAL_INTERVAL: VALUES OF THE TICKS DELIMITATING THE NUMBER NB_INTERVAL
;                     OF INTERVALS ON THE COLORBAR AXIS.  A TICK '0' IS ADDED.
;       NR_SIZE:      SIZE OF THE FIGURES WRITTEN NEXT TO THE COLOR BAR
;
; OUTPUTS:
;       A COLORBAR DRAWN ON THE CURRENT WINDOW (ON SPECIFIED DEVICE:
;       SCREEN, FILE.PS,...)
;       
; RESTRICTIONS:
; 
;
;========================================================================

PRO peyl_drawctblold,vmin=vmin,vmax=vmax,POS=pos,NB_COL=nb_col,FIRST_COL=first_col,$
                  TIT=tit, VERTICBAR=verticbar,AXISTITLE=axistitle,$
                  NB_INTERVAL=nb_interval_, VAL_INTERVAL=val_interval,$
                  leg_size=leg_size,leg_thick=leg_thick,col_text=col_text

;==== Initialisations diverses

IF NOT KEYWORD_SET(nb_col) or (n_elements(nb_col) eq 0 )THEN nb_col = !D.N_COLORS-1
IF NOT KEYWORD_SET(first_col) THEN first_col = 0
IF NOT KEYWORD_SET(col_text) THEN col_text = 0
IF (n_elements(vmin) eq 0) THEN vmin=0
IF (n_elements(vmax) eq 0) THEN vmax=nb_col-1
IF NOT KEYWORD_SET(AXISTITLE) then axistitle=''
IF NOT KEYWORD_SET(NB_INTERVAL_) then nb_interval=0 else nb_interval=nb_interval_
IF n_elements(leg_SIZE) eq 0 then leg_size=1.
IF n_elements(leg_THICK) eq 0 then leg_thick=1.

;--- Si le nombre d'intervalles est nul ou n'est pas specifie, valeur
;    par defaut = 10
IF (NB_INTERVAL eq 0) THEN BEGIN
    if ((nb_col le 10) and (nb_col ne 0)) then NB_INTERVAL=nb_col $
    else NB_INTERVAL=10
    VAL_INTERVAL=INDGEN(nb_interval+1)*(vmax-vmin)/float(nb_interval) + vmin
    if (vmax-vmin) gt (2*nb_interval) then val_interval = round(val_interval)
ENDIF ELSE BEGIN

;;;; Check si val_interval passer et reajuster nb_interval si il le faut

ENDELSE

;==== Dessin des couleurs de la barre

IF !D.NAME EQ 'PS' THEN BEGIN
    V = BYTARR(nb_col,2)
    V(*,0) = BYTE(1+INDGEN(nb_col)+first_col)
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

;==== Dessin du cadre, des ticks et ecriture des valeurs

IF (verticbar ne 0) THEN BEGIN
    plot,[0.,1.],[0.,1.],yrange=[vmin,vmax],POS=pos,/NODATA,/NORMAL,$
      charthick = leg_thick, XSTYLE=1,YSTYLE=1,/NOERASE,TIT=tit, XTICKINTERVAL=5,$
      YTICKNAME=[' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '], XTICKNAME=[' '],col=col_text
    AXIS, YAXIS=1,$
      charthick = leg_thick, XSTYLE=1,YSTYLE=1,/NOERASE,XTICKINTERVAL=5, $
      charsize=leg_size,SUBTITLE=axistitle,$
      YTICKS=nb_interval,YTICKV=val_interval,col=col_text

ENDIF ELSE BEGIN
    plot,[0.,1.],[0.,1.],xrange=[vmin,vmax],POS=pos,/NODATA,/NORMAL,$
      charthick = leg_thick,xthick=leg_thick,XSTYLE=1,YSTYLE=1,XTICKLEN=0.1,/NOERASE,TIT=tit,$
      YRANGE=[0, 1.],YTICKINTERVAL=5, YTICKNAME=[' '],SUBTITLE=axistitle,$
      XTICKS=nb_interval,XTICKV=val_interval,charsize=leg_size,col=col_text
ENDELSE

END
;-

