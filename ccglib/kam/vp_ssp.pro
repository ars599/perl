PRO	VP_SSP,$
	data=data,$
	site=site,$
	sp=sp,$
	matchto=matchto,$
	plotthick=plotthick,$
	linethick=linethick,$
	charsize=charsize,$
	charthick=charthick,$
	symsize=symsize,$
	row=row,col=col,$
	norej=norej,nonb=nonb,$
	dev=dev
;
;This procedure plots single or multiple species profiles.
;One species per profile.
;
;Miscellaneous initialization
;
DEFAULT=(-999.99)
nsp=N_ELEMENTS(sp)
IF NOT KEYWORD_SET(plotthick) THEN plotthick=1.0
IF NOT KEYWORD_SET(linethick) THEN linethick=2.0
IF NOT KEYWORD_SET(charsize) THEN charsize=2.25
IF CCG_VDEF(dev) THEN charsize=charsize*0.6
IF NOT KEYWORD_SET(charthick) THEN charthick=1.5
IF NOT KEYWORD_SET(symsize) THEN symsize=1.0
;
;Plot profiles
;
i=0 & ptr=0 & forever=1
multi_limit=3
IF NOT KEYWORD_SET(row) AND NOT KEYWORD_SET(col) THEN BEGIN
	default_multi=1
	IF nsp LE multi_limit THEN BEGIN
		row=1 & col=nsp
	ENDIF ELSE BEGIN
		row=2 & col=CEIL(nsp/FLOAT(row))
	ENDELSE
ENDIF ELSE BEGIN
	default_multi=0
	;
	;The number of rows and colums should
	;be multiple of number of species.
	;
	IF row*col MOD nsp NE 0 THEN BEGIN
		row=1 & col=nsp
		CCG_MESSAGE,   'row*col must be multiple of number of species.'
		CCG_MESSAGE,   'row set to '+STRCOMPRESS(STRING(row),/RE)+$
					   '.  col set to '+STRCOMPRESS(STRING(col),/RE)+'.'
	ENDIF
ENDELSE
;
;Set up graphics device
;
IF row*col GT 1 THEN landscape=1 ELSE landscape=0
portrait=(landscape EQ 0)

CCG_OPENDEV,	dev=dev,pen=pen,$
		xpixels=513*portrait+728*landscape,$
		ypixels=513*landscape+728*portrait,$
		landscape=landscape,portrait=portrait

!P.MULTI=[0,col,row,0,0]

flight=TAG_NAMES(data)
nflights=N_ELEMENTS(flight)
number=1 & ptr=0
tags=TAG_NAMES(data.(number))
;
;What is the range of altitudes?
;
altrange=[99999,-99999]
FOR i=1,nflights-1 DO BEGIN
	altrange[0]=(MIN(data.(i).alt) LT altrange[0])?MIN(data.(i).alt):altrange[0]
	altrange[1]=(MAX(data.(i).alt) GT altrange[1])?MAX(data.(i).alt):altrange[1]
ENDFOR
;
WHILE forever DO BEGIN

	FOR isp=0,nsp-1 DO BEGIN

		zz=data.(number)
		tagptr=WHERE(tags EQ STRUPCASE(sp[isp]))
		tagptr=tagptr[0]
		j=WHERE(data.(number).(tagptr)-1 GT DEFAULT)
		IF j[0] NE -1 THEN zz=data.(number)(j) 

		zzz=zz

		IF KEYWORD_SET(norej) THEN BEGIN
			j=WHERE(STRMID(zzz.(tagptr+1),0,1) EQ '.')
			IF j[0] NE -1 THEN zzz=zz(j)
		ENDIF

		IF KEYWORD_SET(nonb) THEN BEGIN
			j=WHERE(STRMID(zzz.(tagptr+1),1,1) EQ '.')
			IF j[0] NE -1 THEN zzz=zz(j)
		ENDIF

		CCG_SPDEFS,sp=sp[isp],title=xtitle
    
		PLOT,    [zzz.(tagptr)],[zzz.alt],$
			 /NODATA,$
			 CHARSIZE=charsize,$
			 CHARTHICK=charthick,$
			 COLOR=pen(1),$
			 TITLE=STRUPCASE(site)+' '+STRMID(flight[number],1,100),$

			 YSTYLE=16,$
			 YCHARSIZE=1.0,$
			 YTHICK=plotthick,$
			 YRANGE=altrange,$
			 YTITLE='ALTITUDE (m)',$

			 XSTYLE=16,$
			 XTHICK=plotthick,$
			 XCHARSIZE=1.0,$
			 XTITLE=xtitle

		IF NOT KEYWORD_SET(norej) THEN BEGIN
			j=WHERE(STRMID(zz.(tagptr+1),0,1) NE '.')
			IF j[0] NE -1 THEN BEGIN
				CCG_SYMBOL,sym=11
				OPLOT,[zz[j].(tagptr)],[zz[j].alt],$
					PSYM=8,$
					SYMSIZE=symsize,$
					COLOR=pen(2)
			ENDIF
		ENDIF

		IF NOT KEYWORD_SET(nonb) THEN BEGIN
			j=WHERE(STRMID(zz.(tagptr+1),1,1) NE '.')
			IF j[0] NE -1 THEN BEGIN
				CCG_SYMBOL,sym=10
				OPLOT,[zz[j].(tagptr)],[zz[j].alt],$
				PSYM=8,$
				SYMSIZE=symsize,$
				COLOR=pen(4)
			ENDIF
		ENDIF

		j=WHERE(STRMID(zz.(tagptr+1),0,2) EQ '..')
		IF j[0] NE -1 THEN BEGIN

			OPLOT,[zz[j].(tagptr)],[zz[j].alt],$
                        LINESTYLE=0,$
                        NOCLIP=1,$
                        COLOR=pen(3),$
                        THICK=linethick

			FOR k=0,N_ELEMENTS(j)-1 DO BEGIN
				CCG_SYMBOL,sym=1
				PLOTS,zz[j[k]].(tagptr),zz[j[k]].alt,$
				/NOCLIP,$
				PSYM=8,$
				SYMSIZE=symsize,$
				COLOR=pen(3)
			ENDFOR
		ENDIF

		IF !P.MULTI(0) EQ row*col-1 THEN ptr=number
	ENDFOR

   IF NOT KEYWORD_SET(dev) THEN BEGIN
      ;
      ;Hold panel until mouse interrupt.
      ;
      ;If left button then move forward one page.
      ;If middle button then move backward one page.
      ;If right button then quit.
      ;
      IF !P.MULTI(0) EQ 0 OR (isp EQ nsp AND nsp GT 1 AND default_multi EQ 1) $
        OR number EQ nflights-1 THEN BEGIN

         CCG_MESSAGE,    "Left/Middle/Right -> Forward/Backward/Exit"
         CURSOR,xx,yy,/DOWN
         CASE !ERR OF
         1:    IF number EQ nflights-1 THEN number=ptr-1
         2:    BEGIN
               number=ptr-((row*col)/nsp)-1
               IF number LT 1 THEN number=(0)
               END
	 4:    forever=0
	 ELSE:
	 ENDCASE
	 !P.MULTI(0)=0
      ENDIF
   ENDIF ELSE IF number EQ nflights-1 THEN forever=0
   number=number+1
ENDWHILE
;
;Close graphics device
;
CCG_CLOSEDEV,dev=dev
END
