;+
; NAME:
;	CCG_MEANS
;
; PURPOSE:
;	Calculate annual, monthly, or 
;	daily averages.  Return arrays
;	of averages, number and standard
;	deviation. 
;
; CATEGORY:
;	Miscellaneous.
;
; CALLING SEQUENCE:
;	CCG_MEANS,	xarr=xarr,yarr=yarr,year=1,xres,yres,sd,n
;	CCG_MEANS,	xarr=xarr,yarr=yarr,month=1,xres,yres,sd,n
;	CCG_MEANS,	xarr=xarr,yarr=yarr,day=1,xres,yres,sd,n
;
; INPUTS:
;	xarr:		Decimal date array.
;	yarr:		Corresponding values.
;
; OPTIONAL INPUT PARAMETERS:
;	year:		If non-zero calculate annual averages.
;	month:		If non-zero calculate monthly averages.
;	day:		If non-zero calculate daily averages.
;
; OUTPUTS:
;	xres:		Decimal date array for annual, monthly, or
;			daily averages.  Interval is centered, i.e.,
;			1985.5.	
;
;	yres:		Corresponding averages array.
;
;	sd:		Array of standard deviations of the
;			the calculated averages.
;
;	n:		Number of individual values used in 
;			determining average.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	CCG_MEANS may be called from the IDL command line or
;	from an IDL procedure.
;		Example:
;			CCG_FREAD,	$
;			file='/home/gcarb/data/ch4/in-situ/mlo_data/year/mlo93.ch4',nc=6,var
;			;
;			CCG_DATE2DEC,	$
;			yr=var(1,*),mo=var(2,*),dy=var(3,*),hr=var(4,*),dec=dec
;			;
;			CCG_MEANS,	$
;			xarr=dec,yarr=var(5,*),month=1,xres,yres,sd,n
;			.
;			.
;			.
;
;		
; MODIFICATION HISTORY:
;
;	Written, KAM, March 1994.
;-
;
PRO 	CCG_MEANS,	xarr=xarr,yarr=yarr,$
			year=year,month=month,day=day,$
			xres,yres,sd,n
;
;-------------------------------------- check input parameters
;
IF NOT KEYWORD_SET(xarr) OR $
   NOT KEYWORD_SET(yarr) THEN BEGIN
	CCG_MESSAGE,$
	"'xarr', 'yarr', and interval parameters must all be set.  Exiting ..."
	CCG_MESSAGE,"ex:  CCG_MEANS, xarr=x,yarr=y,month=1,xres,yres,sd,n"
	RETURN
ENDIF
;
;-------------------------------------- miscellaneous initialization
;
num=N_ELEMENTS(xarr)
xres=DBLARR(num)
yres=FLTARR(num)
sd=FLTARR(num)
n=INTARR(num)
;
;calculate annual averages
;
IF KEYWORD_SET(year) THEN BEGIN
	minyr=FIX(MIN(xarr))
	maxyr=FIX(MAX(xarr))
	k=0
	FOR i=minyr,maxyr DO BEGIN
		j=WHERE(FIX(xarr) EQ i)
		IF j(0) NE -1 THEN BEGIN
			yres(k)=CCG_MEAN(yarr(j))
			xres(k)=i+.5
			n(k)=N_ELEMENTS(j)
			IF n(k) GT 1 THEN BEGIN
				sd(k)=STDEV(yarr(j))
			ENDIF ELSE BEGIN
				sd(k)=(-9)
			ENDELSE
			k=k+1
		ENDIF
	ENDFOR
ENDIF
;
;calculate monthly averages
;
IF KEYWORD_SET(month) THEN BEGIN
	CCG_DEC2DATE,xarr,yr,mo,dy,hr
	minyr=FIX(MIN(xarr))
	maxyr=FIX(MAX(xarr))
	k=0
	FOR y=minyr,maxyr DO BEGIN
		FOR m=1,12 DO BEGIN
			j=WHERE(yr EQ y AND mo EQ m)
			IF j(0) NE -1 THEN BEGIN
				yres(k)=CCG_MEAN(yarr(j))
				y1=y & m1=m 
				CCG_DATE2DEC,yr=y1,mo=m1,dy=15,hr=12,mn=0,dec=dec
				xres(k)=dec
				n(k)=N_ELEMENTS(j)
				IF n(k) GT 1 THEN BEGIN
					sd(k)=STDEV(yarr(j))
				ENDIF ELSE BEGIN
					sd(k)=(-9)
				ENDELSE
				k=k+1
			ENDIF
		ENDFOR
	ENDFOR
ENDIF
;
;calculate daily averages
;
IF KEYWORD_SET(day) THEN BEGIN
	CCG_DEC2DATE,xarr,yr,mo,dy,hr
	minyr=FIX(MIN(xarr))
	maxyr=FIX(MAX(xarr))
	k=0
	FOR y=minyr,maxyr DO BEGIN
		FOR m=1,12 DO BEGIN
			FOR d=1,31 DO BEGIN
				j=WHERE((yr EQ y) AND (mo EQ m) AND (dy EQ d))
				IF j(0) NE -1 THEN BEGIN
					yres(k)=CCG_MEAN(yarr(j))
					y1=y & m1=m & d1=d
					CCG_DATE2DEC,$
					yr=y1,mo=m1,dy=d1,hr=12,mn=0,dec=dec
					xres(k)=dec
					n(k)=N_ELEMENTS(j)
					IF n(k) GT 1 THEN BEGIN
						sd(k)=STDEV(yarr(j))
					ENDIF ELSE BEGIN
						sd(k)=(-9)
					ENDELSE
					k=k+1
				ENDIF
			ENDFOR
		ENDFOR
	ENDFOR
ENDIF
xres=xres(0:k-1)
yres=yres(0:k-1)
sd=sd(0:k-1)
n=n(0:k-1)
RETURN
END
