;+
; NAME:
;	PEYL_SLEGEND
;
; PURPOSE:
;	Create a symbol and text legend 
;	and place it on the plotting 
;	surface.
;	MODIFICATION OF CCG_SLEGEND TO BE ABBLE TO DEFINE THE 
;	POSITION OF THE LEGEND IN DATA COORDINATES.... ELSE 
;	WORK LIKE CCG_SLEGEND....
;
;	Type 'legend_ex' for a legend example.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	PEYL_SLEGEND,x=x,y=y,tarr=tarr,sarr=sarr,farr=farr,carr=carr,
;		     /data
;
; INPUTS:
;	x: y:	upper left corner of legend.
;		Specify in NORMAL coordinates	
;		i.e., bottom/left of plotting surface -> x=0,y=0 
;		      top/right of plotting surface   -> x=1,y=1 
;		IF THE KEYWORD DATA IS SPECIFIED THEN X AND Y
;		CORRESPOND TO DATA COORDINATES...
;
;	tarr:  	text vector
;	sarr:	symbol vector (see SYMBOL_EX procedure)
;	farr:	fill symbol vector
;		Values of 1 -> fill symbol
;		Values of 0 -> open symbol
;	carr:	color vector (values [0-255])
;
;	NOTE:	All vectors must be the same length
;
; OPTIONAL INPUT PARAMETERS:
;	charsize:	text size (default: 1)
;	charthick:	text thickness (default: 1)
;	frame:		If set to 1 - draw legend frame
;			If set to 0 - no legend frame	
;	data:           keyword : if set then data coord.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	All vectors must be the same length
;	Procedure uses symbol assignments from
;	the CCG library CCG_SYMBOL procedure.
;
; PROCEDURE:
;	
;	Example:
;
;		PRO 	example,dev=dev
;		;
;		OPENDEVICE,dev=dev,pen=pen
;		.
;		.
;		.
;		;
;		;draw initial plot.
;		;
;		PLOT,	[0,0],[1,1]
;		;
;		;define vectors to be passed to PEYL_SLEGEND.
;		;
;		tarr=[	'SQUARE','CIRCLE','TRIANGLE 1','TRIANGLE 2',$
;			'DIAMOND','STAR 1','STAR 2','HOURGLASS',$
;			'BOWTIE','PLUS','ASTERISK','CIRCLE/PLUS',$
;			'CIRCLE/X']
;
;		sarr=[1,2,3,4,5,6,7,8,9,10,11,12,13]
;
;		farr=[1,1,1,1,1,1,1,1,1,0,0,0,0]
;
;		carr=[	pen(1),pen(2),pen(3),pen(4),pen(5),pen(6),$
;			pen(7),pen(8),pen(9),pen(10),pen(11),pen(12),$
;			pen(1)]
;		;
;		;call to PEYL_SLEGEND
;		;
;		PEYL_SLEGEND,	x=.2,y=.8,$
;				tarr=tarr,$
;				farr=farr,$
;				sarr=sarr,$
;				carr=carr,$
;				charthick=2.0,$
;				charsize=1.0,$
;				frame=1
;		.
;		.
;		.
;		CLOSEDEVICE,dev=dev
;		END
;
; MODIFICATION HISTORY:
;	Written, KAM, February 1994.
;
PRO    	PEYL_SLEGEND,	x=x,y=y,$
			tarr=tarr,sarr=sarr,farr=farr,carr=carr,$
			thickarr=thickarr,$
			charsize=charsize,$
			symsize=symsize,$
			charthick=charthick,$
			frame=frame,$
                        incr_text=incr_text,$
			data=data
;
;return to caller if an error occurs
;
ON_ERROR,2	
;
;If keywords are not set then assign
;default values
;
IF NOT KEYWORD_SET(charthick) THEN charthick=1
IF NOT KEYWORD_SET(charsize) THEN charsize=1
;IF NOT KEYWORD_SET(symsize) THEN charsize=1.3*charsize
IF NOT KEYWORD_SET(carr) THEN carr=MAKE_ARRAY(N_ELEMENTS(sarr),/INT,VALUE=255)
IF NOT KEYWORD_SET(farr) THEN farr=MAKE_ARRAY(N_ELEMENTS(sarr),/INT,VALUE=0)
IF NOT KEYWORD_SET(tarr) THEN tarr=MAKE_ARRAY(N_ELEMENTS(sarr),/STR,VALUE='')
IF NOT KEYWORD_SET(thickarr) THEN thickarr=MAKE_ARRAY(N_ELEMENTS(sarr),/INT,VALUE=1)
IF NOT KEYWORD_SET(x) THEN x=0.5
IF NOT KEYWORD_SET(y) THEN y=0.5
IF NOT CCG_VDEF(incr_text) THEN incr_text=1.

IF NOT KEYWORD_SET(data) THEN data=0
;
;determine number of lines
;
n=N_ELEMENTS(tarr)
;
;determine length of longest text
;
tlen=MAX(STRLEN(tarr))
;
;y increment for text
;
yinc=.025*charsize*incr_text
if data then yinc=(!y.crange(1)-!y.crange(0))/15. * incr_text
;
FOR i=0,n-1 DO BEGIN

	CCG_SYMBOL,	sym=sarr(i),fill=farr(i),thick=thickarr(i)

if data then begin 


	PLOTS, 		x,y-(yinc*i),$
			/DATA,$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=carr(i)

	XYOUTS, 	x+(!x.crange(1)-!x.crange(0))/20.,y-(yinc*(i+1./4.)),$
			/DATA,$
			tarr(i),$
			ALI=0,$
			CHARTHICK=charthick,$
			CHARSIZE=charsize,$
			COLOR=carr(i)
endif else begin 


	PLOTS, 		x,y-(yinc*i),$
			/NORMAL,$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=carr(i)

	XYOUTS, 	x+.015*charsize,y-(yinc*i)-(.007*charsize),$
			/NORMAL,$
			tarr(i),$
			ALI=0,$
			CHARTHICK=charthick,$
			CHARSIZE=charsize,$
			COLOR=carr(i)
endelse

ENDFOR
;
;build legend frame
;
IF KEYWORD_SET(frame) THEN BEGIN

        if data then begin
	   print,'No frame possible in the /DATA coordinates..'
	   return
	endif

	PLOTS,	x-(.02*charsize),$
		y+(.03*charsize),$
		/NORMAL
	PLOTS,	x-(.02*charsize),$
		y-(n*.025*charsize),$
		/NORMAL,$
		/CONTINUE
	PLOTS,	x+(charsize*.015)+(charsize*tlen*.0095)+(charsize*.012),$
		y-(n*.025*charsize),$
		/NORMAL,$
		/CONTINUE
	PLOTS,	x+(charsize*.015)+(charsize*tlen*.0095)+(charsize*.012),$
		y+(.03*charsize),$
		/NORMAL,$
		/CONTINUE
	PLOTS,	x-(.02*charsize),$
		y+(.03*charsize),$
		/NORMAL,$
		/CONTINUE
ENDIF
END
;-

