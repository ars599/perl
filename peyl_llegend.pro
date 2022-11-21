;+
; NAME:
;	PEYL_LLEGEND
;
; PURPOSE:
;	Create a line and text legend 
;	and place it on the plotting 
;	surface.
;	MODIFICATION OF CCG_LLEGEND TO BE ABBLE TO DEFINE THE 
;	POSITION OF THE LEGEND IN DATA COORDINATES.... ELSE 
;	WORK LIKE CCG_LLEGEND.... AND ALSO TO INCLUDE SYMBOL
;	IN THE LINE PLOTTING (using ccg_symbol definitions)...
;
;	Type 'legend_ex' for a legend example.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	PEYL_LLEGEND,x=x,y=y,tarr=tarr,larr=larr,carr=carr,
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
;	larr:	line type vector (see IDL for LINESTYLE)
;	carr:	color vector (values [0-255])
;	sarr:   symbol array : to add on the line plot 
;		according to ccg_symbol; 0: no symbol
;               negatif : only symbols...
;	farr:   1/0 for fill/unfill of the symbols sarr
;
;	NOTE:	All vectors must be the same length
;
; OPTIONAL INPUT PARAMETERS:
;	charsize:	text size (default: 1)
;	charthick:	text thickness (default: 1)
;	thick:		line thickness (default: 1)
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
;		;define vectors to be passed to PEYL_LLEGEND.
;		;
;		tarr=[	'BRW','MLO','SMO','SPO']
;
;		larr=[1,2,3,4]
;
;		carr=[	pen(2),pen(3),pen(4),pen(5)]
;		;
;		;call to PEYL_LLEGEND
;		;
;		PEYL_LLEGEND,	x=.2,y=.8,$
;				tarr=tarr,$
;				larr=larr,$
;				carr=carr,$
;				charthick=2.0,$
;				thick=5.0,$
;				charsize=2.0,$
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
PRO    	PEYL_LLEGEND,	x=x,y=y,$
			tarr=tarr,larr=larr,carr=carr,$
			charsize=charsize,$
			charthick=charthick,$
			thick=thick,$
			frame=frame,$
			sarr=sarr,$
			farr=farr,$
                        incr_text=incr_text,$
                        symsize=symsize,$
                        only_name=only_name,$
                        dx_line = dx_line, $
			data=data
;
;return to caller if an error occurs
;
ON_ERROR,2
;
;
;If keywords are not set then assign
;default values
;
IF NOT CCG_VDEF(thick) THEN thick=replicate(1,(N_ELEMENTS(larr)))
IF NOT CCG_VDEF(charthick) THEN charthick=1
IF NOT CCG_VDEF(charsize) THEN charsize=1
IF NOT CCG_VDEF(carr) THEN carr=MAKE_ARRAY(N_ELEMENTS(larr),/INT,VALUE=255)
IF NOT CCG_VDEF(tarr) THEN tarr=MAKE_ARRAY(N_ELEMENTS(larr),/STR,VALUE='')
IF NOT CCG_VDEF(x) THEN x=0.5
IF NOT CCG_VDEF(y) THEN y=0.5
IF NOT CCG_VDEF(data) THEN data=0
IF NOT CCG_VDEF(sarr) THEN sarr=replicate(0,N_ELEMENTS(larr))
IF NOT CCG_VDEF(farr) THEN farr=replicate(0,N_ELEMENTS(larr))
IF NOT CCG_VDEF(incr_text) THEN incr_text=1.
IF NOT CCG_VDEF(symsize) THEN symsize=1.
IF NOT CCG_VDEF(only_name) THEN only_name=0
IF NOT CCG_VDEF(dx_line) THEN dx_line=-1.
;
;determine number of lines
;
n=N_ELEMENTS(tarr)
;
; Check for problem with thick
if n_elements(thick) lt n then begin
    for k=0,n-1 do thick=[thick,thick(0)]
endif
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
    
    if sarr(i) ne 0 then begin
       ccg_symbol, sym=abs(sarr(i)), fill=farr(i)
       if sarr(i) lt 0 then psym = -8 else psym = 8 
    endif else psym = 0

    if data then begin 
        dx = (!x.crange(1)-!x.crange(0))/15.
        if dx_line ge 0 then dx = dx_line
        if NOT only_name then $
	PLOTS,		[x,x+dx],$
			[y-(yinc*i),y-(yinc*i)],$
			LINESTYLE=larr(i),$
			THICK=thick(i),$
			COLOR=carr(i),$
                        symsize=symsize,$
			PSYM=psym,$
			/DATA
        dxt = dx + (!x.crange(1)-!x.crange(0)) / 35.
	XYOUTS, 	x+dxt,y-(yinc*(i+1./5.)),$
			/DATA,$
			tarr(i),$
			ALI=0,$
			CHARSIZE=charsize,$
			CHARTHICK=charthick,$
			COLOR=carr(i)

    endif else begin
        dx = 0.035*charsize
        if dx_line ge 0 then dx = dx_line
        if NOT only_name then $
	PLOTS,		[x,x+dx],$
			[y-(yinc*i)-(.002*charsize),y-(yinc*i)-(.002*charsize)],$
			LINESTYLE=larr(i),$
			THICK=thick(i),$
			COLOR=carr(i),$
                        symsize=symsize,$
			PSYM=psym,$
			/NORMAL
        dxt = dx + .015*charsize
	XYOUTS, 	x+dxt,y-(yinc*i)-(.007*charsize),$
			/NORMAL,$
			tarr(i),$
			ALI=0,$
			CHARSIZE=charsize,$
			CHARTHICK=charthick,$
			COLOR=carr(i)

endelse

ENDFOR
;
;build legend frame
;
IF CCG_VDEF(frame) THEN BEGIN

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
	PLOTS,	x+(charsize*.04)+(charsize*tlen*.0095)+(charsize*.012),$
		y-(n*.025*charsize),$
		/NORMAL,$
		/CONTINUE
	PLOTS,	x+(charsize*.04)+(charsize*tlen*.0095)+(charsize*.012),$
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



