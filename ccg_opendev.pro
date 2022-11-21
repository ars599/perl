;+
; NAME:
;	CCG_OPENDEV
;
; PURPOSE:
; 	Creates a LANDSCAPE or PORTRAIT graphics file according to
;	the passed user selection.  Procedure accounts
;	for color capabilities of selected graphics option.
;
;	This procedure loads a default CCG colormap.  Users may
;	pass a file describing an alternative colormap or may
;	overwrite the default colormap following the call to
;	CCG_OPENDEV.  The IDL routine 'CCG_EX_COLOR' provides a
;	table of colors from the default color map.
;
;	This procedure is intended to be used with CCG_CLOSEDEV.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	CCG_OPENDEV,dev=dev,pen=pen,pcolor=pcolor,win=win,file=file
;	CCG_OPENDEV,dev=dev,pen=pen,portrait=1
;
; INPUTS:
;	dev:	Specifies the requested graphics option.
;		Current options are:
;
;		*** SAVED AS GRAPHICS FILES ***
;
;	hp:		a b/w HPGL file
;	hpc:		a color HPGL file
;	ps:		a b/w POSTSCRIPT file
;	psc:		a color POSTSCRIPT DEVICE file
;	eps:		an ENCAPSULATED POSTSCRIPT file
;	gif:		a GIF file
; 	wds:		a windows terminal
;
;	NOTE:	When a device from the above list is
;		selected, the file is saved as 'idl.[dev]'
;		in the	users HOME directory unless an alternate
;		destination has been specified using the
;		'saveas' keyword.
;
;		*** SENT TO SPECIFIC GRAPHICS DEVICES ***
;
;	hp3:		a b/w POSTSCRIPT file sent to "hp3 (duplex)"
;	lj5:		a b/w POSTSCRIPT file sent to "lj5 (duplex)"
;	lj4v:		a b/w POSTSCRIPT file sent to "lj4v (simplex)"
;	optra:		a b/w POSTSCRIPT file sent to "optra (duplex)"
;	optra_s:	a b/w POSTSCRIPT file sent to "optra (simplex)"
;
;	tek:		a color POSTSCRIPT file sent to the PHASER 140
;	phaser:		a color POSTSCRIPT file sent to 3rd floor PHASER 550
;	phasert:	a color POSTSCRIPT file sent to 3rd floor PHASER 550 (transparency)
;	phaser2:	a color POSTSCRIPT file sent to 2nd floor PHASER 550
;	phaser2t:	a color POSTSCRIPT file sent to 2nd floor PHASER 550 (transparency)
;	phaser840:	a color POSTSCRIPT file sent to the PHASER 840 (duplex)
;	phaser840_s:	a color POSTSCRIPT file sent to the PHASER 840 (simplex)
;	phaser840t:	a color POSTSCRIPT file sent to the PHASER 840 (transparency)
;
;	culj5:		a color POSTSCRIPT file sent to the SIL color printer (RL1)
;	culj4000:	a b/w POSTSCRIPT file sent to the SIL b/w printer (RL1)
;
; OPTIONAL INPUT PARAMETERS:
;	backstore:
;		If this keyword is set then backstoring
;		will be maintained by IDL instead of
;		the windowing environment.  This option may
;		be required for some IDL sessions running
;		in an emulated X-window environment.
;	colormap:
;		Set this keyword to specify the name of a text
;		file that defines a colormap.  This file should
;		be constructed with the CCG_RGBSAVE procedure.
;	iconify:
;		This keyword applies to the IDL X-window only.
;		If set then the IDL X-window is opened but
;		set as an icon.
;	landscape:
;		If set to one (1) then graphics device will be
;		set to landscape.  Default.
;	portrait:
;		If set to one (1) then graphics device will be
;		set to portrait.
;	pcolor:
;		Array corresponding to names of colors
;		available for the selected graphics type.
;	saveas:
;		Save the graphics file in a user-provided file
;		name. saveas must also be passed to CCG_CLOSEDEV.
;	title:
;		Set this keyword to specify the name of the IDL
;		graphics window.  If not specified, the window
;		is given a label of the form "IDL n" where n is
;		the index number of the window.
;       win:
;		Set this keyword to specify graphics window.
;               If set to -1 then the IDL graphics window will
;               NOT be opened.  The default is set to win=0, e.g.,
;               IDL's default window.
;	xpixels:
;		Number of pixels in 'x' direction of IDL
;		X-window display.
;	ypixels:
;		Number of pixels in 'y' direction of IDL
;		X-window display.
;
; OUTPUTS:
;
;	Pen:
;		Array corresponding to the RGB color indices
;		set by the default color map.
;
;	Pcolor:
;		String vector containing a partial list of color names.
;
;		Note:  Users may use this procedure and choose to
;		overwrite the default colormap.  This is accomplished
;		by a first calling CCG_OPENDEV and then overwriting
;		the current colormap with another map.  However, the
;		'pen' vector may yield un-expected results.
;
;		The IDL routine 'CCG_EX_COLOR' provides a table of
;		colors from the default color map.
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
;	Example implementation:
;
;		PRO example, dev=dev
;		.
;		.
;		.
;		CCG_OPENDEV,dev=dev,pen=pen,pcolor=pcolor
;		.
;		.
;		PLOT,x,y,PSYM=4,COLOR=pen(5)
;		XYOUTS,xnot,ynot,'test',COLOR=pen(2)
;		.
;		.
;		.
;		CCG_CLOSEDEV,dev=dev
;		END
;
;	From the IDL command line, the
;	procedure can be executed as
;	follows:
;
;	IDL>example			<- sends graphics to X-window
;	IDL>example,dev="hp3"		<- sends graphics to b/w PostScript
;	IDL>example,dev="phaser"	<- sends graphics to color PHASER 550
;
;
; MODIFICATION HISTORY:
;	Written, KAM, April 1993.
;	Modified, KAM, November 1995.
;	Modified, KAM, March 1996.
;	Modified, KAM, October 1996.
;	Modified, KAM, September 1997.
;	Modified, KAM, June 1998.
;	Modified, KAM, December 1998.
;	Modified, KAM, March 1999.
;-
PRO 	CCG_OPENDEV,	dev=dev,pen=pen,pcolor=pcolor,$
			saveas=saveas,win=win,$
			colormap=colormap,$
			backstore=backstore,$
			landscape=landscape,$
			portrait=portrait,$
			title=title,$
			iconify=iconify,$
			yoffset=yoffset,ysize=ysize,$
			xoffset=xoffset,xsize=xsize,$
			xpixels=xpixels,ypixels=ypixels
;
;----------------------------------------------- begin set up plot device
;
;!P.MULTI=0
IF NOT KEYWORD_SET(win) THEN win=0
IF NOT KEYWORD_SET(portrait) THEN BEGIN
	portrait=0
	landscape=1
ENDIF ELSE BEGIN
	portrait=1
	landscape=0
ENDELSE

;
;PC users Keep this
;
dir=!DIR+'\lib\ccglib\'
;;IF NOT KEYWORD_SET(colormap) THEN colormap=dir+'data\color_comb1'
;
;Cooment this
;
dir='/flurry/home/fdelage/LIB/idl/'
IF NOT KEYWORD_SET(colormap) THEN colormap=dir+'data/color_comb1'

total_colors_available=256
;
;Define colors for various devices.
;
bwrgb=MAKE_ARRAY(total_colors_available,/INT,VALUE=1)
bwcol=MAKE_ARRAY(total_colors_available,/STR,VALUE="BLACK")

hprgb=[-1,1,2,3,4,5,6,7]
hpcol=["N/A","P1","P2","P3","P4","P5","P6","P7"]
n=N_ELEMENTS(hprgb)
hprgb=[hprgb,INDGEN(total_colors_available-n)+n]
hpcol=[hpcol,MAKE_ARRAY(total_colors_available-n,/STR,VALUE="UNDEFINED")]

psrgb=[-1,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
pscol=["N/A","BLACK","RED","CYAN","GREEN",$
	  "BLUE","MAGENTA","PURPLE 1","GRAY",$
	  "LIME","CINNAMON","ORANGE","PURPLE 2",$
	  "YELLOW","WHITE"]
n=N_ELEMENTS(pscol)
psrgb=[psrgb,INDGEN(total_colors_available-n)+n]
pscol=[pscol,MAKE_ARRAY(total_colors_available-n,/STR,VALUE="UNDEFINED")]

xwrgb=[-1,14,2,3,4,5,6,7,8,9,10,11,12,13,1]
xwcol=["N/A","WHITE","RED","CYAN","GREEN",$
	  "BLUE","MAGENTA","PURPLE 1","GRAY",$
	  "LIME","CINNAMON","ORANGE","PURPLE 2",$
	  "YELLOW","BLACK"]
n=N_ELEMENTS(xwcol)
xwrgb=[xwrgb,INDGEN(total_colors_available-n)+n]
xwcol=[xwcol,MAKE_ARRAY(total_colors_available-n,/STR,VALUE="UNDEFINED")]
;
IF KEYWORD_SET(dev) NE 0 THEN BEGIN
	;
	;units are cm
	;
	IF NOT KEYWORD_SET(ysize) THEN ysize=17.75*landscape+22*portrait
	IF NOT KEYWORD_SET(xoffset) THEN xoffset=2*landscape+2*portrait

	IF NOT KEYWORD_SET(yoffset) THEN yoffset=26*landscape+3*portrait
	IF NOT KEYWORD_SET(xsize) THEN xsize=24.25*landscape+18*portrait
	;
	;What is 'saveas' file status?
	;
	IF NOT KEYWORD_SET(saveas) THEN BEGIN
        z=(dev EQ 'psc') ? 'ps' : dev
        saveas=GETENV("HOME")+'/idl.'+z
	ENDIF

	CASE dev OF
	'wds': $
	    BEGIN
		SET_PLOT, 'WIN'
		DEVICE, DECOMPOSED=0, SET_FONT='Helvetica', /TT_FONT
		IF win NE -1 THEN BEGIN
		  IF KEYWORD_SET(title) THEN $
		       WINDOW,win,title=title,xsize=xpixels,ysize=ypixels,retain=retain $
		  ELSE WINDOW,win,xsize=xpixels,ysize=ypixels,retain=retain
		  ERASE
		  ;;WSHOW,iconic=iconify
	    ENDIF
	    pen=xwrgb
		pcolor=xwcol
		tek_color
		; Match color indices to colors we want to use:
		black=0 & white=1 & red=2
		green=3 & dk_blue=4 & lt_blue=5
		END
	'hp':$
		BEGIN
		SET_PLOT, 'HP'
		DEVICE,	FILENAME=saveas,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			EJECT=1,$
			LANDSCAPE=landscape,$
			PORTRAIT=portrait
		pen=bwrgb
		pcolor=bwcol
		END
	'hpc':$
		BEGIN
		SET_PLOT, 'HP'
		DEVICE, FILENAME=saveas,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			EJECT=1
		pen=hprgb
		pcolor=hpcol
		END
	'psc':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
 			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		;CCG_RGBLOAD,file=colormap
		END
	'pl':$
		BEGIN
		SET_PLOT, 'HP'
		DEVICE, FILENAME=saveas,$
			EJECT=1,$
			LANDSCAPE=landscape,$
			PORTRAIT=portrait
		pen=hprgb
		pcolor=hpcol
		END
	'tek':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phaser':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phasert':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phaser2':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phaser2t':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phaser840':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phaser840_s':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'phaser840t':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'culj5':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			PORTRAIT=portrait,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			BITS=8
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'eps':$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas, /COLOR,$
 			YOFFSET=yoffset, YSIZE=ysize, $
 			XOFFSET=xoffset, XSIZE=xsize, $
 			LANDSCAPE=landscape,$
 			PORTRAIT=portrait,$
			BITS_PER_PIXEL=8,$
			ENCAPSULATED=1,$
			PREVIEW=0
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	'gif':$
		BEGIN
		SET_PLOT, 'Z'
		DEVICE,	SET_RESOLUTION=[900*landscape+670*portrait,$
		                        670*landscape+900*portrait],$
; 		DEVICE,	SET_RESOLUTION=[600*landscape+460*portrait,$
; 		                        460*landscape+600*portrait],$
			Z_BUFFERING=0
		pen=psrgb
		pcolor=pscol
		CCG_RGBLOAD,file=colormap
		END
	ELSE:$
		BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME=saveas,$
			YOFFSET=yoffset, YSIZE=ysize, $
			XOFFSET=xoffset, XSIZE=xsize, $
			LANDSCAPE=landscape,$
			ENCAPSULATED=0,$
			PREVIEW=0,$
			PORTRAIT=portrait
		pen=bwrgb
		pcolor=bwcol
		END
	ENDCASE
ENDIF ELSE BEGIN
	IF KEYWORD_SET(backstore) THEN retain=2 ELSE retain=1
	IF NOT KEYWORD_SET(iconify) THEN iconify=0 ELSE iconify=1
	;
	;units are pixels
	;
	IF NOT KEYWORD_SET(ypixels) THEN ypixels=512*landscape+640*portrait
	IF NOT KEYWORD_SET(xpixels) THEN xpixels=640*landscape+512*portrait
	;
 	SET_PLOT,'X'
	DEVICE, decomposed=0
	IF win NE -1 THEN BEGIN
		IF KEYWORD_SET(title) THEN $
		    WINDOW,win,title=title,xsize=xpixels,ysize=ypixels,$
			retain=retain $
		ELSE $
		    WINDOW,win,xsize=xpixels,ysize=ypixels,retain=retain
		ERASE
		WSHOW,iconic=iconify
	ENDIF
	pen=xwrgb
	pcolor=xwcol
	CCG_RGBLOAD,file=colormap
ENDELSE
END
