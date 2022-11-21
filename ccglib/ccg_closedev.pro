;+
; NAME:
;	CCG_CLOSEDEV
;
; PURPOSE:
; 	Closes a graphics file type specified by 
;	the passed user selection.  Procedure accounts 
;	for color capabilities of selected graphics option.
;	This procedure is intended to be used with CCG_OPENDEV.
;
;	See CCG_OPENDEV for details.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	CCG_CLOSEDEV,dev=dev
;
; INPUTS:
;	dev:	Specifies the requested graphics option.
;		Current options are:
;
;		*** SAVED AS GRAPHICS FILES ***
;
;	hp:	a b/w HPGL file
;	hpc:	a color HPGL file
;	ps:	a b/w POSTSCRIPT file
;	psc:	a color POSTSCRIPT DEVICE file
;	eps:	an ENCAPSULATED POSTSCRIPT file
;	gif:	a GIF file
;
;	NOTE:	When a device from the above list is 
;		selected, the file is saved as 'idl.[dev]' 
;		in the user's HOME directory unless an alternate
;		destination has been specified using the
;		'saveas' keyword.
;
;		*** SENT TO SPECIFIC GRAPHICS DEVICES ***
;
;
;       hp3:            a b/w POSTSCRIPT file sent to "hp3 (duplex)"
;       lj5:            a b/w POSTSCRIPT file sent to "lj5 (duplex)"
;       lj4v:           a b/w POSTSCRIPT file sent to "lj4v (simplex)"
;       optra:          a b/w POSTSCRIPT file sent to "optra (duplex)"
;       optra_s:        a b/w POSTSCRIPT file sent to "optra (simplex)"
;
;       tek:            a color POSTSCRIPT file sent to the PHASER 140
;       phaser:         a color POSTSCRIPT file sent to the 3rd floor PHASER 550
;       phasert:        a color POSTSCRIPT file sent to the 3rd floor PHASER 550 (transparency)
;       phaser2:        a color POSTSCRIPT file sent to the 2nd floor PHASER 550
;       phaser2t:       a color POSTSCRIPT file sent to the 2nd floor PHASER 550 (transparency)
;       phaser840:      a color POSTSCRIPT file sent to the PHASER 840 (duplex)
;       phaser840_s:    a color POSTSCRIPT file sent to the PHASER 840 (simplex)
;       phaser840t:     a color POSTSCRIPT file sent to the PHASER 840 (transparency)
;
;       culj5:          a color POSTSCRIPT file sent to the SIL color printer (RL1)
;       culj4000:       a b/w POSTSCRIPT file sent to the SIL b/w printer (RL1)
;	
; OPTIONAL INPUT PARAMETERS:
;	saveas:	Destination graphics file.  If not specified
;		then the default idl.[dev] file is placed in 
;		the user's home directory.
;
;       nomessages:     If non-zero, messages will be suppressed.
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
;	Must be used with CCG_OPENDEV
;
; PROCEDURE:
;	Example implementation:
;
;		PRO example, dev=dev
;		.
;		.
;		.
;		CCG_OPENDEV,dev=dev,pen=pen,saveas='graphicsfile'
;		.
;		.
;		PLOT,x,y,PSYM=4,COLOR=pen(5)
;		XYOUTS,xnot,ynot,'test',COLOR=pen(2)
;		.
;		.
;		.
;		CCG_CLOSEDEV,dev=dev,saveas='graphicsfile'
;		END
;
;	From the IDL command line, the
;	example procedure can be executed
;	as follows:
;	
;	IDL>example			<- sends graphics to X-window
;	IDL>example,dev="phaser"	<- sends graphics to color paintjet
;	IDL>example,dev='hp3'		<- sends graphics to b/w laserjet
;	IDL>example,dev="psc"		<- sends graphics to color postscript file
;
;		
; MODIFICATION HISTORY:
;	Written, KAM, April 1993.
;	Modified, KAM, November 1995.
;	Modified, KAM, October 1996.
;	Modified, KAM, June 1998.
;	Modified, KAM, March 1999.
;-
PRO 	CCG_CLOSEDEV,	dev=dev,$
			saveas=saveas, $
                        nomessages=nomessages
;
;------------------------------------------------ begin close plot device 
;
IF NOT KEYWORD_SET(nomessages) THEN nomessages=0
IF KEYWORD_SET(dev) THEN BEGIN

	remove=1
	IF NOT KEYWORD_SET(saveas) THEN BEGIN
		z=(dev EQ 'psc') ? 'ps' : dev
		saveas=GETENV("HOME")+'/idl.'+z
	ENDIF

	IF dev EQ 'gif' THEN BEGIN
		a=TVRD()
		a(WHERE(a EQ 0B)) = 255B
		TVLCT,  r,g,b,/GET
		write_gif,saveas,a,r,g,b
	ENDIF

	DEVICE, /CLOSE

	CASE dev OF
	'hp':		BEGIN
			IF NOT nomessages THEN CCG_MESSAGE,'File stored as '+saveas+' (b&w)'
			remove=0
			END
	'hpc':		BEGIN
			IF NOT nomessages THEN CCG_MESSAGE,'File stored as '+saveas+' (color)'
			remove=0
			END
	'ps':		BEGIN
			IF NOT nomessages THEN CCG_MESSAGE,'File stored as '+saveas+' (b&w)'
			remove=0
			END
	'psc':		BEGIN
			IF NOT nomessages THEN CCG_MESSAGE,'File stored as '+saveas+' (color)'
			remove=0
			END
	'eps':		BEGIN
			IF NOT nomessages THEN CCG_MESSAGE,'File stored as '+saveas+' (color)'
			remove=0
			END
	'gif':		BEGIN
			IF NOT nomessages THEN CCG_MESSAGE,'File stored as '+saveas+' (color)'
			remove=0
			END
	'hp3':		SPAWN, 'lp -dhp3 -onb '+saveas
	'lj4v':		SPAWN, 'lp -dlj4v -onb '+saveas
	'lj5':		SPAWN, 'lp -dlj5 -onb '+saveas
	'optra':	SPAWN, 'lp -doptra -onb '+saveas
	'optra_s':	SPAWN, 'lp -doptra -onb -ond '+saveas
	'phaser':	SPAWN, 'lp -dphaser '+saveas
	'phasert':	SPAWN, 'lp -dphasert '+saveas
	'phaser2':	SPAWN, 'lp -dphaser2 '+saveas
	'phaser2t':	SPAWN, 'lp -dphaser2t '+saveas
	'phaser840':	SPAWN, 'lp -dphaser840 '+saveas
	'phaser840_s':	SPAWN, 'lp -dphaser840 -oduplex_none '+saveas
	'phaser840t':	SPAWN, 'lp -dphaser840 -otrans -duplex_none '+saveas
	'tek': 		SPAWN, 'lp -dtek '+saveas
	'culj5': 	SPAWN, 'lp -dculj5 '+saveas
	'culj4000': 	SPAWN, 'lp -dculj4000 '+saveas
	ELSE:		CCG_MESSAGE,'Unknown device '+dev+'.'
	ENDCASE
	;
	;Remove file?
	;
	IF remove THEN SPAWN, 'rm -f '+saveas
	;
	;Set graphics to 'X'
	;
	SET_PLOT,	'X'
ENDIF
END
