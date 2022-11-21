;+
; NAME:
;	CCG_NSREAD	
;
; PURPOSE:
; 	Read a CCG network site file.
;	Network site files contain the sampling 
;	history of every CCG flask sampled. 
;	The content of a sampling history is described below.
;
;	User may suppress messages.
;
; CATEGORY:
;	Text Files.
;
; CALLING SEQUENCE:
;	CCG_NSREAD,file='filename',arr
;	CCG_NSREAD,file='/home/gcarb/data/network/flask/site/brw',/nomessages,arr
;	CCG_NSREAD,file='/users/ken/mlo.temp',structarr
;
; INPUTS:
;	Filename:  Expects CCG network site file format.
;
; OPTIONAL INPUT PARAMETERS:
;	nomessages:	If non-zero, messages will be suppressed.
;
; OUTPUTS:
;	Data:   This array is of type 'ccgnetwork' structure:
;
;		arr().str 	-> entire site string
;		arr().x		-> sample date/time (GMT) in decimal format
;		arr().yr	-> sample year
;		arr().mo	-> sample month
;		arr().dy	-> sample day
;		arr().hr	-> sample hour
;		arr().mn	-> sample minute
;		arr().code	-> 3-letter site code
;		arr().id	-> 8-character flask id
;		arr().type	-> 2-character flask id suffix
;		arr().meth	-> single character method
;               arr().lat       -> decimal latitude position
;               arr().lon       -> decimal longitude position
;               arr().alt       -> altitude in m above sea level
;		arr().wd	-> wind direction in degrees (0-360)
;		arr().ws	-> wind speed (m/s).
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Expects a CCG network site file format.
;
; PROCEDURE:
;	Once the network site file is read into the structure 'arr', the
;	user can employ SQL-type IDL commands to create subsets
;	of the network site file.  
;		Example:
;			CCG_NSREAD,file='.../network/flask/site/brw',arr
;			.
;			.
;			.
;			n_butyl=arr(WHERE(arr.meth EQ 'N' AND arr.type EQ '60'))
;			.
;			.
;			CCG_SWRITE,file='/users/ken/temp.co2',n_butyl.str
;
;		
; MODIFICATION HISTORY:
;	Written, KAM, March 1996.
;-
PRO CCG_NSREAD,	file=file,$
		nomessages=nomessages,$
		arr
;
;Return to caller if an error occurs
;
ON_ERROR,	2
;
;-----------------------------------------------check input information 
;
IF NOT KEYWORD_SET(file) THEN BEGIN
	CCG_MESSAGE,"Source file name must be specified.  Exiting ..."
	CCG_MESSAGE,"(ex) CCG_NSREAD,file='.../network/flask/site/brw',arr"
	RETURN
ENDIF
;
IF NOT KEYWORD_SET(nomessages) THEN nomessages=0 else nomessages=1
;
;Determine number of lines in file.
;
nlines=CCG_LIF(file=file)
IF nlines LE 0 THEN BEGIN
	arr=0
	RETURN
ENDIF
;
WS_DEFAULT=(-9.9)
WD_DEFAULT=(999)
LAT_DEFAULT=(-99.99)
LON_DEFAULT=(-999.99)
ALT_DEFAULT=(-99999)
;
arr=REPLICATE({	ccgnetwork,			$		 
	 		str:	'',		$
	 		x: 	0.0D,           $
			yr:	0,		$
			mo:	0,		$
			dy:	0,		$
			hr:	0,		$
			mn:	0,		$
 			code:	'',		$
 		  	id: 	'',		$ 
 		  	type: 	'-1',		$ 
 		 	meth: 	'', 		$ 
 			inst:	'',		$ 
			lat:	LAT_DEFAULT,	$
			lon:	LON_DEFAULT,	$
			alt:	ALT_DEFAULT,	$
			wd:	WD_DEFAULT,	$
			ws:	WS_DEFAULT},	$
 			nlines)

sformat='(A3,1X,I4.4,4(1X,I2.2),1X,A8,1X,A1,1X,F6.2,1X,F7.2,1X,I6,1X,I3,1X,F4.1)'

yr=0  & mo=0  & dy=0  & hr=0  & mn=0  
st='' & fi='' & me='' & fl='' & in=''
lat=0. & long=0. & alt=0L
wd=0 & ws=0.

OPENR,fpin,file,/GET_LUN
If NOT nomessages THEN CCG_MESSAGE,'Opening '+file+' ...'
FOR i=0,nlines-1 DO BEGIN
	READF,fpin,$
	FORMAT=sformat,$
		st,yr,mo,dy,hr,mn,fi,me,lat,long,alt,wd,ws

	hr2=hr
	mn2=mn
	IF hr EQ 99 THEN hr2=12
	IF mn EQ 99 THEN mn2=00

	arr(i).str=STRING(FORMAT=sformat,$
		st,yr,mo,dy,hr,mn,fi,me,lat,long,alt,wd,ws)

	CCG_DATE2DEC,yr=yr,mo=mo,dy=dy,hr=hr2,mn=mn2,dec=dec
	arr[i].yr=yr
	arr[i].mo=mo
	arr[i].dy=dy
	arr[i].hr=hr
	arr[i].mn=mn
	arr(i).x=dec
	arr(i).code=st
	arr(i).id=fi
	arr(i).meth=me
	IF alt-1 GT ALT_DEFAULT THEN arr(i).alt=alt
	IF lat-1 GT LAT_DEFAULT THEN arr(i).lat=lat
	IF long-1 GT LON_DEFAULT THEN arr(i).lon=long
	IF ws-1 GT WS_DEFAULT THEN arr(i).ws=ws
	IF wd+1 LT WD_DEFAULT THEN arr(i).wd=wd
	k=STRPOS(fi,'-')
        IF k NE -1 THEN arr(i).type=STRMID(fi,k+1,2)
ENDFOR
If NOT nomessages THEN CCG_MESSAGE,'Done reading '+file+' ...'
FREE_LUN,fpin
END
