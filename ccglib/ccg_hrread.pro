;+
; NAME:
;	CCG_HRREAD	
;
; PURPOSE:
; 	Read a CCG files containing in situ hourly average data.
;
; CATEGORY:
;	Text Files.
;
; CALLING SEQUENCE:
;	CCG_HRREAD,file='filename',arr
;	CCG_HRREAD,file='/home/gcarb/data/co/in-situ/brw_data/brw1996.co',/nomessages,arr
;	CCG_HRREAD,file='~joe/hr.dat',structarr,/nodefault
;
; INPUTS:
;	Filename:  File must conform to CCG hourly average format.
;		   (ex)  BRW 1996 01 01 08  1859.35 ..
;
; OPTIONAL INPUT PARAMETERS:
;	nomessages:	If non-zero, messages will be suppressed.
;	nodefault:	If non-zero, DEFAULT values will be excluded from the 
;			returned structure vector.
;
; OUTPUTS:
;	Data:   Type: anonymous structure
;
;		arr().code	-> site code
;		arr().x		-> GMT date/time in decimal format
;		arr().yr	-> sample year
;		arr().mo	-> sample month
;		arr().dy	-> sample day
;               arr().hr        -> sample hour
;               arr().y         -> mixing ratio
;               arr().flag      -> selection flag (if present)
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Expects a CCG in situ hourly average file format.
;
; PROCEDURE:
;	Once the hourly average file is read into the structure 'arr', the
;	user can employ SQL-type IDL commands to create subsets
;	of the file.  
;		Example:
;			CCG_HRREAD,file='/home/gcarb/data/co2/in-situ/brw_data/brw1996',arr
;			.
;			.
;			.
;			PLOT,arr.x,arr.y
;			.
;			.
;
;		
; MODIFICATION HISTORY:
;	Written, KAM, June 1998.
;-
PRO CCG_HRREAD,	file=file,$
		nomessages=nomessages,$
		name_struct=name_struct,$
		nodefault=nodefault,$
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
	CCG_FATALERR,"(ex) CCG_HRREAD,file='/home/gcarb/data/co2/brw_data/brw1996',arr"
ENDIF
;
IF NOT KEYWORD_SET(nomessages) THEN nomessages=0 ELSE nomessages=1
IF NOT KEYWORD_SET(nodefault) THEN nodefault=0 ELSE nodefault=1
;
;
;Does file exist?
;Determine number of lines in file.
;
nlines=CCG_LIF(file=file)
IF nlines LE 0 THEN BEGIN
        arr=0
        RETURN
ENDIF

DEFAULT=(-999.999)

RESTORE,!DIR+'/lib/ccglib/data/ccg_insitu_hr_template'

IF NOT KEYWORD_SET(nomessages) THEN CCG_MESSAGE,'Reading '+file+' ...'
q=READ_ASCII(file,template=ccg_insitu_hr_template)

IF KEYWORD_SET(name_struct) THEN BEGIN
	arr=REPLICATE(CREATE_STRUCT(name='ccgg_insitu',	'str','',$
					'code','',$
					'flag','.',$
					'x',0.0D,$
					'y',0.0,$
					'yr',0,$
					'mo',0,$
					'dy',0,$
					'hr',0),$
					nlines)
ENDIF ELSE BEGIN
	arr=REPLICATE(CREATE_STRUCT('str','',$
					'code','',$
					'flag','.',$
					'x',0.0D,$
					'y',0.0,$
					'yr',0,$
					'mo',0,$
					'dy',0,$
					'hr',0),$
					nlines)
ENDELSE

arr(*).code=q.code
arr(*).yr=q.yr
arr(*).mo=q.mo
arr(*).dy=q.dy
arr(*).hr=q.hr
arr(*).y=q.mr
arr(*).flag=q.flag

j=WHERE(STRLEN(arr.flag) NE 1)
IF j[0] NE -1 THEN arr[j].flag='.'

CCG_DATE2DEC,yr=q.yr,mo=q.mo,dy=q.dy,hr=q.hr,dec=dec
arr(*).x=dec
;
;Exclude default values?
;
arr=(KEYWORD_SET(nodefault)) ? arr(WHERE(arr.y-1 GT DEFAULT)) : arr

IF NOT KEYWORD_SET(nomessages) THEN CCG_MESSAGE,'Done reading '+file+' ...'
END
