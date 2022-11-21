;+
; NAME:
;	CCG_READMERGE	
;
; PURPOSE:
; 	Read a CCG merged site file or similarly
;	formatted file.  See CCG_FSMERGE for an 
;	description of how the merged files are 
;	constructed.
;
;	FORMAT=A3,1X,I4,4(1X,I2),1X,A8,1X,1A,1X,F6.2,1X,F7.2,1X,I6,1X,I3,1X,F4.1,
;		<#>(1X,F8.3,1X,A3)
;
;	where <#> is the number of measurement values
;	and associated flags.  
;
;	These files contain ...
;
;	1.  The sample collection details
;		+ 31-character key
;		+ Position information
;		+ Meteorological information
;	2.  Analysis summary for all species
;		+ The mixing/isotope ratio
;		+ The 3-character 'qualifier' flag
;
;	CCG merged files are automatically re-constructed weekly.
;	Returned structure array contains default values when
;	no mixing ratios are found.
;	
;
;	User may suppress messages.
;
; CATEGORY:
;	Text Files.
;
; CALLING SEQUENCE:
;	CCG_READMERGE,file='filename',arr
;	CCG_READMERGE,file='/home/gcarb/data/network/flask/merge/brw.mrg',/nomessages,arr
;
; INPUTS:
;	file:  		Expects CCG network merged file format.
;			If no path is specified with file name then
;			procedure assumes the default merged file directory.
;
; OPTIONAL INPUT PARAMETERS:
;	nomessages:	If non-zero, messages will be suppressed.
;	header:		If non-zero, the procedure will assume first
;			row in file is header information.
;	nvals:		If non-zero, the procedure will assume there
;			are 'nvals' measurement results and flags.  Each
;			row in the file must still have all the data 
;			attributes appearing before the first measurement
;			value.
;
; OUTPUTS:
;	r:		Returned result is a structure array defined as follows:
;
;	r().str 	-> entire site string
;	r().x		-> GMT date/time in decimal year format
;	r().yr		-> sample collection year (GMT)
;	r().mo		-> sample collection month (GMT)
;	r().dy		-> sample collection day (GMT)
;	r().hr		-> sample collection hour (GMT)
;	r().mn		-> sample collection minute (GMT)
;	r().code	-> 3-letter site code
;	r().id		-> 8-character flask id
;	r().type	-> 2-character flask id suffix
;	r().meth	-> single character method
;       r().lat		-> decimal latitude position
;       r().lon		-> decimal longitude position
;       r().alt		-> altitude in m above sea level
;	r().wd		-> wind direction in degrees (0-360)
;	r().ws		-> wind speed (m/s).
;	r().v<#>	-> mixing/isotope ratio
;	r().v<#>flag	-> qualifying flag associated with derived value
;
;       NOTE:   The returned structure name is determined by user.
;
;               Type 'HELP, <structure name>, /str' at IDL prompt for
;               a description of the structure.
;
;	NOTE:	<#> is the number of the measurement value beyond the
;		meteorological parameters (i.e., wind direction and
;		speed).  If the user reads a file from the CCG network
;		then 
;			v1=co2
;			v2=ch4
;			v3=co
;			v4=h2
;			v5=n2o
;			v6=sf6
;			v7=co2c13
;			v8=co2o18
;
;		If the user reads the CSD merged file then v9=co2o17
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Expects a CCG network merged file format.
;
; PROCEDURE:
;	Once the network merge file is read into the structure 'arr', the
;	user can employ SQL-type IDL commands to create subsets
;	of the file.  
;		Example:
;			CCG_READMERGE,file='/home/gcarb/data/network/flask/merge/brw.mrg',arr
;			.
;			.
;			.
;			j=WHERE(arr.v1flag EQ '...' AND arr.v2flag EQ '...')
;			PLOT,arr(j).v1,arr(j).v2,PSYM=4
;			.
;			.
;			.
;		
; MODIFICATION HISTORY:
;	Written, KAM, January 1998.
;-
PRO CCG_READMERGE,	file=file,$
			nomessages=nomessages,$
			header=header,$
			nvals=nvals,$
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
	CCG_MESSAGE,"(ex) CCG_READMERGE,file='/home/gcarb/data/network/flask/merge/brw',arr"
	RETURN
ENDIF
;
;If no directory path is included in 'file'
;input parameter then assume default merged
;directory path.
;
mdir='/home/gcarb/data/network/flask/merge/'
IF STRPOS(file,'/') EQ -1 THEN file=mdir+file
;
IF NOT KEYWORD_SET(nomessages) THEN nomessages=0 ELSE nomessages=1
IF KEYWORD_SET(header) THEN header=1 ELSE header=0
IF NOT KEYWORD_SET(nvals) THEN nvals=7
;
;Determine number of lines in file.
;
nlines=CCG_LIF(file=file)-header
IF nlines LE 0 THEN BEGIN
	arr=0
	RETURN
ENDIF
;
DEFAULT=(-999.999)
WS_DEFAULT=(-9.9)
WD_DEFAULT=(999)
LAT_DEFAULT=(-99.99)
LON_DEFAULT=(-999.99)
ALT_DEFAULT=(-99999)

zz=CREATE_STRUCT('str',	'',		$
	 	'x' ,	0D,             $
		'yr',	0,		$
		'mo',	0,		$
		'dy',	0,		$
		'hr',	0,		$
		'mn',	0,		$
 		'code', '',		$
 		'id',	'',		$ 
 		'type',	',-1',		$ 
 		'meth',	'', 		$ 
 		'inst',	'',		$ 
		'lat',	LAT_DEFAULT,	$
		'lon',	LON_DEFAULT,	$
		'alt',	ALT_DEFAULT,	$
		'wd',	WD_DEFAULT,	$
		'ws',	WS_DEFAULT)

FOR j=0,nvals-1 DO zz=CREATE_STRUCT(TEMPORARY(zz),$
			'v'+STRCOMPRESS(STRING(j+1),/RE),DEFAULT,$
			'v'+STRCOMPRESS(STRING(j+1),/RE)+'flag','')

tagnames=STRLOWCASE(TAG_NAMES(zz))
arr=REPLICATE(zz,nlines)

sformat1='(A3,1X,I4,4(1X,I2),1X,A8,1X,A1,1X,F6.2,1X,F7.2,1X,I6,1X,I3,1X,F4.1)'
sformat2='(1X,F8.3,1X,A3)'

yr=0  & mo=0  & dy=0  & hr=0  & mn=0
st='' & fi='' & me='' & fl='' & in=''
lat=0. & long=0. & alt=0L
wd=0 & ws=0.

v=0. & vf=''

CCG_SREAD,file=file,nomessages=nomessages,skip=header,str
FOR i=0,nlines-1 DO BEGIN

 	READS,str(i),$
 	FORMAT=sformat1,st,yr,mo,dy,hr,mn,fi,me,lat,long,alt,wd,ws

	IF hr EQ 99 THEN hr=12
	IF mn EQ 99 THEN mn=00

	CCG_DATE2DEC,yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=dec
 	arr(i).x=dec
	arr(i).yr=yr
	arr(i).mo=mo
	arr(i).dy=dy
	arr(i).hr=hr
	arr(i).mn=mn
 	arr(i).code=st
    	arr(i).id=fi
 	arr(i).meth=me
 	arr(i).alt=alt
 	arr(i).lat=lat
 	arr(i).lon=long
 	arr(i).ws=ws
 	arr(i).wd=wd
 	k=STRPOS(fi,'-')
        IF k NE -1 THEN arr(i).type=STRMID(fi,k+1,2)

	arr(i).str=STRING(FORMAT=sformat1,st,yr,mo,dy,hr,mn,fi,me,lat,long,alt,wd,ws)

	FOR j=0,nvals-1 DO BEGIN
		z=STRMID(str(i),62+13*j,13)
		arr(i).str=arr(i).str+z
 		READS,z,FORMAT=sformat2,v,vf
                arr(i).(17+j*2)=v
                arr(i).(17+j*2+1)=vf
	ENDFOR
ENDFOR
END 
