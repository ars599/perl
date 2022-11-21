;+
; NAME:
;	CCG_READINIT	
;
; PURPOSE:
; 	Read a DATA EXTENSION initialization file.
;
;	These init files can be used in a variety of
;	applications.  Each init file is species-dependent
;	and contains a list of sites and their positions, 
;	and acceptable CCGVU curve fitting parameters as 
;	determined by each PI.  Also included are parameters
;	specific to the DATA EXTENSION procedure.
;
;	This procedure calls CCG_SITEDESC
;
; CATEGORY:
;	CCG.
;
; CALLING SEQUENCE:
;	CCG_READINIT,file='init.co2.flask.1997',/nomessages,arr
;	CCG_READINIT,file='/home/gcarb/data/dei/ext/co/results.flask.1997/init.co.flask.1997',a
;
; INPUTS:
;	Filename:  	Expects a DATA EXTENSION-formatted initialization file.
;
; OPTIONAL INPUT PARAMETERS:
;	nomessages:	If non-zero, messages will be suppressed.
;
; OUTPUTS:
;	r:  			Returned result is a structure defined as follows:
;
;	r.sync1			->	Synchronization start date (decimal year).
;					DATA EXTENSION only.
;	r.sync2			->	Synchronization end date (decimal year).
;					DATA EXTENSION only.
;	r.rlm			->	Measurement record minimum length criterium.
;					DATA EXTENSION only.
;	r.fillgap		->	Internal FILL gap criteria (weeks).  Interruptions 
;					in MBL or non-MBL records that exceed 'gap' 
;					weeks will be filled with interpolated values 
;					derived from the data extension procedure.
;					DATA EXTENSION only.
;	r.mblgap		->	Internal MBL gap criteria (weeks).  Interruptions 
;					in MBL records that exceed 'gap' weeks will be
;					filled with interpolated S(t) values only for 
;					the purpose of constructing the reference
;					MBL matrix.
;					DATA EXTENSION only.
;	r.init().str		->	Complete string of parameters by site.
;	r.init().site	        ->	Passed site descriptor.
;	r.init().npoly		->	# of poly terms to be used in 'ccgvu' fit.
;	r.init().nharm		->	#  of harm. terms to be used in 'ccgvu' fit.
;	r.init().interval	->	Interval (in days) used by 'ccgvu' routines.
;	r.init().cutoff1	->	Short-term cutoff used by 'ccgvu' routines.
;	r.init().cutoff2	->	Long-term cutoff used by 'ccgvu' routines.
;
;	r.init().mbl		->	Marine Boundary Layer specification, 
;					1=yes, 0=no.
;
;  **** CCG_SITEDESC RESULTS ****
;
;       r.desc().str            -> "site" string passed
;       r.desc().site_code      -> site code
;       r.desc().site_name      -> site name
;       r.desc().site_type      -> type of sampling site
;                                  e.g., land, aircraft, tower, shipboard
;       r.desc().sample         -> sampling strategy
;                                  e.g., flask, in situ
;       r.desc().sample_code    -> sampling strategy code
;       r.desc().lab_name       -> name of measurement laboratory
;       r.desc().lab_code       -> laboratory identification code
;                                  e.g., 00, 01, 23
;       r.desc().lab_acronym    -> laboratory acronym
;       r.desc().agency         -> cooperating agency
;       r.desc().lat            -> site position degree latitude
;       r.desc().sinlat         -> site position sine of latitude
;       r.desc().long           -> site position degree longitude
;       r.desc().alt            -> site position altitude (masl)
;       r.desc().lst2utc        -> hour conversion from LST to UTC
;
;
;       NOTE:   The returned structure name is determined by user.
;
;               Type 'HELP, <structure name>, /str' at IDL prompt for
;               a description of the structure.
;
;	NOTE:	Elements in the structure arrays r.desc() and r.sites() are matched.
;
;
;	NOTE:	For a complete description of ccgvu curve fitting parameters, see
;			Thoning, K.W., P.P. Tans, and W.D. Komhyr,
;			Atmospheric carbon dioxide at Mauna Loa Observatory, 2,
;			Analysis of the NOAA/GMCC data, 1974-1985,
;			J. Geophys. Res., 94, 8549-8565, 1989.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Expects a DATA EXTENSION-formatted initialization file.
;
; PROCEDURE:
;		Example:
;			CCG_READINIT,file='.../init.co2.flask.1997',arr
;			.
;			.
;			.
;			FOR i=0,N_ELEMENTS(arr)-1 DO BEGIN
;				.
;				.
;				.
;				CCG_FLASKAVE, 	file=file,sp=sp,xret,yret
;
;				CCG_CCGVU, 	x=xret,y=yret,/even,$
;						interval=arr.sites(i).interval,$
;						npoly=arr.sites(i).npoly,$
;						nharm=arr.sites(i).nharm,$
;						cutoff1=arr.sites(i).cutoff1,$
;						cutoff2=arr.sites(i).cutoff2,$
;				.
;				.
;				.
;			ENDFOR
;		
; MODIFICATION HISTORY:
;	Written, KAM, May 1997.
;	Modified, KAM, November 1997.
;	Modified, KAM, June 1999.
;	Modified, KAM, May 2000.
;-
;
PRO	CCG_READINIT,$
	file=file,$
	nomessages=nomessages,$
	result

IF NOT KEYWORD_SET(file) THEN CCG_FATALERR,$
	"Initialization file must be specified, e.g., file='init.co2.flask.1997'."

IF KEYWORD_SET(nomessages) THEN nomessages=1 ELSE nomessages=0
;
;Return to caller if an error occurs
;
ON_ERROR,       2
;
;Misc initialization
;
DEFAULT=(-999.999)
NUMHEADERS=3
headers=STRARR(NUMHEADERS)
UNK='unknown'

ns=CCG_LIF(file=file)-NUMHEADERS

z=REPLICATE({ 		ccg_readinit,$
			str:		UNK,$
			site:	        UNK,$
			npoly:		DEFAULT,$
			nharm:		DEFAULT,$
			interval:	DEFAULT,$
			cutoff1:	DEFAULT,$
			cutoff2:	DEFAULT,$
 			lat: 		DEFAULT,$
 			sinlat:		DEFAULT,$
 			long: 		DEFAULT,$
 			mbl: 		DEFAULT},$
			ns)
;
sync1=0. & sync2=0. & rlm=0. & fillgap=8D & mblgap=48D
;
OPENR,unit,file,/GET_LUN
IF NOT nomessages THEN CCG_MESSAGE,'Reading '+file+' ...'
;
s=''
FOR i=0,NUMHEADERS-1 DO BEGIN
	READF,unit,s
	headers(i)=s
ENDFOR
;
;Does initialization file contain 'gap' information?
;
CCG_STRTOK,str=headers[1],delimiter=' ',s
IF N_ELEMENTS(s) EQ 3 THEN READS,headers(1),sync1,sync2,rlm
IF N_ELEMENTS(s) EQ 4 THEN READS,headers(1),sync1,sync2,rlm,fillgap
IF N_ELEMENTS(s) EQ 5 THEN READS,headers(1),sync1,sync2,rlm,fillgap,mblgap
;
s='' & s1='' & f=FLTARR(6)

FOR i=0,ns-1 DO BEGIN
	READF,unit,s
	CCG_STRTOK,str=s,delimiter=' ',s1

	z(i).str=s
	z(i).site=STRTRIM(s1[0])
	z(i).npoly=FLOAT(s1[1])
	z(i).nharm=FLOAT(s1[2])
	z(i).interval=FLOAT(s1[3])
	z(i).cutoff1=FLOAT(s1[4])
	z(i).cutoff2=FLOAT(s1[5])
	z(i).mbl=FLOAT(s1[6])
ENDFOR
FREE_LUN,unit
IF NOT nomessages THEN CCG_MESSAGE,'Done reading '+file+' ...'
;
;Get site description information
;
CCG_SITEDESC,site=z.site,zz
;
;Create returning structure
;
result=CREATE_STRUCT(	'sync1',sync1,$
			'sync2',sync2,$
			'rlm',rlm,$
			'fillgap',fillgap,$
			'mblgap',mblgap,$
			'init',z,$
			'desc',zz)
END
