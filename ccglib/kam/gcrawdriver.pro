PRO	GCRAWDRIVER_HELP
	CCG_MESSAGE,STRING(10B)+"GCRAWDRIVER"+STRING(10B)
	CCG_MESSAGE,"This procedure processes multiple MLO GC in situ raw"
	CCG_MESSAGE,"files for CH4, CO, and H2.  The procedure calls the IDL"
	CCG_MESSAGE,"procedure 'GCRAW'."
	CCG_MESSAGE,STRING(10B)+"Arguments to GCRAWDRIVER:"+STRING(10B)
	CCG_MESSAGE,"startdate:	Required start date/file, e.g., 19951201."
	CCG_MESSAGE,"enddate:	Required end date/file, e.g., 19951220."
	CCG_MESSAGE,"site:	Required, but at present, default to 'mlo'."
	CCG_MESSAGE,"sp:	Required, must be 'co', or 'h2', or 'ch4'."
	CCG_MESSAGE,"update:	If set to one (1), then update month and year files."
	CCG_MESSAGE,"ddir:	Save update files to non-default directory." 
	CCG_MESSAGE,"noamie:	If set to one (1), AMIE will NOT be invoked for CH4."
	CCG_MESSAGE,"dev:	Send graphics output to specified device."
	CCG_MESSAGE,"nograph:	If set to one (1), there will be no graphics."
	CCG_MESSAGE,STRING(10B)+"Examples:"+STRING(10B)
	CCG_MESSAGE,"GCRAWDRIVER,startdate=19951208,enddate=19951220,site='mlo',sp='ch4',update=1"
	CCG_MESSAGE,"GCRAWDRIVER,startdate=19951208,enddate=19951220,site='mlo',sp='ch4',update=1,nograph=1"
	CCG_MESSAGE,"GCRAWDRIVER,startdate=19951208,enddate=19951209,site='mlo',sp='co',update=1,dev='lj3si'"
END

PRO 	GCRAWDRIVER,$
	help,$
        dev=dev,$
	sp=sp,$
	site=site,$
	startdate=startdate,$
	enddate=enddate,$
	nograph=nograph,$
	ddir=ddir,$
        noamie=noamie,$
	update=update
;
; This procedure will run gcraw.pro on multiple files
; located in a single directory.
; 19 dec 1995, pml & kam
;
;Check help status
;
IF N_PARAMS() THEN BEGIN
        GCRAWDRIVER_HELP
        RETURN
ENDIF

IF NOT KEYWORD_SET (site) THEN BEGIN
	CCG_MESSAGE,"The site must be specified, i.e., site='mlo'"
	RETURN
ENDIF
	
IF NOT KEYWORD_SET (sp) THEN BEGIN
	CCG_MESSAGE,"The species must be specified, i.e., sp='ch4'"
	RETURN
ENDIF

IF NOT KEYWORD_SET (startdate) THEN BEGIN
	CCG_MESSAGE,"The startdate must be specified:"
	CCG_MESSAGE,"i.e., startdate=19951201"
	RETURN
ENDIF

IF NOT KEYWORD_SET (enddate) THEN BEGIN
	CCG_MESSAGE,"The enddate must be specified:" 
	CCG_MESSAGE,"i.e., enddate=19951220"
	RETURN
ENDIF

fyear=STRMID(STRCOMPRESS(STRING(startdate),/REMOVE_ALL),0,4)
lyear=STRMID(STRCOMPRESS(STRING(enddate),/REMOVE_ALL),0,4)

sp=STRLOWCASE(sp)
site=STRLOWCASE(site)

rawdir='/projects/'+sp+'/in-situ/'+site+'_data/raw/'+fyear+'/'

CCG_DIRLIST,	dir=rawdir+'*.'+sp,omitdir=1,list

FOR i=0,N_ELEMENTS(list)-1 DO BEGIN
	f=LONG(STRMID(list(i),0,STRPOS(list(i),'.')))
	IF f GE startdate AND f LE enddate THEN BEGIN
		GCRAW,	$
			dev=dev,$
			sp=sp,$
 			site=site,$
			file=list(i),$
			ddir=ddir,$
			noamie=noamie,$
			nograph=nograph,$
			update=update
	ENDIF
ENDFOR
END
