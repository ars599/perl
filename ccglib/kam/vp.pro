PRO		VP,$

		help=help,$
		sp=sp,$
		matchto=matchto,$

		site=site,$

		startdate=startdate,$
		stopdate=stopdate,$

		xmin=xmin,xmax=xmax,$
		ymin=ymin,ymax=ymax,$

        plotthick=plotthick,$
        linethick=linethick,$
        charsize=charsize,$
        charthick=charthick,$
        symsize=symsize,$
		norej=norej,$
		nonb=nonb,$
		row=row,col=col,$
		dev=dev
;
;This procedure is the driver for all vertical profile procedures.
;
;Verify inputs
;
IF KEYWORD_SET(help) THEN get_help=1
IF NOT KEYWORD_SET(site) THEN get_help=1
;
;
;Need help?
;
IF CCG_VDEF(get_help) THEN BEGIN
    SPAWN,'more '+!DIR+'/lib/ccglib/kam/vp_help.txt'
    RETURN
ENDIF

site=STRLOWCASE(site)
;
;-----------------------------------------------check bin status
;
;
;Miscellaneous Initialization
;
DEFAULT=(-999.99)
rootdir='/projects/'
co2=0 & ch4=0 & co=0 & h2=0 & n2o=0 & sf6=0 & co2c13=0 & co2o18=0
list=['co2','ch4','co','h2','n2o','sf6','co2c13','co2o18']
nlist=N_ELEMENTS(list)
IF NOT KEYWORD_SET(landscape) THEN landscape=0
;
;-----------------------------------------------check species status
;
IF NOT KEYWORD_SET(sp) THEN BEGIN
        PRINT,STRING(7B)
        CCG_MESSAGE,"Species, 'sp', array was not set.  Default set to 'all' ..."
        CCG_MESSAGE,"ex:  MSP,sp=['co2','ch4','co'], ..."
        CCG_MESSAGE,"ex:  MSP,sp=['all'],..."
        PRINT,STRING(7B)
        sp=['all']
ENDIF
;
IF sp(0) EQ 'all' THEN sp=list
sp=STRLOWCASE(sp)
nsp=N_ELEMENTS(sp)
;
;Make sure 'matchto' species is available
;
IF KEYWORD_SET(matchto) THEN BEGIN
    j=WHERE(sp EQ matchto)
    IF j(0) EQ -1 THEN BEGIN
    	CCG_MESSAGE,"Specified 'matchto' species not included in 'sp' vector."
	RETURN
    ENDIF
ENDIF ELSE matchto=sp(0)
;

IF NOT KEYWORD_SET(type) THEN type='ssp'

IF KEYWORD_SET(startdate) THEN BEGIN
	startdate=STRCOMPRESS(STRING(startdate),/RE)
	CASE STRLEN(startdate) OF
	4:	startdate=startdate+'-00-00.0000'
	7:	startdate=startdate+'-00.0000'
	10:	startdate=startdate+'.0000'
	15:
	ELSE:   CCG_FATALERR,"Startdate must have 'yyyy-mm-dd.hhmm' format."
	ENDCASE
ENDIF ELSE startdate='0000-00-00.0000'

IF KEYWORD_SET(stopdate) THEN BEGIN
	stopdate=STRCOMPRESS(STRING(stopdate),/RE)
	CASE STRLEN(stopdate) OF
	4:	stopdate=stopdate+'-99-99.9999'
	7:	stopdate=stopdate+'-99.9999'
	10:	stopdate=stopdate+'.9999'
	15:
	ELSE:   CCG_FATALERR,"Stopdate must have 'yyyy-mm-dd.hhmm' format."
	ENDCASE
ENDIF ELSE stopdate='9999-99-99.9999'

IF NOT KEYWORD_SET(xmin) THEN xmin=MAKE_ARRAY(nsp,/INT,VALUE=DEFAULT)
IF NOT KEYWORD_SET(xmax) THEN xmax=MAKE_ARRAY(nsp,/INT,VALUE=DEFAULT)
IF NOT KEYWORD_SET(ymin) THEN ymin=MAKE_ARRAY(nsp,/INT,VALUE=DEFAULT)
IF NOT KEYWORD_SET(ymax) THEN ymax=MAKE_ARRAY(nsp,/INT,VALUE=DEFAULT)
;
;Re-order species vector so that
;'matchto' species is first.
;
IF nsp GT 1 THEN BEGIN
	j=WHERE(sp EQ matchto)
	k=WHERE(sp NE matchto)
	sp=[sp(j),sp(k)]
	xmin=[xmin(j),xmin(k)]
	xmax=[xmax(j),xmax(k)]
	ymin=[ymin(j),ymin(k)]
	ymax=[ymax(j),ymax(k)]
ENDIF

data=CREATE_STRUCT("dummy",0)
;
;Which merged aircraft files should be read?
;
yr1=FIX(STRMID(startdate,0,4))
yr2=FIX(STRMID(stopdate,0,4))
CCG_DIRLIST,dir=rootdir+'aircraft/'+site+'/history',acyears
acyears=STRMID(acyears,0,4)
j=WHERE(acyears GE yr1 AND acyears LE yr2)
IF j[0] NE -1 THEN acyears=acyears[j]
FOR iyr=0,N_ELEMENTS(acyears)-1 DO BEGIN
	sdir=rootdir+'aircraft/'+site+'/history/'+acyears[iyr]+'/'
	CCG_DIRLIST,dir=sdir+'/*.mrg',/omitdir,files
	j=WHERE(files GE startdate+'.mrg' AND files LE stopdate+'.mrg')

	IF j[0] NE -1 THEN files=files[j]
	FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
		CCG_READACMERGE,file=sdir+files[i],a
		;
		;Build tag name
		;
		z=STRMID(files[i],0,STRPOS(files[i],'.mrg'))
		z='z'+STRMID(z,0,4)+'_'+STRMID(z,5,2)+'_'+STRMID(z,8,2)+'_'+STRMID(z,11,4)

		data=CREATE_STRUCT(data,z,a)
	ENDFOR
ENDFOR

CASE type OF 
   'ssp':    $
             BEGIN
             VP_SSP,$
             data=data,$
             sp=sp,$
	     site=site,$
             matchto=matchto,$
		plotthick=plotthick,$
		linethick=linethick,$
		charsize=charsize,$
		charthick=charthick,$
		symsize=symsize,$
             row=row,col=col,$
	     norej=norej,nonb=nonb,$
             dev=dev
	     END
   ELSE:
ENDCASE
END
