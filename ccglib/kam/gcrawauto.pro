PRO 	GCRAWAUTO,$
        dev=dev,$
	sp=sp,$
	site=site,$
	ddir=ddir,$
        noamie=noamie,$
	update=update
;
;Get system date
;
date=SYSTIME()
;
;Parse date into integer yr, mo, dy
;
currentyear=FIX(STRMID(date,20,4))
year=currentyear

currentmonth=STRUPCASE(STRMID(date,4,3))
CCG_MONTH2INT,mon=currentmonth,imon=imon,three=1
currentmonth=imon

currentday=STRUPCASE(STRMID(date,8,2))
currentday=FIX(TEMPORARY(currentday))
;
;Build DOUBLE date
;for comparison with raw
;file names.
;
currentdate=	DOUBLE(currentyear)*10^4.0+DOUBLE(currentmonth)*10^2.0+$
		DOUBLE(currentday)
;
;build up pointer to log file
;
logfile='/projects/'+sp+'/in-situ/'+site+'_data/raw/'+$
	STRCOMPRESS(STRING(year),/REMOVE_ALL)+$
	'/'+STRCOMPRESS(STRING(year),/REMOVE_ALL)+$
	'.log.'+sp

n=CCG_LIF(file=logfile)
IF n EQ -1 THEN BEGIN
	year=currentyear-1
	logfile='/projects/'+sp+'/in-situ/'+site+'_data/raw/'+$
		STRCOMPRESS(STRING(year),/REMOVE_ALL)+$
		'/'+STRCOMPRESS(STRING(year),/REMOVE_ALL)+$
		'.log.'+sp
ENDIF
;
;Read log file
;
CCG_SREAD,file=logfile,arr
;
DAY=0.002739726
;
;What is the date of the 
;last file processed?
;
n=N_ELEMENTS(arr)
yr=FIX(STRMID(arr(n-1),0,4))
mo=FIX(STRMID(arr(n-1),4,2))
dy=FIX(STRMID(arr(n-1),6,2))
CCG_DATE2DEC,yr=yr,mo=mo,dy=dy,dec=dec
;
;Don't process that file again,
;process the next day.
;
dec=TEMPORARY(dec)+DAY
CCG_DEC2DATE,dec,yr,mo,dy
nextdate=DOUBLE(yr)*10^4.0+DOUBLE(mo)*10^2.0+DOUBLE(dy)
;
;Now run gcrawdriver
;
GCRAWDRIVER,	dev=dev,$
		sp=sp,$
		site=site,$
		startdate=nextdate,$
		enddate=currentdate,$
		ddir=ddir,$
		noamie=noamie,$
		update=update
END
