;+
; NAME:
;	CCG_DEC2DATE
;
; PURPOSE:
;	Convert date from decimal date to year, 
;	month, day, and hour.  If passed decimal
;	date is an array then returned variables
;	are arrays of equal size.  Leap year is
;	considered.
;
; CATEGORY:
;	Miscellaneous.
;
; CALLING SEQUENCE:
;	CCG_DEC2DATE,	decimal,yr,mo,dy,hr,mn
;
; INPUTS:
;	decimal:  decimal date array (double precision).
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;	Returns year, month, day, hour, and minute
;	array.  Array size is same as passed
;	decimal date array. 
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	If decimal date contains
;	minute information, the procedure
;	may return a time that is affected
;	by hardware precision.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;
;	Example:
;		IDL> CCG_DEC2DATE,	92.068534,yr,mo,dy,hr
;		IDL> PRINT,yr,mo,dy,hr
;		IDL> 92 01 26 02
;
;		IDL> CCG_DEC2DATE,	1984.161202,yr,mo,dy,hr,mn
;
;		IDL> CCG_DEC2DATE,	decarr,yarr,marr,darr,harr
;
; MODIFICATION HISTORY:
;	Written, September 1993 - kam.
;	Modified, July 1997 - kam.
;-
PRO		CCG_DEC2DATE,	decimal,yr,mo,dy,hr,mn
;
;************************************************
;
;Return to caller if an error occurs
;
;ON_ERROR,2
;
daytab = [[ -9, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ], $
	  [ -9, 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ]]
;
n=N_ELEMENTS(decimal)
;
yr=MAKE_ARRAY(n,/INT)
mo=MAKE_ARRAY(n,/INT)
dy=MAKE_ARRAY(n,/INT)
hr=MAKE_ARRAY(n,/INT)
mn=MAKE_ARRAY(n,/INT)
;
FOR i=0L,n-1 DO BEGIN
	tyr = FIX(decimal(i))
	;
	leap=(tyr MOD 4 EQ 0 AND tyr MOD 100 NE 0) OR tyr MOD 400 EQ 0
	;
	fract_date=decimal(i)-tyr	
	fhoy=fract_date*((365.+leap)*24.)
	ihoy=FIX(fhoy)
	mn(i)=CCG_ROUND(((fhoy-ihoy)*60.),0)

	IF mn(i) EQ 60 THEN BEGIN
		mn(i)=0 & ihoy=ihoy+1
	ENDIF

	doy=(ihoy/24.)+1.
	;
	tmo=0
	REPEAT tmo=tmo+1 UNTIL (tmo EQ 13 OR daytab(tmo,leap) GE FIX(doy))
	tmo=tmo-1
	tdy=doy-daytab(tmo,leap)
	thr=ihoy MOD 24.
	;
	yr(i)=tyr
	mo(i)=tmo
	dy(i)=tdy
	hr(i)=thr
ENDFOR
IF n EQ 1 THEN BEGIN
	yr=yr(0)
	mo=mo(0)
	dy=dy(0)
	hr=hr(0)
	mn=mn(0)
ENDIF ELSE BEGIN
	yr=yr(0:i-1)
	mo=mo(0:i-1)
	dy=dy(0:i-1)
	hr=hr(0:i-1)
	mn=mn(0:i-1)
ENDELSE
END
