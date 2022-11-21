;+
; NAME:
;	CCG_DATE2DEC
;
; PURPOSE:
;	Convert date from yr mo dy hr mn to decimal date. 
;	Leap years are considered.
;
; CATEGORY:
;	Miscellaneous.
;
; CALLING SEQUENCE:
;	CCG_DATE2DEC,	yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=dec
;
; INPUTS:
;	yr:  2-digit year array.
;	mo:  2-digit month array.
;	dy:  2-digit day array.
;	hr:  2-digit hour array.
;	mn:  2-digit minute array.
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;	dec:	Returned decimal date double array
;		of equal size.
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
;
;	Example:
;		IDL> CCG_DATE2DEC,	yr=92,mo=01,dy=26,hr=02,mn=00,dec=dec
;		IDL> PRINT,dec
;		IDL> 92.068534
;
;		IDL> CCG_DATE2DEC,	yr=1984,mo=02,dy=29,dec=dec
;		IDL> PRINT,dec
;		IDL> 1984.162568
;
;		IDL> CCG_DATE2DEC,	yr=yrarr,mo=moarr,dy=dyarr,hr=hrarr,dec=dec
;
; MODIFICATION HISTORY:
;	Written, KAM, April 1993.
;	Modified, KAM, August 1994.
;-
;
PRO 	CCG_DATE2DEC,	yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=dec
;
;**************************************
;
;Return to caller if an error occurs
;
ON_ERROR,2
;
;Check input parameters
;
IF NOT KEYWORD_SET(yr) THEN yr=0
;
n=N_ELEMENTS(yr)
;
IF N_ELEMENTS(mo) EQ 0 THEN mo=MAKE_ARRAY(n,/INT,VALUE=1)
IF N_ELEMENTS(dy) EQ 0 THEN dy=MAKE_ARRAY(n,/INT,VALUE=15)
IF N_ELEMENTS(hr) EQ 0 THEN hr=MAKE_ARRAY(n,/INT,VALUE=12)
IF N_ELEMENTS(mn) EQ 0 THEN mn=MAKE_ARRAY(n,/INT,VALUE=00)
;
diy    = [[-9,0, 31, 59, 90,120,151,181,212,243,273,304,334,365], $
	  [-9,0, 31, 60, 91,121,152,182,213,244,274,305,335,366]]

leap=(yr MOD 4 EQ 0 AND yr MOD 100 NE 0) OR yr MOD 400 EQ 0

tdy=dy

tdy=TEMPORARY(tdy)+diy(mo,leap)

dec=DOUBLE(yr)+((tdy-1)*24.0+hr+(mn/60.0))/((365+leap)*24.0)

IF n EQ 1 THEN dec=dec(0)
IF n GT 1 THEN dec=REFORM(dec,n)
END
