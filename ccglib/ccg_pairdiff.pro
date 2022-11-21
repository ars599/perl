;+
; NAME:
;	CCG_PAIRDIFF
;
; PURPOSE:
;	Calculate pair differences for the passed
;	CCG site file and compute percentage of
;	good pairs. 
;
;	Note:  
;
;	This routines has the option to include or exclude rejected
;	samples.  Subset of pairs based on flask type, etc... should 
;	be constructed prior to calling CCG_PAIRDIFF.
;
;	If there exist multiple aliqouts for individual flask members
;	then the difference is computed as the first aliquot from the
;	first flask pair member minus the first aliquot from the second
;	flask member.
;	
; CATEGORY:
;	CCG flasks	
;
; CALLING SEQUENCE:
;	CCG_PAIRDIFF,	sp='ch4',file='/users/ken/sitefile.ch4',diff=diff
;	CCG_PAIRDIFF,	sp='co2',file='/home/gcarb/data/co2/flask/site/cgo.co2',$
;			diff=diff,meth='P',bins=bins,gp=gp
;	CCG_PAIRDIFF,	sp='co2c13',file='/home/gcarb/data/co2c13/flask/site/brw.co2c13',$
;			norej=1,diff=diff
;	CCG_PAIRDIFF,	sp='ch4',file='/home/gcarb/data/ch4/flask/site/brw.ch4',$
;			norej=1,yr1=1990,diff=diff
;
; INPUTS:
;	sp:	Name of trace gas species,
;		ch4,co,co2,co2c13,co2o18,sf6,n2o
;
;	file:	Name of a CCG site file.
;
; OPTIONAL INPUT PARAMETERS:
;	yr1:	Option to evaluate pair 
;		differences for consecutive years.
;		Year should be specified using
;		4-digits, e.g., yr1=1994.  If
;		specified, flasks sampled at or
;		later than 'yr1' will be used in 
;		the pair difference analysis.
;		For a single year, set both 'yr1'
;		and 'yr2' to the same year. 
;
;	yr2:	Option to evaluate pair 
;		differences for consecutive years.
;		Year should be specified using
;		4-digits, e.g., yr2=1994.  If
;		specified, flasks sampled at or
;		before 'yr2' will be used in the 
;		pair difference analysis.
;		For a single year, set both 'yr1'
;		and 'yr2' to the same year. 
;
;	meth:	Single letter code specifying the fill
;		method, e.g., meth='P'.  If specified, 
;		only method 'P' flasks will be used in 
;		the pair difference analysis.
;
;	norej:	If set to one (1) then flasks that have been
;		assigned a hard rejection flag (column 1 flag
;		not equal to '.') are excluded. 
;
;	cond:	Optional float that specifies the 
;		condition for good pairs.  If no
;		condition is specified then species-specific
;		defaults are assigned. 
;
;		default conditions
;
;		cond=0.5	<- co2
;		cond=4.0	<- ch4
;		cond=3.0	<- co
;		cond=10.0	<- h2
;		cond=0.09	<- co2c13
;		cond=0.15	<- co2o18
;		cond=3.0	<- n2o
;		cond=0.5	<- sf6
;
;	h_span:	Specifies the span of the histogram.  For
;		example if h_span=20 and h_res=1.0 then the
;		histogram will span -10 to 10 with steps
;		of 1.  If h_span is not specified, species-
;		specific defaults will be used.
;
;	h_res:	Specifies the bin size of the histogram.  For
;		example if h_span=2 and h_res=0.1 then the
;		histogram will span -1 to 1 with steps
;		of 0.1.  If h_res is not specified, species-
;		specific defaults will be used.
;	
; OUTPUTS:
;
;	diff:	The double precision array containing
;		pair difference results:
;
;		diff(0,*) -> 	Decimal date of each pair.
;		diff(1,*) -> 	Pair difference of the first
;				aliquot of the first member minus
;				the first aliquot of the second member
;				for each pair.
;
;	bins:	Two-dimensional float array containing histogram of pair
;		differences.  The first column contains the bin minimum
;		or maximum bin value in typical species units; the second
;		column is the number of pairs with differences that fall 
;		into this bin.
;
;	gp:	Computed percentage of good pairs based
;		on assigned good pair condition.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	The passed input file must have CCG site file format.
;	
; PROCEDURE:
;	Example:
;
;		PRO example
;		.
;		.
;		.
;		CCG_PAIRDIFF,	sp='co2',file='/home/gcarb/data/co2/flask/site/cgo.co2',$
;				diff=diff,meth='P',gp=gp
;
;		PLOT,		diff(0,*),diff(1,*),PSYM=2
;		.
;		.
;		.
;		END
;		
; MODIFICATION HISTORY:
;	Written, KAM, October 1995.
;	Modified, KAM, April 1998.
;-
;
PRO 	CCG_PAIRDIFF,$
	sp=sp,file=file,$
	diff=diff,meth=meth,$
	h_span=h_span,h_res=h_res,$
	yr1=yr1,$
	yr2=yr2,$
	structarr=arr,$
	norej=norej,$
	bins=bins,gp=gp,$
	cond=cond
;
;-----------------------------------------------check input information 
;
IF (NOT KEYWORD_SET(file) AND NOT KEYWORD_SET(arr)) OR NOT KEYWORD_SET(sp) THEN BEGIN
	CCG_MESSAGE,"File and species must be specified.  Exiting ..."
	CCG_MESSAGE,"(ex) CCG_PAIRDIFF,file='/home/gcarb/data/co2/flask/site/alt.co2',sp='co2',diff=diff,meth=meth
	RETURN
ENDIF
;
;Miscellaneous initialization
;
gp=0 & diff=0 & bins=0
;
;Define span and resolution
;for each species.
;
CASE sp OF
'co2':		BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=2
		IF NOT KEYWORD_SET(h_res) THEN h_res=0.1
		IF NOT KEYWORD_SET(cond) THEN cond=0.5
		END
'ch4':		BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=20
		IF NOT KEYWORD_SET(h_res) THEN h_res=1.0
		IF NOT KEYWORD_SET(cond) THEN cond=4.0
		END
'co':		BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=20
		IF NOT KEYWORD_SET(h_res) THEN h_res=1.0
		IF NOT KEYWORD_SET(cond) THEN cond=3.0
		END
'h2':		BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=20
		IF NOT KEYWORD_SET(h_res) THEN h_res=1.0
		IF NOT KEYWORD_SET(cond) THEN cond=3.0
		END
'co2c13':	BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=0.5
		IF NOT KEYWORD_SET(h_res) THEN h_res=0.01
		IF NOT KEYWORD_SET(cond) THEN cond=0.09
		END
'co2o18':	BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=0.4
		IF NOT KEYWORD_SET(h_res) THEN h_res=0.01
		IF NOT KEYWORD_SET(cond) THEN cond=0.15
		END
'n2o':		BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=20
		IF NOT KEYWORD_SET(h_res) THEN h_res=1.0
		IF NOT KEYWORD_SET(cond) THEN cond=3.0
		END
'sf6':		BEGIN
		IF NOT KEYWORD_SET(h_span) THEN h_span=2
		IF NOT KEYWORD_SET(h_res) THEN h_res=0.1
		IF NOT KEYWORD_SET(cond) THEN cond=0.5
		END
ELSE:   	BEGIN
                CCG_MESSAGE,"Passed Species not recognized.  Exiting..."
                RETURN
                END
ENDCASE
;
;Read flask site file?
;
IF NOT KEYWORD_SET(arr) THEN CCG_FSREAD,file=file,arr
arr=arr(SORT(arr.str))
;
;Create a subset by year(s) if necessary
;
IF KEYWORD_SET(yr1) THEN BEGIN
	j=WHERE(arr.x GE yr1)
	IF j(0) NE -1 THEN arr=arr(j) ELSE RETURN
ENDIF

IF KEYWORD_SET(yr2) THEN BEGIN
	j=WHERE(FIX(arr.x) LE yr2)
	IF j(0) NE -1 THEN arr=arr(j) ELSE RETURN
ENDIF
;
;Create a subset by method if necessary
;
IF KEYWORD_SET(meth) THEN BEGIN
	j=WHERE(arr.meth EQ meth)
	IF j(0) NE -1 THEN arr=arr(j) ELSE RETURN
ENDIF
;
;Create a subset by excluding rejected samples if necessary
;
IF KEYWORD_SET(norej) THEN BEGIN
	f=STRMID(arr.flag,0,1)
	;
	;changed based on conversations with TJC.
	;April 8, 1998 - kam
	;
	;was this ...
	;
	;j=WHERE(f EQ '.' OR f EQ '+' OR f EQ '-' OR f EQ 'P')
	;now this ...
	;
	j=WHERE(f EQ '.')
	IF j(0) NE -1 THEN arr=arr(j) ELSE RETURN
ENDIF
;
;Build flask pair key
;
key=STRMID(arr.str,0,20)+arr.meth
;
;How many unique key values are there?
;
unique=key(UNIQ(key))
nunique=N_ELEMENTS(unique)
;
;Declare return arrays
;
nret=0
diff=DBLARR(2,nunique)
IF NOT KEYWORD_SET(meth) THEN meth=STRARR(nunique)
nbins=1+h_span/h_res
bins=MAKE_ARRAY(2,nbins,/FLOAT,VALUE=0)
bins(0,*)=FINDGEN(nbins)*h_res-h_span/2.0
;
;Loop thru unique key values.
;Subtract first aliquot of first flask
;minus first aliquot from second
;flask.
;
FOR i=0,nunique-1 DO BEGIN
	j=WHERE(key EQ unique(i))
	first_id=arr(j(0)).id
	first_dec=arr(j(0)).x
	first_me=arr(j(0)).meth
	first_mr=arr(j(0)).y
	first_type=arr(j(0)).type
	j=WHERE(key EQ unique(i) AND arr.id NE first_id AND arr.type EQ first_type)
	IF j(0) NE -1 THEN BEGIN
		second_id=arr(j(0)).id
		second_mr=arr(j(0)).y
		;
		;calculate pair differences
		;
		diff(0,nret)=first_dec
		diff(1,nret)=first_mr-second_mr
		IF NOT KEYWORD_SET(meth) THEN meth(nret)=first_me
		;
		;determine the difference bin
		;
		k=WHERE(diff(1,nret) LE bins(0,*))
		IF k(0) NE -1 THEN BEGIN
			IF diff(1,nret) LT 0 THEN BEGIN
				IF NOT k(0) THEN BEGIN
					bins(1,k(0))=TEMPORARY(bins(1,k(0)))+1
				ENDIF ELSE BEGIN
					bins(1,k(0)-1)=TEMPORARY(bins(1,k(0)-1))+1
				ENDELSE
			ENDIF ELSE BEGIN
				bins(1,k(0))=TEMPORARY(bins(1,k(0)))+1
			ENDELSE
		ENDIF ELSE BEGIN
			bins(1,nbins-1)=TEMPORARY(bins(1,nbins-1))+1
		ENDELSE
	nret=TEMPORARY(nret)+1
	ENDIF
ENDFOR
;
;Now determine percent good pairs
;
total=TOTAL(bins(1,*))
j=WHERE(ABS(bins(0,*)) LE cond)
IF total THEN gp=100.0*TOTAL(bins(1,j))/total ELSE gp=0

IF nret GT 1 THEN BEGIN
	diff=diff(*,0:nret-1)
	IF NOT KEYWORD_SET(meth) THEN meth=meth(0:nret-1)
ENDIF ELSE BEGIN
	diff=diff(*,0)
	IF NOT KEYWORD_SET(meth) THEN meth=meth(0)
ENDELSE
END
