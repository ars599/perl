;+
; NAME:
;	CCG_FILTER
;
; PURPOSE:
;	This IDL procedure constructs a 'selected' or filtered resultant
;	data set based on iterative curve fits to the supplied data values
;	(file) using techniques described by
;
;               Thoning, K.W., P.P. Tans, and W.D. Komhyr,
;               Atmospheric carbon dioxide at Mauna Loa Observatory, 2,
;               Analysis of the NOAA/GMCC data, 1974-1985,
;               J. Geophys. Res., 94, 8549-8565, 1989.
;
;	This procedure calls a 'c' driver which calls the Fortran filter
;	routines developed by Mr. Kirk Thoning of the Carbon Cycle Group.
;
;	The selected record is achieved when an iteration results in no
;	residuals lying outside a tolerance band specified by the user.
;	The tolerance is defined as [sigmafactor * 1 standard deviation of the
;	residuals about the smooth curve, S(t)], where 'sigmafactor' is set by
;	the user.  Values lying outside the tolerance band are flagged as not
;	representative of background conditions and removed from subsequent fits.
;	Fits continue until there are no further values lying outside the specified
;	filter tolerance.
;
;	Legend Description:
;
;	SQUARE (cyan)		->	Retained values.
;	PLUS   (green)		->	Pre-existing second column flag that 
;					was not previously assigned by this 
;					IDL procedure.
;	ASTERISK (red)		->	Hard rejection (first column) flag.  
;					Values are plotted but not used in
;					fits.
;	PLUS (magenta)		->	Values that have been determined (by this 
;					IDL procedure) to be not-representative of 
;					the distribution (based on sigma_factor). 
;	SQUARE (magenta)	->	Values that were considered to be not-
;					representative of the distribution in one
;					of the curve fit iterations but fell within
;					the tolerance band during the last iteration.
;					In other words, these values have been 
;					re-introduced into the retained data set.
;
;       WARNING:
;               Outcome depends on most input keywords.
;		The keyword 'interval' should reflect the
;		sample frequency of the source record.
;
;		Data values that have been filtered during the iterative fitting 
;		process, but fall within sigma_factor * 1 standard deviation of
;		the final iteration are re-introduced into the retained data set.  
;		These values are plotted as retained values (different color) in 
;		the final iteration plot, but were not used during the last 
;		iterative fit.
;
;		See CCG_CCGVU for further details on curve fitting parameters.
;
; CATEGORY:
;	Models.
;
;  CALLING SEQUENCE:
;	CCG_FILTER,	sp='ch4',$
;			open='/home/gcarb/data/ch4/flask/site/cgo.ch4',$
;			group='noaa',$
;			saveas='/users/ken/cgo_sel.ch4',$
;			summary='/users/ken/cgo_sel_sum.ch4',$
;			sigmafactor=3,$
;			dev='tek'
;
;	CCG_FILTER,	sp='c13co2',$
;			import='/users/ken/tmp/cri.c13co2',$
;			nc=3,$
;			skip=10
;
;      	CCG_FILTER,     sp='ch4',$
;			open='/atmos/crc/b/gaslab/ch4/flask/fixed/cga.ch4',$
;			group='csiro',$
;			saveas='/crc/b/kam/cga_sel.ch4',$
;			summary='/crc/b/kam/cga_sel_sum.ch4',$
;			sigmafactor=3,$
;			dev='phaser-lps'
;
; INPUTS:
;	sp:		Specification of trace gas (or isotope) identifier.
;			This string keyword must be specified.  See CCG_SPDEFS 
;			for the	current list of recognized species.
;
;	open:		This keyword is used to name a CCG 'site' files.
;
;       group:          This keyword must be set to either 'noaa' or 'csiro'
;                       if using the 'open' keyword.  Default:  'noaa'
;
;	import:		Data files that have column format such that the first 
;			two columns contain decimal date and measurement values 
;			may be imported.  Columns must contain real numbers and
;			no blank entries.   The number of columns 'nc' in the file 
;			(if more than 2) and the number of header lines that 
;			should be skipped 'skip' (if more than 0) should also 
;			be specified.  See below.
;
; OPTIONAL INPUT PARAMETERS:
;
;	nc:             Number of columns in the import file.
;                       Columns must contain integers or real numbers.
;			Columns beyond the first two are ignored.
;			Default:  nc=2.
;
;       skip:           Integer specifying the number
;                       of lines to skip at the beginning
;                       of the file. Default:  skip=0.
;
;	sigmafactor:	This keyword sets the tolerance window used in determining
;			if values are representative of the general population.  The
;			tolerance is defined as 'sigmafactor' times the residual
;			standard deviation (RSD) about the smooth curve, S(t).  
;			Residuals lying outside the tolerance window are flagged 
;			as not representative of background conditions and excluded 
;			from  subsequent iterations.  NOTE:  Each subsequent curve 
;			fit produces a smaller tolerance window as RSD continues to
;			be minimized.  The process continues until there are no 
;			further values lying outside the specified tolerance window.
;			Default:  sigmafactor=3.0
;
;	saveas:		String constant specifying pathname where resulting 
;			filtered record should be saved.  If original file
;			was 'open'ed then resulting file will contain identical
;			format.  Values determined to be not representative of
;			background conditions will be assigned a single flag to
;			the second column.  All pre-existing flags will remain
;			intact except for those flags previously assigned by this
;			procedure.
;
;			If the original file was 'import'ed then only the first
;			two columns of the original file will be saved.  An
;			attached third column will indicate which values have
;			been flagged by this procedure.  The flags are described
;			as 
;
;			Zero (0) - sample is representative of background
;				   conditions.
;
;			One (1)  - sample is NOT representative of background
;				   conditions.
;
;	summary:   	String constant specifying the pathname where the filter summary
;			should be saved.  The summary includes a list of all 
;			parameters used in the curve fit as well as relevant 
;			information pertaining to each iteration.
;
;	show_iter:	Set this keyword to one (1) to display a plot depicting the
;			resulting curve, filter window, and retained and flagged 
;			values used in each iteration.
;
;	nolegend:	Set this keyword to one (1) to suppress the legend. 
;
;	npoly:     	This keyword is used to specify the number of polynomial
;			terms used in the function f(t).  Note that a default value 
;			of three (3) is used if the source file contains more than two
;			(2) years of data, and a value of two (2) is assigned if less
;			than two (2) years of data are supplied.   Keep in mind that
;			neither default may be appropiate for the supplied data.
;
;	nharm:		This keyword is used to specify the number of harmonic terms
;			used in the function f(t).  Note that a default value of 
;			four (4) is used .  Keep in mind that this default assignment
;			may not be appropiate for the supplied data.
;
;	interval:	This keyword is used to specify the resolution or time-step 
;			interval (in days) of the supplied data.  Note that a default
;			value of seven (7) days is assigned if none is specified.
;			Keep in mind that this default assignment may not be 
;			appropiate for the supplied data.
;
;	cutoff1:	This keyword is used to specify the short term filter cutoff 
;			used in constructing the smooth curve S(t).  Note that a 
;			default value of eighty (80) is assigned if none is specified.
;			Keep in mind that this default assignment may not be 
;			appropiate for the supplied data.
;
;	cutoff2:	This keyword is used to specify the long term filter cutoff 
;			used in constructing the smooth trend curve T(t).  Note that 
;			a default value of 667 is assigned if none is specified.  Keep
;			in mind that this default assignment may not be appropiate
;			for the supplied data.
;
;	tzero:		This keyword is used to specify the date (in decimal notation
;			at t=0.  Note that if none is specified then the decimal date
;			at t=0 is set to be the minimum decimal date in the supplied
;			data file.
;
;	nolabid:	Set this keyword to one (1) to suppress the laboratory
;			identification and date stamp.	
;
;	noproid:	Set this keyword to one (1) to suppress the procedure stamp.
;
;	dev:		Set this keyword to the desired output device.  
;			See <lab>_OPENDEV, <lab>_CLOSEDEV for complete description.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Flags previously assigned by this procedure will be overwritten if the source
;	file is 'open'ed.
;
; RESTRICTIONS:
;	See conditions associated with INPUT keywords.
;
; PROCEDURE:
;	Example:
;		.
;		.
;		.
;		CCG_FILTER,	sp='co2',$
;				open='/home/gcarb/data/co2/flask/site/brw.co2',$
;				summary='/users/ken/tmp/brw_sum.co2',$
;				saveas='/users/ken/tmp/brw_sf25.co2',$
;				sigmafactor=2.5,$
;				dev='tek'
;
;		CCG_FLASKAVE,	sp='co2',$
;				sigmafactor=2.5,$
;				file='/users/ken/tmp/brw_sf25.co2',$
;				xret,yret,xnb,ynb
;
;		CCG_SYMBOL,	sym=1,thick=2
;
;		PLOT,	xret,yret,$
;			PSYM=8,$
;			COLOR=pen(3)
;
;		CCG_SYMBOL,	sym=10,thick=2
;
;		PLOT,	xnb,ynb,$
;			PSYM=8,$
;			COLOR=pen(2)
;		.
;		.
;		.
;		END
;
;	Example:
;		.
;		.
;		.
;		CCG_FILTER,	sp='f11',$
;				import='/crc/b/kam/tmp/mlo.f11',$
;				nc=4,$
;				skip=10,$
;				npoly=2,$
;				nharm=2,$
;				interval=14,$
;				sigmafactor=3,$
;				show_iter=1,$
;				saveas='/crc/b/kam/tmp/mlo_sf30.f11'
;				
;		CCG_FREAD,	file='/crc/b/kam/tmp/mlo_sf30.f11',$
;				nc=3,$
;				var
;
;		j=WHERE(var(2,*) EQ 0)
;		IF j(0) NE -1 THEN PLOT, var(0,j),var(1,j),psym=4,color=pen(3)
;
;		j=WHERE(var(2,*) EQ 1)
;		IF j(0) NE -1 THEN PLOT, var(0,j),var(1,j),psym=5,color=pen(2)
;		.
;		.
;		.
;		END
;		
;		
; MODIFICATION HISTORY:
;	Written, June 1996 - kam.
;	Written, January 1997 - kam.
;-
PRO	CCG_FILTER_ARGS

	CCG_MESSAGE,""
	CCG_MESSAGE,"Example 1:"
	CCG_MESSAGE,""
        CCG_MESSAGE,"CCG_FILTER,$"
	CCG_MESSAGE,"sp='ch4',$"
	CCG_MESSAGE,"open='/home/gcarb/data/ch4/flask/site/cmo.ch4',$"
	CCG_MESSAGE,"saveas='/users/ken/tmp/cmo_sf30.ch4',$"
	CCG_MESSAGE,"summary='/users/ken/tmp/cmo_sum.ch4'"
	CCG_MESSAGE,""
	CCG_MESSAGE,"Example 2:"
	CCG_MESSAGE,""
        CCG_MESSAGE,"CCG_FILTER,$"
	CCG_MESSAGE,"sp='co',$"
	CCG_MESSAGE,"open='/home/gcarb/data/co/flask/site/mlo.co'
	CCG_MESSAGE,""
	CCG_MESSAGE,"Example 3:"
	CCG_MESSAGE,""
        CCG_MESSAGE,"CCG_FILTER,$"
	CCG_MESSAGE,"import='/users/ken/tmp/sch.co2'$"
	CCG_MESSAGE,"sp='co2',$"
        CCG_MESSAGE,"nc=5,$"
        CCG_MESSAGE,"sigmafactor=2.5,$"
	CCG_MESSAGE,"saveas='/users/ken/tmp/sch_sf25.co2'"
        CCG_MESSAGE,""
  	CCG_MESSAGE,"Example 4:"
        CCG_MESSAGE,""
        CCG_MESSAGE,"CCG_FILTER,$"
	CCG_MESSAGE,""
        CCG_MESSAGE,"sp='ch4',$"
        CCG_MESSAGE,"open='/atmos/crc/b/gaslab/ch4/flask/fixed/cri.ch4',$"
        CCG_MESSAGE,"group='csiro',$"
        CCG_MESSAGE,"sigmafactor=3.0,$"
        CCG_MESSAGE,"saveas='/crc/b/kam/tmp/cri_sf30.ch4',$"
        CCG_MESSAGE,"summary='/crc/b/kam/tmp/cri_sum.ch4'"
END

PRO 	CCG_FILTER,$

	sp=sp,$

	group=group,$
	open=open,$

	import=import,$
	nc=nc,$
	skip=skip,$

	saveas=saveas,$
	summary=summary,$

	show_iter=show_iter,$
	nolegend=nolegend,$

	npoly=npoly,$
	nharm=nharm,$
	interval=interval,$
	cutoff1=cutoff1,$
	cutoff2=cutoff2,$
	tzero=tzero,$

	sigmafactor=sigmafactor,$

	nolabid=nolabid,$
	noproid=noproid,$
	dev=dev
;
;Check a few critical input parameters
;
;
;-------------------------------------- check species parameter
;
IF NOT KEYWORD_SET(sp) THEN BEGIN
        CCG_MESSAGE,"Species, 'sp', parameter must be set.  Exiting ..."
	CCG_FILTER_ARGS
	RETURN
ENDIF

IF NOT KEYWORD_SET(open) AND NOT KEYWORD_SET(import) THEN BEGIN
        CCG_MESSAGE,"Either 'open' or 'import' must be specified.  Exiting ..."
	CCG_FILTER_ARGS
        RETURN
ENDIF

IF NOT KEYWORD_SET(group) THEN group='noaa' ELSE group=STRLOWCASE(group)
IF KEYWORD_SET(open) AND NOT KEYWORD_SET(group) THEN BEGIN
        CCG_MESSAGE,"'group' must be set when using 'open'.  Exiting ..."
        CCG_FILTER_ARGS
        RETURN
ENDIF
;
;----------------------------------------------- set up plot device 
;
CCG_OPENDEV,dev=dev,pen=pen,win=(-1)
;
;----------------------------------------------- misc initialization 
;
RETAIN=0
REJECT=(-1)
NB=(-2)
;
;Index value assigned to ptr2not if data value 
;has been filtered by an iterative fit. 
;
FILTER=(-3) 
;
;index value assigned to ptr2not if data value
;has been un-filtered, i.e., a data value that
;had been filtered in an earlier iterative fit
;that should be included based on uncertainty in 
;last iterative fit. 
;
UNFILTER=(-4)
symsize=0.8

IF NOT KEYWORD_SET(saveas) THEN saveas=""
IF NOT KEYWORD_SET(show_iter) THEN show_iter=0
IF NOT KEYWORD_SET(nc) THEN nc=2
IF NOT KEYWORD_SET(skip) THEN skip=0
;
;Set plotting parameters
;
CCG_SPDEFS,sp=sp,title=ytitle
;
;Default CCGVU settings
;
IF NOT KEYWORD_SET(npoly) THEN npoly=3
IF NOT KEYWORD_SET(nharm) THEN nharm=4
IF NOT KEYWORD_SET(interval) THEN interval=7
IF NOT KEYWORD_SET(cutoff1) THEN cutoff1=80
IF NOT KEYWORD_SET(cutoff2) THEN cutoff2=667
IF NOT KEYWORD_SET(tzero) THEN tzero=0
IF NOT KEYWORD_SET(sigmafactor) THEN sigmafactor=3.0

IF saveas EQ '2paradox' THEN BEGIN
	;paradoxdir='/atmos/crc/b/gaslab/2paradox/filter/'
	paradoxdir='/tmp/'
	;
	;Overwrite summary location
	;
	file=STRMID(open,CCG_STRRPOS(open,'/')+1,10)
	site=STRMID(file,0,CCG_STRRPOS(file,'.'))
	summary=paradoxdir+site+'_sf'+$
		STRCOMPRESS(STRING(FORMAT='(I2)',sigmafactor*10.),/RE)+$
				'_sum.'+sp
ENDIF

IF KEYWORD_SET(summary) THEN OPENW,fpout,summary,/GET_LUN ELSE fpout=(-1)
;
;----------------------------------------------- read data
;
IF KEYWORD_SET(open) THEN BEGIN

	title=open

        IF group EQ 'csiro' THEN BEGIN
                GAS_FSREAD,file=open,all=1,arr
                nbflag='M'
        ENDIF
        IF group EQ 'noaa' THEN BEGIN
                CCG_FSREAD,file=open,all=1,arr
                nbflag='X'
        ENDIF
	;
	;Maintain a pointer to original data set
	;
	ptr2arr=MAKE_ARRAY(N_ELEMENTS(arr),/INT,VALUE=RETAIN)
	;
	;For this exercise, use only non-rejected samples
	;with second column flag equal to '.' or nbflag 
	;(e.g., the non-background flag previously assigned
	;by the filter routine.
	;
	j=WHERE(STRMID(arr.flag,0,1) EQ '.' AND $
	       (STRMID(arr.flag,1,1) EQ  nbflag OR $
		STRMID(arr.flag,1,1) EQ '.'))
	IF j(0) NE -1 THEN BEGIN
		;
		;Overwrite 'nbflag' flags that were 
		;previously assigned by this procedure.
		;
		z=arr(j).flag	
		STRPUT,z,'..',0
		arr(j).flag=z
		ptr2arr(j)=RETAIN
	ENDIF
	;
	;Keep track of rejected samples
	;
	j=WHERE(STRMID(arr.flag,0,1) NE  '.')
	IF j(0) NE -1 THEN ptr2arr(j)=REJECT
	;
	;Keep track of non-background samples
	;determined by means other than the filter routine.
	;
	j=WHERE(STRMID(arr.flag,0,1) EQ '.' AND $
		STRMID(arr.flag,1,1) NE  nbflag AND $
		STRMID(arr.flag,1,1) NE '.')
	IF j(0) NE -1 THEN ptr2arr(j)=NB
ENDIF ELSE BEGIN
	group='other'
	title=import
	CCG_FREAD,file=import,nc=nc,skip=skip,var
	;
	;build up a structure similar to flask files
	;
	n=N_ELEMENTS(var(0,*))
	arr=REPLICATE({z,x:0.0,y:0.0,str:' '},n)
	arr.x=REFORM(var(0,*))
	arr.y=REFORM(var(1,*))
	;
	;This loop is required because 
	;STRING(arr.x) works only when
	;the size is less than 1024.
	;March 1997 - kam
	;
	FOR i=0,n-1 DO arr(i).str=STRING(FORMAT='(F12.6)',arr(i).x)+$
				STRING(FORMAT='(F12.3)',arr(i).y)
	xnot=REFORM(arr.x)
	ynot=REFORM(arr.y)
	;
	;Maintain a pointer to original data set
	;
	ptr2arr=MAKE_ARRAY(N_ELEMENTS(arr),/INT,VALUE=RETAIN)
ENDELSE
;
;Print general header information
;
PRINTF,fpout,FORMAT='(/,A31,/)','**** CCG_FILTER ****'
PRINTF,fpout,FORMAT='(A23,A)','Input File:  ',title
IF KEYWORD_SET(summary) THEN $
	PRINTF,fpout,FORMAT='(A23,A)','Summary File:  ',summary

istep=1
REPEAT BEGIN
	;
	PRINTF,fpout,FORMAT='(/,A33,/)'$
		,'**** ITERATION  '+STRCOMPRESS(STRING(istep),/RE)+' ****'
	PRINTF,fpout,FORMAT='(A21,A12,/)',$
		'Filter (* 1 sigma):',STRCOMPRESS(STRING(sigmafactor),/RE)
	;
	;Call to CCG_CCGVU
	;
	ptr2ret=WHERE(ptr2arr EQ RETAIN)
	x=arr(ptr2ret).x
	y=arr(ptr2ret).y

	CCG_CCGVU,	x=x,y=y,$
			nharm=nharm,npoly=npoly,interval=interval,$
			cutoff1=cutoff1,cutoff2=cutoff2,$
			even=1,sc=sc,tzero=tzero,$
			sum=filtersum,$
			residf=residf,residsc=residsc
	;
	;Determine STDEV of residuals about smooth curve
	;
	one_sigma=STDEV(residsc(1,*))
	;
	;Which samples lie outside one sigma * sigmafactor?
	;
	j=WHERE(ABS(residsc(1,*)) GE sigmafactor*one_sigma)
	flag=MAKE_ARRAY(N_ELEMENTS(x),/INT,VALUE=0)
	nflag=0
	IF j(0) NE -1 THEN BEGIN
		flag(j)=1
		ptr2arr(ptr2ret(j))=FILTER
		nflag=N_ELEMENTS(j)
	ENDIF
	;
	;Print summary
	;
	FOR i=0,N_ELEMENTS(filtersum)-1 DO PRINTF,fpout,filtersum(i)

	PRINTF,fpout,FORMAT='(A21,A12,/)',$
		'# Points Flagged:',STRCOMPRESS(STRING(nflag),/RE)

	;
	;Determine Y range to be used for all iterations.
	;
	IF istep EQ 1 THEN BEGIN
		j=WHERE(ptr2arr NE REJECT)
		xv=[REFORM(sc(0,*)),REFORM(sc(0,*)),arr(j).x]
		yv=[REFORM(sc(1,*))+one_sigma*sigmafactor,$
			REFORM(sc(1,*))-one_sigma*sigmafactor,arr(j).y]
	ENDIF
	IF show_iter THEN BEGIN	

		IF NOT KEYWORD_SET(dev) THEN BEGIN
			WINDOW,	istep, $
			TITLE='Iter '+STRCOMPRESS(STRING(istep),/RE)
			WSHOW,	istep,/ICONIC
		ENDIF

		PLOT,	xv,yv,$
			/NODATA,$
			CHARSIZE=1.5,$
			CHARTHICK=2.0,$
			COLOR=pen(1),$
			TITLE=title,$
			SUBTITLE='Iteration: '+STRCOMPRESS(STRING(istep),/RE),$
		
			YSTYLE=16,$
			YTITLE=ytitle,$
			YCHARSIZE=1.0,$
			YTHICK=2.0,$

			XSTYLE=16,$
			XCHARSIZE=1.0,$
			XTITLE='YEAR',$
			XTHICK=2.0
	ENDIF
			
	j=WHERE(flag EQ 1)
	IF j(0) NE -1 THEN BEGIN
		IF show_iter THEN BEGIN	
			CCG_SYMBOL,	sym=10,fill=0,thick=2
			OPLOT, 		x(j),y(j),$
					PSYM=8,$
					SYMSIZE=symsize,$
					COLOR=pen(6)
		ENDIF
	ENDIF

	j=WHERE(flag EQ 0)
	IF j(0) NE -1 THEN BEGIN
		IF show_iter THEN BEGIN	
			CCG_SYMBOL,	sym=1,fill=0,thick=2
			OPLOT, 		x(j),y(j),$
					PSYM=8,$
					SYMSIZE=symsize,$
					COLOR=pen(3)
		ENDIF
		x=x(j)
		y=y(j)
	ENDIF

	IF show_iter THEN BEGIN	
		OPLOT,		sc(0,*),sc(1,*)+one_sigma*sigmafactor,$
				LINESTYLE=1,$
				THICK=2.0,$
				COLOR=pen(1)

		OPLOT,		sc(0,*),sc(1,*)-one_sigma*sigmafactor,$
				LINESTYLE=1,$
				THICK=2.0,$
				COLOR=pen(1)

		OPLOT,		sc(0,*),sc(1,*),$
				LINESTYLE=0,$
				THICK=2.0,$
				COLOR=pen(1)

		IF NOT KEYWORD_SET(nolabid) THEN CCG_LABID
		IF NOT KEYWORD_SET(noproid) THEN CCG_PROID
	ENDIF
	istep=istep+1
ENDREP UNTIL nflag EQ 0
;
;Are there any points that have been filtered by
;an earlier iteration, but with the final fit now
;fall within one_sigma*sigma_factor?  If so, re-introduce
;these values into the retained data set.  Plot them
;with a different symbol so that user can understand
;what has happened.
;
j=WHERE(ptr2arr EQ FILTER)
IF j(0) NE -1 THEN BEGIN
	FOR i=0,N_ELEMENTS(j)-1 DO BEGIN
		z=WHERE(sc(0,*) LE arr(j(i)).x)
		z1=z(N_ELEMENTS(z)-1)
		z=WHERE(sc(0,*) GE arr(j(i)).x)
		z2=z(0)

		IF z1 EQ -1 AND z2 NE -1 THEN z1=z2
		IF z2 EQ -1 AND z1 NE -1 THEN z2=z1

		IF z1 NE z2 THEN BEGIN
			m=(sc(1,z2)-sc(1,z1))/(sc(0,z2)-sc(0,z1))
			b=sc(1,z2)-m*sc(0,z2)
			sc_interp=m*arr(j(i)).x+b
		ENDIF ELSE sc_interp=sc(1,z1)
		z=one_sigma*sigmafactor
		IF arr(j(i)).y LT sc_interp+z AND arr(j(i)).y GT sc_interp-z THEN $
			ptr2arr(j(i))=UNFILTER
	ENDFOR
ENDIF

IF NOT KEYWORD_SET(dev) THEN WINDOW,	0, TITLE='CCG_FILTER Summary'

PLOT,		xv,yv,$
		/NODATA,$
		CHARSIZE=1.5,$
		CHARTHICK=2.0,$
		COLOR=pen(1),$
		TITLE=title,$
	
		YSTYLE=16,$
		YTITLE=ytitle,$
		YCHARSIZE=1.0,$
		YTHICK=2.0,$

		XSTYLE=16,$
		XCHARSIZE=1.0,$
		XTITLE='YEAR',$
		XTHICK=2.0
	
j=WHERE(ptr2arr EQ RETAIN)
IF j(0) NE -1 THEN BEGIN 
	CCG_SYMBOL,	sym=1,fill=0,thick=2
	OPLOT, 		[arr(j).x],[arr(j).y],$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=pen(3)
ENDIF

j=WHERE(ptr2arr EQ FILTER)
IF j(0) NE -1 THEN BEGIN
		
	CCG_SYMBOL,	sym=10,fill=0,thick=2
	OPLOT, 		[arr(j).x],[arr(j).y],$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=pen(6)
ENDIF

j=WHERE(ptr2arr EQ UNFILTER)
IF j(0) NE -1 THEN BEGIN
		
	CCG_SYMBOL,	sym=1,fill=0,thick=2
	OPLOT, 		[arr(j).x],[arr(j).y],$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=pen(6)
ENDIF

j=WHERE(ptr2arr EQ REJECT)
IF j(0) NE -1 THEN BEGIN
	CCG_SYMBOL,	sym=11,fill=0,thick=2
	OPLOT, 		[arr(j).x],[arr(j).y],$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=pen(2)
ENDIF

j=WHERE(ptr2arr EQ NB)
IF j(0) NE -1 THEN BEGIN
	CCG_SYMBOL,	sym=10,fill=0,thick=2
	OPLOT, 		[arr(j).x],[arr(j).y],$
			PSYM=8,$
			SYMSIZE=symsize,$
			COLOR=pen(4)
ENDIF

OPLOT,		sc(0,*),sc(1,*)+one_sigma*sigmafactor,$
		LINESTYLE=1,$
		THICK=2.0,$
		COLOR=pen(1)

OPLOT,		sc(0,*),sc(1,*)-one_sigma*sigmafactor,$
		LINESTYLE=1,$
		THICK=2.0,$
		COLOR=pen(1)

OPLOT,		sc(0,*),sc(1,*),$
		LINESTYLE=0,$
		THICK=2.0,$
		COLOR=pen(1)
;
;Include legend?
;
IF NOT KEYWORD_SET(nolegend) THEN 	CCG_SLEGEND,$
					x=0.74,y=0.25,$
					tarr=[	'ret',$
						'ret (un-filtered)',$
						'nb (filter [s.f. = '+$
						STRCOMPRESS(STRING(FORMAT='(F4.1)',$
						sigmafactor),/RE)+'])',$
						'nb (other)',$
						'rej'],$
					sarr=[1,1,10,10,11],$
					carr=[pen(3),pen(6),pen(6),pen(4),pen(2)],$
					charsize=1.0,$
					charthick=2.0,$
					frame=0
					
IF NOT KEYWORD_SET(nolabid) THEN CCG_LABID
IF NOT KEYWORD_SET(noproid) THEN CCG_PROID
;
;Flag original data array
;
;
;Send flagged subset to screen
;
j=WHERE(ptr2arr EQ FILTER)
IF j(0) NE -1 THEN BEGIN
	FOR i=0,N_ELEMENTS(j)-1 DO PRINT,FORMAT='(I4,1X,A)',i+1,arr(j(i)).str
ENDIF
;
;What should be done with output?
;
IF saveas NE '' THEN BEGIN
	IF saveas EQ '2paradox' THEN group=saveas
	CASE group OF
	'other':	BEGIN
			j=WHERE(ptr2arr NE FILTER)
			IF j(0) NE -1 THEN ptr2arr(j)=0
			j=WHERE(ptr2arr EQ FILTER)
			IF j(0) NE -1 THEN ptr2arr(j)=1
			CCG_FWRITE,file=saveas,double=1,nc=3,xnot,ynot,ptr2arr
			END
	'noaa':		BEGIN
			j=WHERE(ptr2arr EQ FILTER)
			IF j(0) NE -1 THEN arr(j).flag='.'+nbflag+STRMID(arr(j).flag,2,1)
			CCG_FSWRITE,file=saveas,sp=sp,arr
			END
	'csiro':	BEGIN
			j=WHERE(ptr2arr EQ FILTER)
			IF j(0) NE -1 THEN arr(j).flag='.'+nbflag+STRMID(arr(j).flag,2,1)
			GAS_FSWRITE,file=saveas,arr
			END
	'2paradox':	BEGIN
			j=WHERE(ptr2arr NE FILTER)
			IF j(0) NE -1 THEN ptr2arr(j)=0
			j=WHERE(ptr2arr EQ FILTER)
			IF j(0) NE -1 THEN ptr2arr(j)=1
			;
			;Build output file name
			;
			site=STRMID(file,0,CCG_STRRPOS(file,'.'))
			saveas=	paradoxdir+site+'_sf'+$
				STRCOMPRESS(STRING(FORMAT='(I2)',sigmafactor*10.),/RE)+$
				'.'+sp
			OPENW,unit,saveas,/GET_LUN
			FOR i=0,N_ELEMENTS(ptr2arr)-1 DO $
				PRINTF,unit,FORMAT='(I6,",",I1)',arr(i).uan,ptr2arr(i)
			FREE_LUN,unit
			END
	ENDCASE
	;
	;Notify user
	;
	CCG_MESSAGE, 'NOTE:  See file '+saveas+' for results ...'
ENDIF
;
;----------------------------------------------- close up shop 
;
IF KEYWORD_SET(summary) THEN FREE_LUN,fpout
CCG_CLOSEDEV,dev=dev
END
