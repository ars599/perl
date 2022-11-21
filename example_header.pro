;========================================================================
; NAME:
;	CCG_CCGVU	
;
; PURPOSE:
;	Perform a fit to passed x and y arrays using the curve fitting
;	techniques developed by the NOAA/CMDL Carbon Cycle Group and
;	documented in 
;
;		Thoning, K.W., P.P. Tans, and W.D. Komhyr,
;		Atmospheric carbon dioxide at Mauna Loa Observatory, 2,
;		Analysis of the NOAA/GMCC data, 1974-1985,
;		J. Geophys. Res., 94, 8549-8565, 1989.
;
;	This procedure calls a 'c' driver which calls the Fortran filter
;	routines developed by Mr. Kirk Thoning of the Carbon Cycle Group.
;
;	WARNING:
;		Outcome depends on most input keywords.
;
; CATEGORY:
;	Models.
;
; CALLING SEQUENCE:
;	CCG_CCGVU,      x=x,y=y,$
;			npoly=npoly,nharm=nharm,tzero=tzero,$
;			interval=interval,cutoff1=cutoff1,cutoff2=cutoff2,$
;			even=even,$
;			ftn=ftn,sc=sc,tr=tr,gr=gr,$
;			fsc=fsc,ssc=ssc,$
;			residf=residf,residsc=residsc,$
;			coef=coef,$
;			phase=phase,$
;			predfile=predfile,$
;			summary=summary
;
; INPUTS:
;    x:	 	Array of abscissa values.  This vector must have dates in
;		decimal notation, e.g., 1996.011234 or 96.011234.
;
;    y:	 	Array of ordinate values.  This vector contains values
;		to be fitted.
;
; OPTIONAL INPUT PARAMETERS:
;    even:     	If specified then returned arrays (except residuals) will have
;		evenly spaced values beginning at the first data value and 
;		stepping one interval or time-step as specified with keyword
;		'x'.  If not specified, selected return arrays have identical 
;		resolution to source data.
;
;    npoly:    	This keyword is used to specify the number of polynomial
;		terms used in the function f(t).  Note that a default value of 
;		three (3) is used if the source file contains more than two
;		(2) years of data, and a value of two (2) is assigned if less
;		than two (2) years of data are supplied.   Keep in mind that
;		neither default may be appropiate for the supplied data.
;
;    nharm:    	This keyword is used to specify the number of harmonic terms
;		used in the function f(t).  Note that a default value of 
;		four (4) is used .  Keep in mind that this default assignment
;		may not be appropiate for the supplied data.
;
;    interval: 	This keyword is used to specify the resolution or time-step 
;		interval (in days) of the supplied data.  Note that a default
;		value of seven (7) days is assigned if none is specified.
;		Keep in mind that this default assignment may not be appropiate
;		for the supplied data.
;
;    cutoff1:  	This keyword is used to specify the short term filter cutoff 
;		used in constructing the smooth curve S(t).  Note that a 
;		default value of eighty (80) is assigned if none is specified.
;		Keep in mind that this default assignment may not be appropiate
;		for the supplied data.
;
;    cutoff2:  	This keyword is used to specify the long term filter cutoff 
;		used in constructing the smooth trend curve T(t).  Note that a 
;		default value of 667 is assigned if none is specified.  Keep
;		in mind that this default assignment may not be appropiate
;		for the supplied data.
;
;    tzero:    	This keyword is used to specify the date (in decimal notation
;		at t=0.  Note that if none is specified then the decimal date
;		at t=0 is set to be the minimum decimal date in the supplied
;		data file.
;
; predfile:    	This keyword names a file that contains a single column of
;		time steps (in decimal years) from which values will be predicted
;		(extracted) from the various fits.  When specified, this procedure 
;		will add 6 columns of predicted values from S(t), f(t), T(t) dT/dt,
;		harmonic components of f(t), and S(t)-T(t) to the input file.
;
; OUTPUTS:
;    ftn:	This keyword is used to capture the function, f(t), fitted
;		to the passed 'x' and 'y' arrays.  The returned array contains
;		two (2) columns.  The first column contains the date in decimal
;		notation and the second column contains values of the function
;		determined at each step.
;
;    sc:	This keyword is used to capture the smooth curve, S(t), fitted
;		to the passed 'x' and 'y' arrays.  The returned array contains
;		two (2) columns.  The first column contains the date in decimal
;		notation and the second column contains values of the smooth
;		curve determined at each step.
;
;    tr:	This keyword is used to capture the smooth trend, T(t).
;		The returned array contains two (2) columns.  The first column
;		contains the date in decimal notation and the second column
;		contains values of the smooth trend curve determined at each
;		time step.
;
;    gr:	This keyword is used to capture the growth rate, dT/dt.
;		The returned array contains two (2) columns.  The first column
;		contains the date in decimal notation and the second column
;		contains values of the growth rate determined at each time step.
;
;    fsc:      	This keyword is used to capture the harmonic component of f(t).
;		The returned array contains two (2) columns.  The first column
;		contains the date in decimal notation and the second column
;		contains values of the harmonic components of f(t) determined
;		at each time step.
;
;    ssc:      	This keyword is used to capture the smooth seasonal cycle,
;		S(t)-T(t).  The returned array contains two (2) columns.  The
;		first column contains the date in decimal notation and the
;		second column contains values of the smooth seasonal cycle
;		determined at each time step.
;
;    residf:   	This keyword is used to capture the function residuals,
;		c(t)-f(t).  The returned array will contain two (2) columns.
;		The first column contains the date in decimal notation and
;		the second column contains values of the function residuals
;		determined at each data value time step.
;
;    residsc:  	This keyword is used to capture the smooth curve residuals,
;		c(t)-S(t).  The returned array contains two (2)	columns.  The
;		first column contains the date in decimal notation and the
;		second column contains values of the smooth curve residuals
;		determined at each data value time step.
;
;    coef:	This keyword is used to capture the function f(t) coefficients 
;		and their uncertainities as defined by Thoning et al.  The
;		returned array contains two (2)	columns.  The first column 
;		contains the coefficient of each term in the function f(t)
;		and the second column contains values of the coefficient
;		uncertainty.  The number of rows in the matrix is determined
;		by the values of npoly and nharm, e.g., rows=npoly+2*nharm.
;
;    phase:	This keyword is used to capture the amplitude and phase of
;		the harmonic terms.  The returned array contains two (2) 
;		columns.  The first column contains the amplitude of the
;		harmonic (sine plus cosine) and the second collumn contains 
;		the phase in degrees.  The number of rows in the matrix is
;		determined by the value 'nharm', e.g., rows=nhars/2.
;
;    summary:  	This keyword is used to produce a summary array.  The 
;		returned string array summarizes all parameters used
;		in the curve fit.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	'x' vector must have decimal date notation, 
;	e.g., 1996.011234 or 96.011234.
;
; PROCEDURE:
;	Example:
;		.
;		.
;		.
;		CCG_FLASKAVE,sp='co2',site='bme',xret,yret
;		;
;	        CCG_CCGVU,$
;			x=xret,y=yret,$
;			npoly=3,nharm=4,$
;			interval=7,cutoff1=80,cutoff2=650,$
;			even=1,$
;			sc=sc,tr=tr,gr=gr,$
;			fsc=fsc,ssc=ssc,$
;			residf=residf,residsc,residsc
;		;
;		CCG_SYMBOL,sym=1,fill=0
;
;		PLOT,	xret,yret,PSYM=8,COLOR=pen(2)
;		OPLOT,	sc(0,*),sc(1,*),LINESTYLE=0,COLOR=pen(3)
;		OPLOT,	tr(0,*),tr(1,*),LINESTYLE=1,COLOR=pen(4)
;		.
;		.
;		.
;		END
;		
;		
; MODIFICATION HISTORY:
;	Written, KAM, January 1996.
;	Modified, KAM, December 1997.
;	Modified, KAM, June 1998.
;
;========================================================================
PRO CCG_CCGVU, x=x,y=y,$
               npoly=npoly,nharm=nharm,tzero=tzero,$
               interval=interval,cutoff1=cutoff1,cutoff2=cutoff2,$
               even=even,$
               ftn=ftn,sc=sc,tr=tr,gr=gr,$
               fsc=fsc,ssc=ssc,$
               residf=residf,residsc=residsc,$
               coef=coef,$
               phase=phase,$
               predfile=predfile,$
               summary=summary


;;;;;;;; 
; code not include...
;;;;;;

END
