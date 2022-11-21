;+
PRO 	PEYL_ODR,$
        dev=dev,$
        plotfile=plotfile,$     ;optional name of ploting file (default = 'odr_plot.ps')
	x=x,$		        ;optional array of x data to be passed in
	y=y,$		        ;optional array of y data to be passed in	
	file=file,$	        ;optional file of x,y data (text bicolomn file : x and y)
	noplot=noplot,$	        ;cancels plot (when calling from outside)
	coeff,$		        ;fitting coefficients and associated errors passed out
	yfit1,$		        ;vector of y-values to be used in plotting odr line
	yfit2,$		        ;vector of y-values to be used in plotting lsq line
	text1,$                 ;text string for equation of odr line
	text2,$                 ;text string for equation of lsq line
        tol=tol,$               ;tolerance for convergence passed to fitexy.pro (default=1e-6)
        r2=r2,$
        s=s,$                   ;outlier criterion in sigma factors (no rejection if not set)
            
        textx=textx,$           ;input text for x title
        texty=texty,$           ;input text for y title

        sigma_x=sigma_x,$       ;common uncertainty in x (default=0.1 ppm)
        sigma_y=sigma_y,$       ;common uncertainty in y (default=0.01 per mil)

        kp=kp,$         ;convert data to keelping plot mode: i.e. 1/co2 v. d13c
        tp=tp,$         ;convert data to 'tans' plot mode: i.e. co2 v. d13c*co2

        noscale1=noscale1,$       ;turns off all error bar scaling
        noscale2=noscale2,$     ;turns off scaling of error bars to produce q=0.5
        reverse=reverse         ;switch x and y and error bars to test for symmetry

;
;Procedure to use Orthogonal Distance Regression (from Num. Recipes)
;to fit lines to any x and y.
;The procedure estimates x and y uncertainty in the data due to 
;natural variability by relating errors to chi-sq probabilities.
;These errors allow for a realistic estimation of slope and intercept
;uncertainties.
;
;P Tans date=?
;modified by JBM 11/6/00,4/9/01
;
;

;Read in text file containing x and y data to be fit
;and create separate arrays, only if x,y data (or
;file) not passed from outside

IF NOT KEYWORD_SET(tol) THEN tol=1D-6

IF NOT KEYWORD_SET(x) OR NOT KEYWORD_SET(y) THEN BEGIN
	
	IF NOT KEYWORD_SET(file) THEN BEGIN	
            print,'Please specify file OR x and y !!!'
            stop
	ENDIF
	
	CCG_FREAD,file=file,/nomessages,nc=2,xydata
	xdata=xydata(0,*)
	ydata=xydata(1,*)

ENDIF ELSE BEGIN
	xdata=x
	ydata=y
ENDELSE

;revert to least squares fit if only 2 data points
;and process output arrays
IF N_ELEMENTS(xdata) EQ 1 THEN BEGIN
	coeff=FLTARR(2,4)
	yfit1=-999.9
	yfit2=-999.9
	text1='-999'
	text2='-999'
ENDIF ELSE IF N_ELEMENTS(xdata) LT 3 THEN BEGIN
	result=POLY_FIT(xdata,ydata,1,yfit)
	;construct return arrays
	coeff=FLTARR(2,4)
	coeff(0,0)=result(0)		;lsq int
	coeff(1,0)=-999.9		;lsq int err
	coeff(0,1)=result(1)		;lsq slope
	coeff(1,1)=-999.9		;lsq slope err
	coeff(0,2)=-999.9		;lsq chi2
	coeff(1,2)=-999.9		;lsq chi2prob
	coeff(0,3)=result(0)		;LSq intercept
	coeff(1,3)=result(1)		;LSq slope

	yfit1=yfit
	yfit2=yfit

	;full  line equations
	text1=STRCOMPRESS('LSQ: y='+STRING(FORMAT='(F9.1)',result(1))+$
		'('+STRING(FORMAT='(F7.2)',coeff(1,1))+')'+'x+'+ $
		STRING(FORMAT='(F7.2)',result(0)),/RE)
	text2=STRCOMPRESS('LSQ: y='+STRING(FORMAT='(F9.1)',coeff(1,3))+$
		'x+'+STRING(FORMAT='(F7.2)',coeff(0,3)),/RE)
	;just slopes and error
	text1=STRCOMPRESS(STRING(FORMAT='(F9.1)',result(1))+$
		'('+STRING(FORMAT='(F7.2)',coeff(1,1))+')',/RE)
	text2=STRCOMPRESS(STRING(FORMAT='(F9.1)',coeff(1,3)),/RE)	

ENDIF ELSE BEGIN

;fitting parameters
	;polynomial degree of fit
	n=1
	;number of fixed parameters
	p=0

IF NOT KEYWORD_SET(sigma_x) OR NOT KEYWORD_SET(sigma_y) THEN BEGIN
	;Estimate initial natural variability uncertainty for x and y data
        print,'WARNING : DEFAULT initial uncertainties in x and y are 0.1 and 0.01 !!!'
	sigma_x=0.1	;for CO2 in ppm
	sigma_y=0.01	;for d13C
ENDIF


IF NOT KEYWORD_SET(textx) THEN textx='x'
IF NOT KEYWORD_SET(texty) THEN texty='y'

IF KEYWORD_SET(kp) THEN BEGIN
        print,'Special conversion to get Keeling plot : x -> 1/x '
        IF NOT KEYWORD_SET(textx) THEN textx='1/x'
        IF NOT KEYWORD_SET(texty) THEN texty='y'
        sigma_x=sigma_x/(MEAN(xdata))^2
        xdata=1./xdata
ENDIF

IF KEYWORD_SET(tp) THEN BEGIN
        print,'Special conversion to get Tans plot : y -> x.y '
        IF NOT KEYWORD_SET(textx) THEN textx='x'
        IF NOT KEYWORD_SET(texty) THEN texty='xy'
  	sigma_y=SQRT((MEAN(xdata)*sigma_y)^2+(MEAN(ydata)*sigma_x)^2)
  	ydata=xdata*ydata
ENDIF


IF KEYWORD_SET(reverse) THEN BEGIN
        xd=xdata
        yd=ydata
        xdata=yd
        ydata=xd
        sx=sigma_x
        sy=sigma_y
        sigma_x=sy
        sigma_y=sx
ENDIF

;Fit initial straight line to CO2 vs C13 (or any x,y)
jj=INDGEN(N_ELEMENTS(xdata))       	
time=INDGEN(N_ELEMENTS(xdata))
fitcf=FINDGEN(2)	;linear fits (Num. Rec.) of C13 vs CO2 etc.
errcf=FINDGEN(2)

;;;IF N_ELEMENTS(jj) GT 2 THEN BEGIN 

		;call fitexy.pro
		PEYL_FITEXY,xdata,ydata,X_SIGMA=sigma_x,Y_SIGMA=sigma_y,TOL=tol,$
		        int,slope,errs,chisquared,chi2prob
  			
  		fitcf(0)=int
  		fitcf(1)=slope
  		errcf(0)=errs[0]
  		errcf(1)=errs[1]
  		
		PRINT, '   slope=',fitcf(1),'    +-',errcf(1)
  		PRINT, '   chisquared=',chisquared,' chi2prob=',chi2prob
		PRINT, '   intercept=',fitcf(0),'    +-',errcf(0)

    IF NOT KEYWORD_SET(noscale1) THEN BEGIN

        ;Iterative loop: Change sigma_x and sigma_y based on the previous
        ;fit (residuals in x- and in y-direction), and re-fit.  If this
        ;iteration converges we end up with a straight line whereby the
        ;squared residual deviations perpendicular to the line have been
        ;minimized.
        ;Note: in that case the ratio of sigma_y to sigma_x equals the slope.
        ;Reject outliers if present, and re-fit curve.
        ;Select accepted data
        ;slope must converge to within 0.01 per cent
  
        iter=1
        ;re-calculate sigmas
        sigma_x=STDEV(xdata-ydata/slope+int/slope)
        sigma_y=STDEV(ydata-xdata*slope-int)

        change=1.     ;Initial value - will be relative change in slope(or int) between iterations

        WHILE iter LT 10 AND ABS(change) GT 0.0001 DO BEGIN
        	;'s'Sigma filter for outliers    
   		;Retained data
                                ;outlier criterion in sigma factors
                IF KEYWORD_SET(s) THEN BEGIN
                    ja=WHERE(ABS(ydata-int-slope*xdata) LT s*sigma_y)

                                ;Rejected data
                    jr=WHERE(ABS(ydata-int-slope*xdata) GE s*sigma_y)
                ENDIF ELSE BEGIN
                    ja = INDGEN(n_elements(xdata))
                    jr = -1 
                ENDELSE

                                ;chisquared=1
    		IF N_ELEMENTS(ja) GT 2 THEN BEGIN
    		      			
      			;call fitexy.pro
		        PEYL_FITEXY,xdata,ydata,X_SIGMA=sigma_x,Y_SIGMA=sigma_y,TOL=tol,$
		                int,slope,errs,chisquared,chi2prob
  			
  		        fitcf(0)=int
  		        fitcf(1)=slope
  		        errcf(0)=errs[0]
  		        errcf(1)=errs[1]
  		
		        PRINT, iter,'   slope=',fitcf(1),'    +-',errcf(1)
  		        PRINT, '   chisquared=',chisquared,' chi2prob=',chi2prob
		        PRINT, '   intercept=',fitcf(0),'    +-',errcf(0)

      			newslope=fitcf(1)     	;output value
      			newint=fitcf(0)		;output value
      			error=errcf(1)     	;output value
      			change=(newslope-slope)/slope
      			;change=(newint-int)/int
      			iter=iter+1
      			slope=newslope
      			int=newint
      			sigma_x=STDEV(xdata(ja)-ydata(ja)/slope+int/slope)
      			sigma_y=STDEV(ydata(ja)-xdata(ja)*slope-int)
      			;stop
    		ENDIF ELSE BEGIN   ;wrt 'IF N_ELEMENTS(ja) gt 2'
      			newslope=-99.99 
      			error=-9.99     
      			change=(newslope-slope)/slope
      			iter=iter+1
      			slope=newslope
      			sigma_x=STDEV(xdata(jr)-ydata(jr)/slope+int/slope)
      			sigma_y=STDEV(ydata(jr)-xdata(jr)*slope-int)
    		ENDELSE
 
	ENDWHILE
  
	IF iter GE 19 THEN BEGIN
    		slope=-99.99
   		error=-9.99
	ENDIF

    IF NOT KEYWORD_SET(noscale2) THEN BEGIN
	;this iterative loop keeps the ratio of sigma-x and sigma-y constant from
	;the previous loop
	;diff is the difference between calculated chi squared probability and 0.5
        ;(50% chi2prob).  The optimization of the error bar size is set to stop when
        ;diff is less than 0.05 (0.45<chi2prob<0.55) or after 10 iterations
        ;
        ;Note that error bars can also be scaled by SQRT(chisquared/(N-2)) and this
        ;yields a final reduced chisquared of 1, which is equivalent to q=0.5 in the
        ;limit of large N

        diff=1.
	iter=1
	WHILE iter LT 10 AND ABS(diff) GT 0.01 DO BEGIN
        	        ;call fitexy.pro
		        PEYL_FITEXY,xdata,ydata,X_SIGMA=sigma_x,Y_SIGMA=sigma_y,TOL=tol,$
		                int,slope,errs,chisquared,chi2prob
  			
  		        fitcf(0)=int
  		        fitcf(1)=slope
  		        errcf(0)=errs[0]
  		        errcf(1)=errs[1]
  		
		        PRINT, iter,'   slope=',fitcf(1),'    +-',errcf(1)
  		        PRINT, '   chisquared=',chisquared,' chi2prob=',chi2prob
		        PRINT, '   intercept=',fitcf(0),'    +-',errcf(0)

      			newslope=fitcf(1)     	;output value
      			newint=fitcf(0)		;output value
      			newerror=errcf(1)
      			;aveerror=(error+newerror)/2
      			error=newerror
      			diff=chi2prob-0.5
      			iter=iter+1
      			;stop
      			sigma_x=sigma_x/(1+0.3*diff)
			sigma_y=sigma_y/(1+0.3*diff)
			;stop
	ENDWHILE
    ENDIF        ;wrt noscale2	
	;IF iter EQ 10 THEN error=aveerror
   ENDIF ELSE BEGIN       ;wrt to 'noscale1'
        ja=INDGEN(N_ELEMENTS(xdata))
        jr=[-1]
        error=errcf[1]
   ENDELSE	

;;;ENDIF         ;wrt 'IF N_ELEMENTS(jj) gt 2'
;endif
;calculate least squares fit coefficients
result2=POLY_FIT(xdata,ydata,1,yfit,SIGMA=errcf2)
r=CORRELATE(xdata,ydata)
r2=r^2
int2=result2[0]
slope2=result2[1]
			
;skip plot if program is called from another program
IF NOT KEYWORD_SET(noplot) THEN BEGIN

;Create plot
IF N_ELEMENTS(jj) GT 2 THEN BEGIN
If NOT KEYWORD_SET(plotfile) then plotfile = 'odr_plot.ps'
CCG_OPENDEV, dev=dev, saveas=plotfile,pen=pen,portrait=0

	;titletext=file
	
PLOT, xdata, ydata, $
        PSYM=4,$
        /NODATA, $
  	;title=titletext, $
  	xtitle=textx, $
  	ytitle=texty, $
  	charsize=1.3,$
  	YSTYLE=16 ;,$
  	;XRANGE=[500,580]


IF ja(0) NE -1 THEN OPLOT, xdata(ja), ydata(ja), PSYM=3

;overplot error bars
CCG_ERRPLOT,	xdata(ja),ydata(ja)-sigma_y,ydata(ja)+sigma_y,COLOR=pen[1]
CCG_ERRPLOT,	ydata(ja),xdata(ja)-sigma_x,xdata(ja)+sigma_x,y=1,COLOR=pen[1]

PRINT,sigma_x
PRINT,sigma_y

;Overplot rejected data IF any
IF jr(0) NE -1 THEN OPLOT,xdata(jr),ydata(jr), PSYM=2

;Overplot the fitted line
np=50   ;# of points in line
xline=MIN(xdata(ja)) + FINDGEN(np)*(MAX(xdata(ja))-MIN(xdata(ja)))/(np-1)
yline=fitcf(0)+fitcf(1)*xline 
OPLOT,xline,yline,THICK=2

;OPLOT,[MIN(xdata(jj)),MAX(xdata(jj))],[0,0]          ;plot axes through (0,0)
;OPLOT,[0,0],[MIN(ydata(jj)),MAX(ydata(jj))]          ;plot axes through (0,0)

;compare with standard least squares fit
OPLOT, xdata,yfit
			
;Write out line fit eqquations on plot
text=STRCOMPRESS('R!U2!N = '+ $
                 STRING(FORMAT='(F7.3)',r2))
XYOUTS,0.15,0.27,/NORMAL,text


text1=STRCOMPRESS('number of points = '+ $
                 STRING(FORMAT='(I4)',N_ELEMENTS(xdata(ja)))) 
;XYOUTS,0.15,0.23,/NORMAL,text1

text2=STRCOMPRESS('ODR line: y=' + STRING(FORMAT='(F9.2)',slope) $
      	+ '(' +STRING(FORMAT='(F4.2)',errcf[1]) + ')x+' + $
      	STRING(FORMAT='(F7.2)',int) + '(' + STRING(FORMAT='(F6.2)',errcf[0]) + ')')
XYOUTS,0.15,0.19,/NORMAL,text2

text3=STRCOMPRESS('LSq line: y=' + STRING(FORMAT='(F9.2)',slope2) $
      	+ '(' +STRING(FORMAT='(F4.2)',errcf2[1]) + ')x+' + $
      	STRING(FORMAT='(F7.2)',int2) + '(' + STRING(FORMAT='(F6.2)',errcf2[0]) + ')')
XYOUTS,0.15,0.15,/NORMAL,text3

CCG_CLOSEDEV, dev=dev,saveas=plotfile
ENDIF

ENDIF		;wrt 'IF NOT KEYWORD_SET(noplot)

;construct return arrays
coeff=DBLARR(2,4)
coeff(0,0)=fitcf(0)		;ODR int
coeff(1,0)=errcf(0)	        ;ODR int err
coeff(0,1)=fitcf(1)		;ODR slope
coeff(1,1)=error		;ODR slope err
coeff(0,2)=chisquared		;ODR chi2
coeff(1,2)=chi2prob		;ODR chi2prob
coeff(0,3)=result2(0)		;LSq intercept
coeff(1,3)=result2(1)		;LSq slope

yfit1=fitcf(0) + fitcf(1)*xdata

yfit2=result2(0) + result2(1)*xdata

;full  line equations
text1=STRCOMPRESS('ODR: y='+STRING(FORMAT='(F9.1)',fitcf(1))+$
	'('+STRING(FORMAT='(F7.2)',error)+')'+'x+'+ $
	STRING(FORMAT='(F7.2)',fitcf(0)),/RE)
text2=STRCOMPRESS('LSQ: y='+STRING(FORMAT='(F9.1)',coeff(1,3))+$
	'x+'+STRING(FORMAT='(F7.2)',coeff(0,3)),/RE)
;just slopes and error
text1=STRCOMPRESS(STRING(FORMAT='(F9.3)',fitcf(1))+$
	'('+STRING(FORMAT='(F7.3)',error)+')',/RE)
text2=STRCOMPRESS(STRING(FORMAT='(F9.3)',coeff(1,3)),/RE)
ENDELSE			;wrt min. data points criteria
;stop
END
;-
