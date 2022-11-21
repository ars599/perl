;+
;---------------------------------------------------------------------------------
; NAME:
;	FILTER.PRO
;
; PROCEDURE
;	This procedure starts the CCGVU calculation for a time serie, 
;       and return the desired component..
;
;--------- Parameters which can be return (DO NOT CHANGE THE ORDER !!)
;  output_list  = [ ['**Original Data',           'orig'   ], $
;                   ['Function',                  'func'   ], $
;                   ['Polynomial',                'poly'   ], $
;                   ['Smooth Curve',              'smooth' ], $
;                   ['Trend Curve',               'trend'  ], $
;                   ['**Detrended Original',      'detrend'], $
;                   ['Smooth Seasonal',           'smcycle'], $
;                   ['Harmonics',                 'harm'   ], $
;                   ['**Residual Function',       'res'    ], $
;                   ['Smoothed Residuals',        'smres'  ], $
;                   ['Trend of Residuals',        'trres'  ], $
;                   ['**Residual Smooth Curve',   'ressm'  ], $
;                   ['Growth Rate',               'gr'     ], $
;                   ['Monthly Mean From smooth',  'mmsm'   ], $
;                   ['Monthly Mean From data',    'mmdata' ], $
;                   ['*Coefficients',             'coef'   ]  ]
;
;----- Order of data in CCGRV (From K.Thoning, DO NOT CHANGE)
;  param_order = [ 'orig','func','poly','smooth','trend','detrend', $
;                  'smcycle','harm','res','smres','trres','ressm','gr' ]
;
; MODIFICATION HISTORY:
; 	Written by:	Michel Ramonet, 14-feb-1996
;	Modified by:    Philippe Peylin, 10-jan-1997
;---------------------------------------------------------------------------------
FUNCTION peyl_fitter, param, input_time, input_val


;-------------------------------------------------------------------------
;		SPECIAL CHECK FOR CONSISTENCY
;
;Monthly Means calculated if out_month>0 (& out_smooth=1 or out_orig=1)
;    out_month = 0 => No monthly means
;    out_month = 1 & out_smooth=1 => Monthly means from smoothed curve
;    out_month = 2 & out_orig = 1 => Monthly means from original data

if param.out_month ne 0 then begin
   print,'Option for monthly mean not available....'
   stop
endif
if param.out_coef ne 0 then begin
   print,'Option for coefficients not available...'
   stop
endif
val_min = MIN(input_val(*))
val_max = MAX(input_val(*))
IF (val_min eq 0.) and (val_max eq 0.) THEN BEGIN
   PRINT,'>>> Bug in input values..  >> max=min=0 !!!'
   STOP
ENDIF 


;-------------------------------------------------------------------------
;		Write the command variable
;		==========================

command = ' -npoly ' +STRCOMPRESS(STRING(param.polyterm),/REMOVE_ALL)+$
          ' -nharm ' +STRCOMPRESS(STRING(param.harmterm),/REMOVE_ALL)+$
          ' -interv '+STRCOMPRESS(STRING(param.interval),/REMOVE_ALL)+$
          ' -short ' +STRCOMPRESS(STRING(param.sfilter) ,/REMOVE_ALL)+$
          ' -long '  +STRCOMPRESS(STRING(param.lfilter) ,/REMOVE_ALL)

IF (param.out_orig eq 1) THEN BEGIN
   command = command + ' -orig'
   param.out_timestep = 0
ENDIF
IF (param.out_detrend eq 1) THEN BEGIN
   command = command + ' -detrend'
   param.out_timestep = 0
ENDIF
IF (param.out_func eq 1) THEN command = command + ' -func'
IF (param.out_poly eq 1) THEN command = command + ' -poly'
IF (param.out_harm eq 1) THEN command = command + ' -harm'
IF (param.out_smooth eq 1) THEN command = command + ' -smooth'
IF (param.out_trend eq 1) THEN command = command + ' -trend'
IF (param.out_smcycle eq 1) THEN command = command + ' -smcycle'
IF (param.out_gr eq 1) THEN command = command + ' -gr'
IF (param.out_res eq 1) THEN BEGIN 
  command = command + ' -res'
  param.out_timestep = 0
ENDIF
IF (param.out_ressm eq 1) THEN BEGIN
  command = command + ' -ressm'
  param.out_timestep = 0
ENDIF
IF (param.out_smres eq 1) THEN command = command + ' -smres'
IF (param.out_trres eq 1) THEN command = command + ' -trres'

n = param.polyterm + 2*param.harmterm -1
IF (param.out_coef EQ 1) THEN command = command + ' -coef 0,'+ $
			      STRCOMPRESS(STRING(n),/REMOVE_ALL)


;-------------------------------------------------------------------------
;		 Set the time step and format of output.

IF (param.out_timestep EQ 1) THEN command = command + ' -equal'
IF (param.out_timefmt  EQ 1) THEN BEGIN
    command = command + ' -cal'
    ncol_offset = 3
ENDIF ELSE ncol_offset = 1 


;------------------------------------------------------------------------
;		Define number of parameter and time_step

output_list =[param.out_orig, param.out_func, param.out_poly, param.out_smooth, param.out_trend, $
	      param.out_detrend, param.out_smcycle, param.out_harm, param.out_res, param.out_smres, $
	      param.out_trres, param.out_ressm, param.out_gr]
foo = where(output_list eq 1, cc)
if cc gt 0 then output_npar = cc $
else begin
     print,'No parameter selected.... ',output_list
     stop
endelse
ncolumn = output_npar + ncol_offset
input_ntime = n_elements(input_time)


;-------------------------------------------------------------------------
;		Write in a temporary file input values
;		======================================

infile = 'tmp_fitter_input'
OPENW, unit, infile, /GET_LUN
FOR n=0,input_ntime-1 DO BEGIN
    PRINTF, unit, input_time(n), input_val(n)
ENDFOR
FREE_LUN, unit

;-------------------------------------------------------------------------
;			Run ccgcrv:
;			===========

outfile  = 'tmp_fitter_result'
SPAWN, '/home/gcarb/contrib/tmsvu/src/ccgcrv'+command+' '+infile+' >! '+outfile
SPAWN,'rm -f '+ infile


;-------------------------------------------------------------------------
;		Open the temporary output file
;		==============================

OPENR,unit,outfile,/GET_LUN
CCG_FREAD,file=outfile,nc=ncolumn,var0
ntime=CCG_LIF(file=outfile)
FREE_LUN,unit

;-------------------------------------------------------------------------
;		DEFINE OUPUTS
;		=============

;Dimensions of the main arrays:
PRINT,'     Ntime=',ntime,' Npar=',output_npar
val_dec       = DBLARR(ntime)
val_date      = DBLARR(ntime,4)

IF (param.out_timefmt EQ 0) THEN BEGIN
   val_dec(*) = var0(0,*)  
   output = { $
	     date : val_dec, $
	     val  : var0(ncol_offset:output_npar, *) $
	    }
ENDIF ELSE BEGIN
   val_date(*,0:2) = TRANSPOSE(var0(0:2,*))
   val_date(*,3)   = 0.
   CCG_DATE2DEC, yr=val_date(*,0),mo=val_date(*,1),$
                 dy=val_date(*,2),hr=val_date(*,3),dec=val_dec
   output = { $
	     date : val_date, $
	     val  : var0(ncol_offset:output_npar, *) $
	    }
ENDELSE

RETURN,output

END
;-


