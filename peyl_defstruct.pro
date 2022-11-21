;====================================================================
;			DEFINE STRUCTURES
;====================================================================


;====================================================================
;		Define Parameter for Plot Routines 
;====================================================================
PRO def_par_plot, param
param = { $
	  dev:'', $
	  outfile: STRCOMPRESS(GETENV("PWD"))+'/idl.ps', $
	  portrait: 0, $
	  add_obs: 1, $
	  stat_name: 'unname', $
	  obs_type: 'mmix', $
	  year_obs: intarr(10), $
	  reseau: 'noaa', $
	  add_fit_mod: 0, $
	  add_fit_obs: 0, $
	  add_mod_err:0,$
	  add_obs_err:0,$
	  yrange: [0.,0.] , $
	  xrange: [0.,0.] , $
	  max_number: 1.e34, $
	  xtitle: '', $
	  xtitle2: '', $
	  ytitle: '', $
	  title: '', $
	  titlebis: '', $
	  footnote: '', $
          label: '',$
	  symbol_mod: 0, $
	  symbol_obs: 0, $
	  component: strarr(30), $
	  text_legend: strarr(30), $
          nodata: 0, $
	  annot: 0, $
	  zero_line:0, $
	  pos_leg:0. $
	}

END

;====================================================================
;		Define Parameter for Fitter Programme
;====================================================================
PRO def_par_fitter, param
param = { $
	  harmterm:4,$
	  polyterm:2,$
	  interval:1,$
	  sfilter:80,$
	  lfilter:650,$
	  out_month:0,$
	  out_orig:0,$
	  out_detrend:0,$
	  out_func:0,$
	  out_poly:0,$
	  out_harm:0,$
	  out_smooth:0,$
	  out_trend:0,$
	  out_smcycle:0,$
	  out_gr:0,$
	  out_ressm:0,$
	  out_res:0,$
	  out_smres:0,$ 
	  out_trres:0,$ 
	  out_timestep:0,$
	  out_coef:0,$		;---- 0 pour pas de coeff.
	  out_timefmt:0 $	;---- 0 for date else for yy mm dd hh	  
	}

END



