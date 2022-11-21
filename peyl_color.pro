;+
PRO peyl_color,save=save,get=get,col_rgb=col_rgb
;
if n_elements(save) gt 0 then begin
    print,'save current colors to file ',save
    tvlct,r,g,b,/get
    col_rgb = {r:r,g:g,b:b}
    save,file=save,col_rgb
endif else if n_elements(get) gt 0 then begin
    print,'read colors from save file ',get
    restore,get
    tvlct,col_rgb.r,col_rgb.g,col_rgb.b
endif else print,'no action : use save or get keywords'
END
;-
