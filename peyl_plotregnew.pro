;+
PRO PEYL_PLOTREGNEW, reg_contour=reg_contour, $
                     data_contour=data_contour,$
                     reg_clon_contour = reg_clon_contour,$
                     reg_clat_contour = reg_clat_contour,$
                     reg_map=reg_map, $
                     reg_color=reg_color, $
                     data_map=data_map,$
                     reg_clon_map = reg_clon_map,$
                     reg_clat_map = reg_clat_map,$
                     grid_name = grid_name, $ 
                     position = position, $
                     noerase = noerase, $
                     color_text = color_text,$
                     color_conti = color_conti,$
                     continent = continent, $
                     lthick = lthick, $
                     lstyle = lstyle, $
                     cont_lthick = cont_lthick, $
                     title = title, $
                     xtitle = xtitle,$
                     ytitle = ytitle,$
                     limit = limit,$
                     label = label,$
                     color_default=color_default,$
                     no_axes = no_axes,$
                     special_aerocarb=special_aerocarb,$
                     dev=dev

;-----------------------------------------------------------------
;           Define all variables.
;-----------------------------------------------------------------

if (not keyword_set(grid_name)) then grid_name = 'mask11'
if (not keyword_set(dev)) then dev = 'ps'
if (not keyword_set(position)) then position = [0.,0.,1.,1.]
if (not keyword_set(continent)) then continent = 0
if (not keyword_set(lthick)) then lthick = 1
if (not keyword_set(lstyle)) then lstyle = 0
if (not keyword_set(cont_lthick)) then cont_lthick = 1
if (not keyword_set(title)) then title = ''
if (not keyword_set(label)) then label = ''
if (not keyword_set(limit)) then limit = [-90,-180,90,180]
if (not keyword_set(no_axes)) then no_axes = 0
if (not keyword_set(special_aerocarb)) then special_aerocarb = 0
if (not keyword_set(color_default)) then color_default = 255
if (not keyword_set(color_text)) then begin
    color_text  = 1
    if dev eq '' then color_text = 255
endif
if (not keyword_set(color_conti)) then begin
    color_conti = color_text
    if continent eq 0 then color_conti = 255
endif


if (not keyword_set(reg_contour)) then begin
    nb_reg_contour = 0
endif else begin
    nb_reg_contour = n_elements(reg_contour)
    if reg_contour(0) eq '' then nb_reg_contour = 0
endelse
if (not keyword_set(reg_map)) then begin
    nb_reg_map = 0
endif else begin
    nb_reg_map = n_elements(reg_map)
    if reg_map(0) eq '' then nb_reg_map = 0
endelse

grid = peyl_choixgrid(grid_name)



;-----------------------------------------------------------------
;           Color the specified regions..
;-----------------------------------------------------------------

if (not keyword_set(data_map) and nb_reg_map le 0) then begin

    map_set, 0, 0,$
      position=position, $
      title= title,$
      mlinethick=cont_lthick,$
      mlinestyle=0,$
      continent=continent,$
;      continent=0,$
      limit=limit,$
      /cylindrical, /noborder, $
      noerase=noerase

;;map_continents,/usa,/countries


endif else begin

print,'Begin of data to map'

;---- Define the data if not supplied to be colored

if (not keyword_set(data_map)) then begin
    if (nb_reg_map gt n_elements(reg_color) ) then begin
        print,'erreur nb de region and nb de couleur'
        stop
    endif
    data_map = fltarr(grid.nlon,grid.nlat)
    data_map(*) = color_default
    surf_reg = fltarr(nb_reg_map)
    reg_clon_map = fltarr(nb_reg_map)
    reg_clat_map = fltarr(nb_reg_map)
    for i=0,nb_reg_map-1 do begin    
;;        components  = str_sep(reg_map(i),'+')
;;        nbcomp      = n_elements(components)
        ind         = peyl_choixregnew_old(reg_map(i),grid_name=grid.name,$
                                       special_aerocarb=special_aerocarb,/noinfo) 
        surf_reg(i) = total(grid.dxyp(ind.ii2d))
        data_map(ind.ii2d) = reg_color(i)
        temp_lon = grid.lon(ind.lon)
        temp_lat = grid.lat(ind.lat)
        reg_clon_map(i) = (max(temp_lon) + min(temp_lon) )/2.
        reg_clat_map(i) = (max(temp_lat) + min(temp_lat) )/2.
    endfor
endif

if 1 then begin
    vmin = 0
    vmax = 255
endif

;print,limits
peyl_map2d, data_map, $
  LATS=grid.lat, $
  LONS=grid.lon, $
  LIMITS=limit, $
  vmin=vmin,$
  vmax=vmax,$
  MISSING=missing, $
  BADCOLOR=badcolor, $
  POS=position,$
  TITLE=title, $
;  COLBAR=colbar, $
;  VERTICBAR=verticbar,$
;  AXISTITLE=axistitle,$
;  NB_INTERVAL=nb_interval, $
;  VAL_INTERVAL=val_interval, $
;  NB_COL=nb_col, $
;  FIRST_COL=first_col, $
  col_text = color_text,$
  NOCOAST=0,$
  mollweide=mollweide,$
  INTERPOLATE=interpolate, $
  NOERASE=noerase, $
  sature_minmax=sature_minmax,$
  x_center = x_center,$
  y_center = y_center,$
  mlinethick = mlinethick,$
  leg_charsize=leg_size,$
  leg_charthick=leg_thick,$
  add_point=addpt
;  xaxis=2,$
;  nb_x=nb_x,$
;  xthick=xthick,$
;  yaxis=2,$
;  nb_y=nb_y,$
;  ythick=ythick
        
endelse 


;-----------------------------------------------------------------
;                 Draw border of regions...
;-----------------------------------------------------------------

if (not keyword_set(data_contour) and nb_reg_contour le 0) then goto,suite_contour

print,'Begin of data to contour'

;---- Define the data if not supplied to be contoured

if (not keyword_set(data_contour)) then begin
    data_contour = fltarr(grid.nlon,grid.nlat)
    reg_clon_contour = fltarr(nb_reg_contour)
    reg_clat_contour = fltarr(nb_reg_contour)
    for i=0,nb_reg_contour-1 do begin    
;;        components  = str_sep(reg_contour(i),'.')
;;        nbcomp      = n_elements(components)
        ind         = peyl_choixregnew_old(reg_contour(i),grid_name=grid.name,$
                                       special_aerocarb=special_aerocarb,/noinfo) 
        data_contour(ind.ii2d) = i + 1
        temp_lon = grid.lon(ind.lon)
        temp_lat = grid.lat(ind.lat)
        reg_clon_contour(i) = (max(temp_lon) + min(temp_lon) )/2.
        reg_clat_contour(i) = (max(temp_lat) + min(temp_lat) )/2.
    endfor
endif

lonw = grid.lon ;;;; - grid.dlon/2. 
lone = lonw + grid.dlon
lats = grid.lat - grid.dlat/2.
latn = lats + grid.dlat

;--- petite bidouille..
;lats = lats + lats * 0.025
;latn = latn + latn * 0.025
;latn(where(latn gt 90.)) = 90.
;lats(where(lats lt -90.)) = -90.


for j=0,grid.nlat-1 do begin
    for i=0,grid.nlon-1 do begin        
                                ;--- Define preceeding point
        ip = max([0,i-1])
        jp = max([0,j-1])
                                ;--- Plot latitudes lines
        if data_contour(ip,j) ne data_contour(i,j) then $
          oplot,[lonw(i),lonw(i)],[lats(j),latn(j)],$
                color=color_text,thick=lthick,linestyle=lstyle

                                ;--- Plot longitudes lines
        if data_contour(i,jp) ne data_contour(i,j) then begin
            dd = 0.3
            oplot,[lonw(i)-dd,lone(i)+dd],[lats(j),lats(j)],$
              color=color_text,thick=lthick,linestyle=lstyle
        endif
    endfor 
endfor

suite_contour:


;-----------------------------------------------------------------
;                     Define axes 
;-----------------------------------------------------------------

if 0 then begin

lati = ['90!eo!nS', '60!eo!nS', '30!eo!nS', '0!eo!n', $
        '30!eo!nN', '60!eo!nN','90!eo!nN']
long = ['180!eo!nW', '120!eo!nW', '60!eo!nW', '0!eo!n', $
        '60!eo!nE', '120!eo!nE', '180!eo!nE']
yminor = 3
yticks = 6
xminor = 3
xticks = 6

if limit(0) eq -60 then begin
    lati = ['60!eo!nS', '30!eo!nS', '0!eo!n', $
            '30!eo!nN', '60!eo!nN','90!eo!nN']

    yminor = 3
    yticks = 5
endif 

if no_axes then begin 
    lati = replicate(' ',20)
    long = replicate(' ',20)
    ytitle = ''
    xtitle = ''
    yminor = 1
    yticks = 1
    xminor = 1
    xticks = 1   
endif

th=1.
cth=1.
size=1.
csize=1
axis,ystyle = 1, $
  yaxis = 0, $
  yminor = yminor, $
  yticks = yticks, $
  yticklen = 0.01, $
  ytitle = ytitle, $
  color=1,$
  ythick=th,$
  charthick=cth,$
  ycharsize=size,$
  charsize=csize,$
  ytickname = lati

axis,xstyle = 1, $
  xaxis = 0, $
  xminor = xminor, $
  xticks = xticks, $
  xticklen = 0.01, $
  xtitle = xtitle, $
  color=1,$
  xthick=th,$
  charthick=cth,$
  xcharsize=size,$
  charsize=csize,$
  xtickname = long

;--------- label top and right axes
;
axis,ystyle = 1, $
  yaxis = 1, $
  yminor = yminor, $
  yticks = yticks, $
  yticklen = 0.01, $
  color=1,$
  ythick=th,$
  charthick=cth,$
  ycharsize=size,$
  charsize=csize,$
  ytickname = replicate(' ',10)

axis,xstyle = 1, $
  xaxis = 1, $
  xminor = xminor, $
  xticks = xticks, $
  xticklen = 0.01, $
  color=1,$
  xthick=th,$
  charthick=cth,$
  xcharsize=size,$
  charsize=csize,$
  xtickname = replicate(' ',10)

endif

;-------- Label the plot...
;

if label ne '' then begin
    lon = [limit(1),limit(3)]
    lat = [limit(2),limit(2)]
    p = convert_coord(lon, lat, /data, /to_device)

;    xyouts, limit(1)-0.1*(limit(3)-limit(1)),limit(2),label,$
    xyouts, p(0,0)-0.12*(p(0,1)-p(0,0)),p(1,0),label,/device,$
      orientation=0, $
      charsiz=1.2,$
      charthick=2.

endif


END
;-

