;+
PRO PEYL_PLOTREG, reg_contour=reg_contour, $
                  data_contour=data_contour,$
                  reg_map=reg_map, $
                  reg_color=reg_color, $
                  data_map=data_map,$
                  grid_name = grid_name, $ 
                  position = position, $
                  noerase = noerase, $
                  continent = continent, $
                  lthick = lthick, $
                  lstyle = lstyle, $
                  cont_lthick = cont_lthick, $
                  title = title, $
                  xtitle = xtitle,$
                  ytitle = ytitle,$
                  limit = limit,$
                  label = label,$
                  no_axes = no_axes,$
                  dev=dev

;-----------------------------------------------------------------
;           Define all variables.
;-----------------------------------------------------------------

if (not keyword_set(grid_name)) then grid_name = 'sib54'
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

nb_reg_contour = n_elements(reg_contour)
nb_reg_map     = n_elements(reg_map)

grid = peyl_choixgrid(grid_name)

color_text  = 0
if dev eq '' then color_text = 255
color_conti = color_text
if continent eq 0 then color_conti = 255


;-----------------------------------------------------------------
;           Define the map
;-----------------------------------------------------------------

contour, [[0,0],[1,1]],$
  position=position,$
  xstyle = 4, ystyle = 4,$
  xtitle = xtitle,$
  ytitle = ytitle,$
  title = title,$
  color=color_text,$
  /nodata, noerase=noerase

;-----------------------------------------------------------------
;           Color the specified regions..
;-----------------------------------------------------------------

if (not keyword_set(data_map) and nb_reg_map le 0) then goto,suite_map

;---- Define the data if not supplied to be colored

if (not keyword_set(data_map)) then begin
    if (nb_reg_map gt n_elements(reg_color) ) then begin
        print,'erreur nb de region and nb de couleur'
        stop
    endif
    data_map = fltarr(grid.nlon,grid.nlat)
    data_map(*) = 255
    surf_reg = fltarr(nb_reg_map)
    for i=0,nb_reg_map-1 do begin    
        components  = str_sep(reg_map(i),'.')
        nbcomp      = n_elements(components)
        ind         = peyl_choixreg(components,grid=grid.name,/noinfo) 
        surf_reg(i) = total(grid.dxyp(ind.ii2d))
        data_map(ind.ii2d) = reg_color(i)
    endfor
endif

sz=size(data_map)

px = !x.window * !d.x_vsize
py = !y.window * !d.y_vsize
swx = px(1)-px(0)               ; Size in x in device units

swy = py(1)-py(0)               ; Size in Y
six = float(sz(1))              ; Image sizes
siy = float(sz(2))
aspi = six / siy                ; Image aspect ratio
aspw = swx / swy                ; Window aspect ratio
f = aspi / aspw                 ; Ratio of aspect ratios

if (!d.flags and 1) ne 0 then begin ; Scalable pixels?
    tv,  data_map,  px(0),py(0),  xsize = swx, ysize = swy,  /device
endif else begin                ; Not scalable pixels
    interp=0
    tv,  poly_2d(data_map, [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]], $
                 interp,swx,swy), px(0),py(0)
endelse

suite_map:

;-----------------------------------------------------------------
;                 Draw boundary of continents...
;-----------------------------------------------------------------

map_set, 0, 0,$
  position=position, $
  title= title,$
;  latdel=360, $
;  londel=720,$
;  color=color_conti,$
;  grid = 0, $
;  E_GRID={color:255}, $
  mlinethick=cont_lthick,$
  mlinestyle=0,$
  continent=continent,$
  limit=limit,$
    /cylindrical, /noborder, /noerase

;;map_continents,/usa,/countries

;-----------------------------------------------------------------
;                 Draw border of regions...
;-----------------------------------------------------------------

if (not keyword_set(data_contour) and nb_reg_contour le 0) then goto,suite_contour

;---- Define the data if not supplied to be contoured

if (not keyword_set(data_contour)) then begin
    data_contour = fltarr(grid.nlon,grid.nlat)
    for i=0,nb_reg_contour-1 do begin    
        components  = str_sep(reg_contour(i),'.')
        nbcomp      = n_elements(components)
        ind         = peyl_choixreg(components,grid=grid.name,/noinfo) 
        data_contour(ind.ii2d) = i + 1
    endfor
endif

lonw = grid.lon ;;;; - grid.dlon/2. 
lone = lonw + grid.dlon
lats = grid.lat - grid.dlat/2.
latn = lats + grid.dlat

;--- petite bidouille..
lats = lats + lats * 0.025
latn = latn + latn * 0.025
latn(where(latn gt 90.)) = 90.
lats(where(lats lt -90.)) = -90.


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

