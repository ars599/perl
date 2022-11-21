;+
;
;  Permet de faire une carte a partir d'un fichier 2D
;  INPUT : tab  Doit etre un fichier 2D.  Si c'est du byte, il est 
;     directement projette.  Si c'est un autre type, il va dabord etre
;                 converti en byte sur la palette de couleur (avec 
;                 les options vmin et vmax)
;  OPTIONS:
; vmin,vmax : Valeurs min et max.  Si tab n'est pas de type Byte, 
;             utilise pour convertir les donnees sur la palette.  Si tab est de 
;             format byte, permet de donner les valeurs physiques a la palette 
;             de couleur
;  POS=     : position de la carte sur la fenetre
;  MISSING= : valeur des donnees "non physiques"
;  LIMITS=[latmin,lonmin,latmax,lonmax]  Permet de faire un zoom sur 
;             une portion du globe
;  TIT=tit  : permet de mettre un titre
;  /COLBAR  : dessine une palette de couleur si le keyword est positionne
;  MOLLWEIDE: pour une projection mollweide
;  /NOCOAST : Pour ne pas mettre les lignes de cote
;  lons=    : Longitudes sur lesquelles sont donnees les valeurs du tableau.
;             Si vecteurs a 2 elements, interprete comme [lonmin,lonmax]
;  lats=    : Latitudes sur lesquelles sont donnees les valeurs du tableau.
;             Si vecteurs a 2 elements, interprete comme [latmin,latmax]
;  ATTENTION : les valeurs min/max des latitudes et longitudes doivent etre 
;              les centres des boites
;  BADCOLOR=: Color value to be used for ploting badata
;  /NOINTERPOLATE : Permet de ne pas interpoler (+ proche voisin)
;
PRO peyl_map2dbreon,tab_,POS=pos,LATS=lats_,LONS=lons,MISSING=baddata,LIMITS=limits, $
          TIT=tit,mollweide=mollweide,COLBAR=colbar,vmin=vmin,vmax=vmax,$
          NOCOAST=nocoast,BADCOLOR=badcolor,NOINTERPOLATE=nointerpolate

IF NOT KEYWORD_SET(pos) THEN IF KEYWORD_SET(COLBAR) THEN $
  pos=[0.02,0.2,0.98,0.98] ELSE pos=[0.02,0.1,0.98,0.98]

s=size(tab_)
IF (s(0) EQ 3) THEN BEGIN
    print,'Tableau de dimension 3 !! : Moyenne sur la 3eme dimension !!'
    tab = total(tab_,3)/float(s(3))
    s = size(tab)
ENDIF ELSE tab = tab_

IF (s(0) NE 2) THEN BEGIN
   print,'L"argument de la fonction plotarray doit etre un tableau bibimensionnel'
   RETURN
ENDIF

IF KEYWORD_SET(lats_) THEN BEGIN
    IF N_ELEMENTS(lats_) EQ 2 THEN BEGIN
        latmin = MIN(lats_,MAX=latmax) 
        Nlat   = s(2)
    ENDIF ELSE BEGIN 
;
; In case the latitude are not in increasing order
;
        IF (lats_[0] GT lats_[1]) THEN BEGIN
            lats = REVERSE(lats_)
            tab  = ROTATE(tab,7)
        ENDIF ELSE lats = lats_
        
        IF N_ELEMENTS(lats) NE s[2] THEN STOP,' Incompatibilite des dimensions en latitude'
        latmin = MIN(lats,max=latmax) 
        Nlat   = s(2)
        reso_lat = MIN(ABS(lats(0:Nlat-2)-lats(1:Nlat-1)))
        Nlat_int = (latmax-latmin)/reso_lat + 1
        Y = FLTARR(Nlat_int)
        ilat = 0
        FOR i = 0, Nlat_int-1 DO BEGIN
            lat = latmin + (latmax-latmin)*i/FLOAT(Nlat_int-1)
            WHILE (lat GT lats[ilat+1]) DO ilat = ilat + 1
            Y[i] = ilat + (lat-lats[ilat])/(lats[ilat+1]-lats[ilat])
        ENDFOR
        IF KEYWORD_SET(Nointerpolate) THEN Y=ROUND(Y)
    ENDELSE
ENDIF ELSE BEGIN
    Nlat   = s(2)
    latmin =-90.+90./Nlat & latmax=90.-90./Nlat 
ENDELSE

IF KEYWORD_SET(lons) THEN  BEGIN
    IF N_ELEMENTS(lons) EQ 2 THEN BEGIN
        lonmin = MIN(lons,MAX=lonmax) 
        Nlon   = s(1)
    ENDIF ELSE BEGIN

; Longitude should be in increasing order..
;
        IF (lons[0] GT lons[s[1]-1]) THEN stop,' Longitude should be in increasing order...'

        IF (N_ELEMENTS(lons) NE s[1]) THEN STOP,'Incompatibilite des dimensions en longitude'
        lonmin=MIN(lons,max=lonmax) 
        Nlon = N_ELEMENTS(lons)
        reso_lon = MIN(ABS(lons(0:Nlon-2)-lons(1:Nlon-1)))
        Nlon_int = (lonmax-lonmin)/reso_lon + 1
        X = FLTARR(Nlon_int)
        ilon = 0
        FOR i = 0, Nlon_int-1 DO BEGIN
            lon = lonmin + (lonmax-lonmin)*i/FLOAT(Nlon_int-1)
            WHILE (lon GT lons[ilon+1]) DO ilon = ilon + 1
            X[i] = ilon + (lon-lons[ilon])/(lons[ilon+1]-lons[ilon])
        ENDFOR
        IF KEYWORD_SET(Nointerpolate) THEN X=ROUND(X)
    ENDELSE 
ENDIF ELSE BEGIN
    Nlon   = s(1)
    lonmin =-180.+180./Nlon & lonmax=180.-180./Nlon
ENDELSE
    
IF (KEYWORD_SET(lons) OR KEYWORD_SET(lats)) THEN BEGIN
    IF NOT KEYWORD_SET(Y) THEN Y = INDGEN(s(2))
    IF NOT KEYWORD_SET(X) THEN X = INDGEN(s(1))
    tab_int=INTERPOLATE(tab,X,Y,/GRID)
ENDIF ELSE tab_int = tab

IF s(3) EQ 1 THEN BEGIN
;
;  On est dans le cas ou on a deja mis les valeurs en equivalent palette de couleur
;
   visu=tab_int
ENDIF ELSE BEGIN
;
;  On veut projeter les valeurs sur la palette de couleur.
;
   tvlct,r,g,b,/get
   NC = n_elements(R)
   IF NOT (KEYWORD_SET(vmin) AND KEYWORD_SET(vmax)) THEN BEGIN
;  On recherche les valeurs min et max dans le tableau
     IF KEYWORD_SET(baddata) THEN BEGIN
       valid = WHERE(tab_int NE baddata)
       vmin=MIN(tab_int(valid),MAX=vmax)
     ENDIF ELSE vmin=MIN(tab_int,MAX=vmax)
   ENDIF
   visu=BYTE( (1+((tab_int-vmin)>0)*(NC-3.)/(vmax-vmin))<(NC-3.) )
   IF KEYWORD_SET(baddata) THEN BEGIN
     IF NOT KEYWORD_SET(badcolor) THEN badcolor =  !P.BACKGROUND
     bad = where(tab_int EQ baddata,cc)
     IF cc NE 0 THEN visu(bad) = badcolor
   ENDIF
ENDELSE

map_set,0.,0.,limit=limits,/NOBORDER,TITLE=tit,POSITION=POS,mollweide=mollweide,/NOERASE

warp=map_image(visu,xx,yy,xs,ys,compress=1,latmin=latmin,latmax=latmax,$
               lonmin=lonmin,lonmax=lonmax,missing=!P.BACKGROUND,scale=0.04)

tv,warp,xx,yy,xsize=xs,ysize=ys
IF NOT KEYWORD_SET(nocoast) THEN map_continents,/coasts

IF KEYWORD_SET(colbar) THEN peyl_drawctblbreon,vmin=vmin,vmax=vmax,pos=[0.3,0.05,0.7,0.1],NC=NC-2,FIRST=1

END
;-
