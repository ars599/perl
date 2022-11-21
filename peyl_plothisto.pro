;+
;========================================================================
; NAME:
;       PEYL_plothisto
;
; Cette routine trace un histogramme et un histogramme cumulé 
; d'un tableau donné en entrée
; La routine va adapte le range a 1%-99% de la distribution 
; pour virer les eventuelles valeurs erronées.
;
; OPTION:
;  UNVALID=unvalid   : Pour rentrer une valeur a ne pas considerer.  Defaut: pas de valeur
;  POS               : Position du tableau sur le graphique  Defaut : [0.15,0.15,0.98,0.98]
;  XTIT='xtit'       : Pour mettre un axe des x.  Defaut: pas de titre
;  COLhisto=colhisto : Couleur a utiliser pour l'histogramme (-1 pour pas d'histogramme) Defaut : 3
;  COLcum=colcum     : Couleur a utiliser pour l'histogramme cumulé (-1 pour pas d'histogramme) Defaut : 1
;
; CALLING SEQUENCE:
;       
;
; INPUTS:
;     
;
; OPTIONAL INPUT PARAMETERS:
; 
;
; OUTPUTS:
;
;       
; RESTRICTIONS:
; 
;
;========================================================================
PRO plothisto,dat_o,UNVALID=unvalid,POS=pos,XTIT=xtit,COLhisto=colhisto,COLcum=colcum
;


IF NOT KEYWORD_SET(pos) THEN pos=[0.15,0.15,0.98,0.98]
IF NOT KEYWORD_SET(xtit) THEN xtit=' '
IF NOT KEYWORD_SET(colhisto) THEN colhisto=3
IF NOT KEYWORD_SET(colcum)   THEN colcum  =1

IF KEYWORD_SET(unvalid) THEN BEGIN
  valid = WHERE(dat_o NE unvalid)
  dat = dat_o[valid]
ENDIF ELSE dat = dat_o

omin  = MIN(dat,MAX=omax)
bin   = (omax-omin)/10000.
histo = HISTOGRAM(dat,BINSIZE=bin)
Npt   = N_ELEMENTS(dat)
;
; On cherche 1% et 99% de la distribution
;
chisto = histo
FOR i=1l, N_ELEMENTS(histo)-1 DO chisto[i] = chisto[i-1] + histo[i]
vm = 0.01*Npt & vmin = 0l  &  WHILE (chisto[vmin] LE vm) DO vmin=vmin+1
vm = 0.99*Npt & vmax = 0l  &  WHILE (chisto[vmax] LT vm) DO vmax=vmax+1

X = INDGEN(N_ELEMENTS(histo))*bin + omin
vmin = omin + vmin*bin
vmax = omin + vmax*bin
IF (vmin GT 0. AND vmin LT 0.1*(vmax-vmin)) THEN vmin = 0.

box = (vmax-vmin)/float(omax-omin)*100.
histo = SMOOTH(histo*1.,FIX(box))
histo = histo/float(max(histo))
chisto = chisto/FLOAT(MAX(chisto))
xrange = [vmin,vmax]

plot,X,histo,xrange=[vmin,vmax],yrange=[0.,1.],POS=pos,/NODATA,/NOERASE,XTIT=xtit
IF colhisto GE 0 THEN oplot,X, histo,COL=colhisto
IF colcum   GE 0 THEN oplot,X,chisto,COL=colcum

END
;-
