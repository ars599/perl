;+
PRO peyl_drawctblbreon,vmin=vmin,vmax=vmax,POS=pos,NC=nc,FIRST=fi,TIT=tit,NOFRAME=noframe
;
;  Cette routine permet de tracer une palette de couleur
;
IF NOT KEYWORD_SET(nc) THEN nc = !D.N_COLORS-1
IF NOT KEYWORD_SET(fi) THEN fi = 0
IF NOT KEYWORD_SET(vmin) THEN vmin=0
IF NOT KEYWORD_SET(vmax) THEN vmax=nc-1
IF NOT KEYWORD_SET(pos) THEN pos=[0.3,0.05,0.7,0.1]

IF !D.NAME EQ 'PS' THEN BEGIN
  V = BYTARR(nc,1)
  V(*,0) = BYTE(INDGEN(nc)+fi)
  tv,V,pos(0),pos(1),/NORMAL,xsize=pos(2)-pos(0),ysize=pos(3)-pos(1)
ENDIF ELSE BEGIN
  pxsize = LONG((POS(2)-POS(0))*!D.X_SIZE + 0.5)
  pysize = LONG((POS(3)-POS(1))*!D.Y_SIZE + 0.5)
  V = BYTARR(pxsize,pysize)
  Vv = BYTE(INDGEN(pxsize)*(nc-1.)/float(pxsize-1)+fi)
  FOR i=0,pysize-1 DO V(*,i) = Vv
  tv,V,pos(0),pos(1),/NORMAL
ENDELSE

V = INDGEN(nc)*(vmax-vmin)/float(nc-1) + vmin

IF NOT KEYWORD_SET(noframe) THEN plot,[0.,1.],[0.,1.],xrange=[vmin,vmax],pos=pos,$
                      /NODATA,/NORMAL,XSTYLE=1,YSTYLE=4,/NOERASE,TIT=tit

END
;-
