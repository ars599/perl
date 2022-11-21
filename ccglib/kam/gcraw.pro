PRO	GCRAW_HELP
	CCG_MESSAGE,STRING(10B)+"GCRAW"+STRING(10B)
	CCG_MESSAGE,"This procedure processes MLO GC in situ raw files"
	CCG_MESSAGE,"for CH4, CO, and H2.  The procedure calls the C program"
	CCG_MESSAGE,"/projects/network/lib/gc_insituprocess which in turn calls the"
	CCG_MESSAGE,"ccg library."
	CCG_MESSAGE,STRING(10B)+"Arguments to GCRAW:"+STRING(10B)
	CCG_MESSAGE,"file:	Must be of the form 19951231.ch4"
	CCG_MESSAGE,"sp:	Only required if file has no species suffix." 
	CCG_MESSAGE,"site:	Required, but at present, default to 'mlo'."
	CCG_MESSAGE,"update:	If set to one (1), then update month and year files."
	CCG_MESSAGE,"ddir:	Save update files to non-default directory." 
	CCG_MESSAGE,"noamie:	If set to one (1), AMIE will NOT be invoked for CH4."
	CCG_MESSAGE,"dev:	Send graphics output to specified device."
	CCG_MESSAGE,"nograph:	If set to one (1), there will be no graphics."
	CCG_MESSAGE,"nomonth:	If set to one (1), there will be no monthly plots."
	CCG_MESSAGE,STRING(10B)+"Examples:"+STRING(10B)
	CCG_MESSAGE,"GCRAW,file='19951208.ch4',site='mlo'
	CCG_MESSAGE,"GCRAW,file='19951208.ch4',site='brw'
	CCG_MESSAGE,"GCRAW,file='/users/ken/tmp/19959999.ch4'
	CCG_MESSAGE,"GCRAW,file='19951201.co',update=1
	CCG_MESSAGE,"GCRAW,file='19951231.co',ddir='/users/ken/tmp/',update=1
	CCG_MESSAGE,"GCRAW,file='19960101.ch4',noamie=1,update=1,nograph=1,nomonth=1
END

PRO	GCRAW,$
	help,$
	sp=sp,$
	site=site,$
	dev=dev,$
	file=rawfile,$
	ddir=ddir,$
	noamie=noamie,$
	update=update,$
	nomonth=nomonth,$
	nograph=nograph
;
;Comments
;
;GAP of 0.37 corresponds to ~22 minutes
;This is to ensure that gaps in ambient samples for
;both ch4 and co are not plotted with connected lines. 
;
;Check help status
;
IF N_PARAMS() THEN BEGIN
	GCRAW_HELP
        RETURN
ENDIF
;
;Check raw file status
;
IF NOT KEYWORD_SET(rawfile) THEN BEGIN
        CCG_MESSAGE,"Raw file must be specified, i.e., rawfile='19951207.co'"
        RETURN
ENDIF
;
;Was a species suffix specified with the file name?
;
j=CCG_STRRPOS(rawfile,'.')
IF j(0) NE -1 THEN sp=STRMID(rawfile,j(0)+1,1000)
;
;Check Species status
;
IF NOT KEYWORD_SET(sp) THEN BEGIN
        CCG_MESSAGE,"Since file name does not appear to have the usual suffix,
	CCG_MESSAGE,"e.g., ch4,co,h2.  You must also specify species, e.g., sp='co'"
        RETURN
ENDIF
sp=STRLOWCASE(sp)
;
;If site is not specified then
;default to 'mlo' for now.
;
IF NOT KEYWORD_SET(site) THEN site='mlo'
;
;Should month files be plotted?
;
IF KEYWORD_SET(nomonth) THEN nomonth=1 ELSE nomonth=0
;
;Report to user
;
;
CCG_MESSAGE, "Processing "+rawfile+" ..."
;
IF NOT KEYWORD_SET(update) THEN $
	CCG_MESSAGE,"Monthly and yearly files NOT updated"
IF NOT KEYWORD_SET(noamie) AND sp EQ 'ch4' THEN $
	CCG_MESSAGE,"AMIE has been invoked"
;
;-------------------------------------- misc. initialization
;
tempfile='/usr/tmp/'+GETENV("LOGNAME")+'.gcraw.1'
monthfile='/usr/tmp/'+GETENV("LOGNAME")+'.gcraw.2'
SPAWN, 'rm -f '+tempfile+' '+monthfile
DEFAULT=(-999.999)
GAP=0.37
HIY=8760
ccode='/projects/network/lib/gc_insituprocess'
;
;Build argument list for C code
;
arg=' -r '+rawfile
arg=TEMPORARY(arg)+' -g '+sp
arg=TEMPORARY(arg)+' -t '+monthfile
arg=TEMPORARY(arg)+' -c '+tempfile
arg=TEMPORARY(arg)+' -s '+site
IF KEYWORD_SET(ddir) THEN arg=TEMPORARY(arg)+' -d '+ddir
IF NOT KEYWORD_SET(noamie) THEN arg=TEMPORARY(arg)+' -a '
IF KEYWORD_SET(update) THEN arg=TEMPORARY(arg)+' -u '
;
;Run C program to process rawfile
;
SPAWN,	ccode+arg
;
;Are we done or should we do graphics?
;
IF KEYWORD_SET(nograph) THEN RETURN
;
;Any problems with raw file?
;
z1=CCG_LIF(file=monthfile)
z2=CCG_LIF(file=tempfile)
IF z1 LT 2 THEN BEGIN
	CCG_MESSAGE,"A problem was detected processing "+rawfile+".  No Graphics.",/bell
	RETURN
ENDIF
;
;-------------------------------------- setup graphics device
;
CCG_OPENDEV,    dev=dev,pen=pen,win=(-1)
;
;Read saved chromatographic information
;
CCG_SREAD,file=tempfile,/nomessages,result
;
;Identify number of
;references in raw file.
;
j=WHERE(STRLEN(result) LT 20)
nrefs=N_ELEMENTS(j)-1
nres=N_ELEMENTS(result)

refname=MAKE_ARRAY(nrefs+1,/STR,VALUE='')
FOR i=0,nrefs-1 DO refname(i)=result(j(i))
site=STRLOWCASE(result(j(nrefs)))
refname(nrefs)='AMBIENT'

gmt=MAKE_ARRAY(nrefs+1,nres,/DOUBLE,VALUE=DEFAULT)
year=MAKE_ARRAY(nrefs+1,nres,/INT,VALUE=DEFAULT)
pkht=MAKE_ARRAY(nrefs+1,nres,/DOUBLE,VALUE=DEFAULT)
pkar=MAKE_ARRAY(nrefs+1,nres,/DOUBLE,VALUE=DEFAULT)
rt=MAKE_ARRAY(nrefs+1,nres,/DOUBLE,VALUE=DEFAULT)
;
;Parse reference and sample
;information.	
;
IF nrefs EQ 0 THEN BEGIN
        CCG_MESSAGE,"A problem was detected reading "+rawfile+".  Exiting..."
        RETURN
ENDIF

FOR i=0,nrefs DO BEGIN
	;
	;save to temporary file
	;
	OPENW,unit,tempfile,/GET_LUN
	IF i LT nrefs THEN 	PRINTF,unit,result(j(i)+1:j(i+1)-1) $
	ELSE 			PRINTF,unit,result(j(i)+1:*)
	FREE_LUN,unit
	;
	;read temporary file
	;
	CCG_FREAD,	file=tempfile,/nomessage,nc=8,var
	CCG_DATE2DEC,	yr=var(0,*)-FIX(var(0,0)),$
			mo=var(1,*),dy=var(2,*),hr=var(3,*),mn=var(4,*),dec=dec

	year(i,0:N_ELEMENTS(dec)-1)=var(0,*)
	gmt(i,0:N_ELEMENTS(dec)-1)=dec
	pkht(i,0:N_ELEMENTS(dec)-1)=var(5,*)
	pkar(i,0:N_ELEMENTS(dec)-1)=var(6,*)
	rt(i,0:N_ELEMENTS(dec)-1)=var(7,*)
ENDFOR
;
;Now determine X Axis range
;
j=WHERE(gmt-1 GT DEFAULT)

CCG_DEC2DATE,	MIN(gmt(j)),yr,mo,dy,hr,mn
CCG_DATE2DEC,	yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=xmin
fhour=hr

CCG_DEC2DATE,	MAX(gmt(j)),yr,mo,dy,hr,mn
CCG_DATE2DEC,	yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=xmax

nhours=CCG_ROUND((xmax-xmin)*HIY,0)+1
;
;If there are more than 30 hours then
;readjust X scale.
;
steps=1
IF nhours GT 30 THEN BEGIN
        IF nhours MOD 2 EQ 1 THEN nhours=TEMPORARY(nhours)+1
        nhours=TEMPORARY(nhours)/2
	steps=2
ENDIF

nxlbl=MAKE_ARRAY(nhours,/STR,VALUE=' ')
xlbl=MAKE_ARRAY(nhours,/STR,VALUE=' ')

j=fhour
FOR i=0,nhours-1 DO BEGIN
	IF j GT 23 THEN j=0
	xlbl(i)=STRCOMPRESS(STRING(j),/RE)
	j=j+steps
ENDFOR
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PLOT GC RAW DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;Name window
;
IF NOT KEYWORD_SET(dev) THEN WINDOW,	0,$
					xsize=700,ysize=600,$
					TITLE='GC raw data '+rawfile
;
;Figure sizes
;
fig_top=0.95
fig_left=0.12

yfmax=0.40
xfmax=0.56

npanes=nrefs+1
ydelta=yfmax/npanes
xdelta=0.38
;
;Top Left frame - Peak Heights
;
top=fig_top
left=fig_left
 
FOR pn=1,npanes DO BEGIN

	x1=left
	x2=left+xdelta
	y1=top-(ydelta*pn)
	y2=top-(ydelta*(pn-1))

	j=WHERE(pkht(pn-1,*)-1 GT DEFAULT)
	x=gmt(pn-1,j)
	y=pkht(pn-1,j)/10.0^6.0
	;
	;Determine Y axis range
	;for peak heights
	;
;
;Fix this
;
	IF N_ELEMENTS(y) GT 1 THEN BEGIN
		z=MOMENT(y,sdev=sdev)
		ymin=CCG_ROUND(z(0)-(5*sdev),-3)
		ymax=CCG_ROUND(z(0)+(5*sdev),-3)
	ENDIF ELSE BEGIN
		ymin=-1*CCG_ROUND(5*sdev,-3)
		ymax=CCG_ROUND(5*sdev,-3)
	ENDELSE

	PLOT,		x,y,$
 			COLOR=pen(1), $
			CHARSIZE=2.0,$
			CHARTHICK=1.0,$
			/NODATA,$
			/NOERASE,$
			POSITION=[x1,y1,x2,y2],$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			YTHICK=1.0,$
			YMINOR=2, $
			YCHARSIZE=0.01,$
			XSTYLE=1,$
			XRANGE=[xmin,xmax],$
			XTICKS=nhours-1,$
			XTHICK=1.0,$
			XMINOR=2, $
			XTICKNAME=nxlbl,$
			XCHARSIZE=0.01

	AXIS,		YAXIS=0,$
			YSTYLE=1,$
			COLOR=pen(1),$
			YRANGE=[ymin,ymax],$
			YTHICK=1.0,$
			YMINOR=2, $
			CHARSIZE=2.0,$
			YCHARSIZE=0.30

	CCG_PLOTGAP,	x,y,$
			gap=GAP,$
			THICK=1,$
			COLOR=pen(pn+1)

	XYOUTS,		xmin+(xmax-xmin)*0.05,$
			ymax-(ymax-ymin)*0.25,$
			COLOR=pen(1),$
			/DATA,$
			CHARSIZE=1.0,$
			ALI=0.0,$
			STRCOMPRESS(refname(pn-1),/RE)
		
ENDFOR

PLOTS,	[x1,x1,x2,x2,x1],[top,y1,y1,top,top],$
	THICK=2.0,$
	COLOR=pen(13),$
	/NORMAL
;
;Bottom Left frame - Peak Areas
;
top=fig_top-yfmax
left=fig_left
;
FOR pn=1,npanes DO BEGIN

	x1=left
	x2=left+xdelta
	y1=top-(ydelta*pn)
	y2=top-(ydelta*(pn-1))

	j=WHERE(pkar(pn-1,*)-1 GT DEFAULT)
	x=gmt(pn-1,j)
	y=pkar(pn-1,j)/10.0^6.0
	;
	;Determine Y axis range
	;for peak heights
	;
	z=MOMENT(y,sdev=sdev)
	ymin=CCG_ROUND(z(0)-(5*sdev),-3)
	ymax=CCG_ROUND(z(0)+(5*sdev),-3)

	IF pn EQ npanes THEN xtickname=xlbl ELSE xtickname=nxlbl

	PLOT,		x,y,$
			COLOR=pen(1), $
			CHARSIZE=2.0,$
			CHARTHICK=1.0,$
			/NODATA, $
			/NOERASE, $
 			POSITION=[x1,y1,x2,y2],$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			YTHICK=1.0,$
			YMINOR=2, $
			YCHARSIZE=0.01,$
			XSTYLE=1,$
			XRANGE=[xmin,xmax],$
			XTICKS=nhours-1,$
			XTICKNAME=xtickname,$
			XTHICK=1.0,$
			XMINOR=2, $
			XCHARSIZE=0.35

	AXIS,		YAXIS=0,$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			COLOR=pen(1),$
			YTHICK=1.0,$
			YMINOR=2, $
			CHARSIZE=2.0,$
			YCHARSIZE=0.30

	CCG_PLOTGAP,	x,y,$
			gap=GAP,$
			THICK=1,$
			COLOR=pen(pn+1)

	XYOUTS,		xmin+(xmax-xmin)*0.05,$
			ymax-(ymax-ymin)*0.25,$
			COLOR=pen(1),$
			/DATA,$
			CHARSIZE=1.0,$
			ALI=0.0,$
			STRCOMPRESS(refname(pn-1),/RE)
		
ENDFOR

PLOTS,	[x1,x1,x2,x2,x1],[top,y1,y1,top,top],$
	THICK=2.0,$
	COLOR=pen(13),$
	/NORMAL
;
;Top Right frame - Peak Widths
;
top=fig_top
left=fig_left+xdelta+0.02
;
FOR pn=1,npanes DO BEGIN

	x1=left
	x2=left+xdelta
	y1=top-(ydelta*pn)
	y2=top-(ydelta*(pn-1))
	;
	;Peak width is a computed parameter
	;
	j=WHERE(pkht(pn-1,*) GT 0 AND pkar(pn-1,*) GT 0)

	x=gmt(pn-1,j)
	y=pkar(pn-1,j)/(pkht(pn-1,j)*60.0)*10.0
	;
	;Determine Y axis range
	;for peak heights
	;
	z=MOMENT(y,sdev=sdev)
	ymin=CCG_ROUND(z(0)-(5.0*sdev),-3)
	ymax=CCG_ROUND(z(0)+(5.0*sdev),-3)

	PLOT,		x,y,$
			COLOR=pen(1), $
			CHARSIZE=2.0,$
			CHARTHICK=1.0,$
			/NODATA,$
			/NOERASE, $
			POSITION=[x1,y1,x2,y2],$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			YTHICK=1.0,$
			YMINOR=2, $
			YCHARSIZE=0.01,$
			XSTYLE=1,$
			XRANGE=[xmin,xmax],$
			XTICKS=nhours-1,$
			XTHICK=1.0,$
			XMINOR=2, $
			XCHARSIZE=0.01

	AXIS,		YAXIS=1,$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			COLOR=pen(1),$
			YTHICK=1.0,$
			YMINOR=2, $
			CHARSIZE=2.0,$
			YCHARSIZE=0.30

	CCG_PLOTGAP,	x,y,$
			gap=GAP,$
			LINESTYLE=0,$
			THICK=1,$
			COLOR=pen(pn+1)

	XYOUTS,		xmin+(xmax-xmin)*0.05,$
			ymax-(ymax-ymin)*0.25,$
			COLOR=pen(1),$
			/DATA,$
			CHARSIZE=1.0,$
			ALI=0.0,$
			STRCOMPRESS(refname(pn-1),/RE)
ENDFOR

PLOTS,	[x1,x1,x2,x2,x1],[top,y1,y1,top,top],$
	THICK=2.0,$
	COLOR=pen(13),$
	/NORMAL
;
;Bottom Right frame - Retention Times
;
top=fig_top-yfmax
left=fig_left+xdelta+0.02
;
FOR pn=1,npanes DO BEGIN

	x1=left
	x2=left+xdelta
	y1=top-(ydelta*pn)
	y2=top-(ydelta*(pn-1))

	j=WHERE(rt(pn-1,*)-1 GT DEFAULT)

	x=gmt(pn-1,j)
	y=rt(pn-1,j)
	;
	;Determine Y axis range
	;for peak heights
	;
	z=MOMENT(y,sdev=sdev)
	ymin=CCG_ROUND(z(0)-(5*sdev),-3)
	ymax=CCG_ROUND(z(0)+(5*sdev),-3)

	IF pn EQ npanes THEN xtickname=xlbl ELSE xtickname=nxlbl

	PLOT,		x,y,$
			COLOR=pen(1), $
			CHARSIZE=2.0,$
			CHARTHICK=1.0,$
			/NODATA, $
			/NOERASE, $
			POSITION=[x1,y1,x2,y2],$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			YTHICK=1.0,$
			YMINOR=2, $
			YCHARSIZE=0.01,$
			XSTYLE=1,$
			XRANGE=[xmin,xmax],$
			XTICKS=nhours-1,$
			XTICKNAME=xtickname,$
			XTHICK=1.0,$
			XMINOR=2, $
			XCHARSIZE=0.35

	AXIS,		YAXIS=1,$
			YSTYLE=1,$
			COLOR=pen(1),$
			YRANGE=[ymin,ymax],$
			YTHICK=1.0,$
			YMINOR=2, $
			CHARSIZE=2.0,$
			YCHARSIZE=0.30

	CCG_PLOTGAP,	x,y,$
			gap=GAP,$
			LINESTYLE=0,$
			THICK=1,$
			COLOR=pen(pn+1)

	XYOUTS,		xmin+(xmax-xmin)*0.05,$
			ymax-(ymax-ymin)*0.20,$
			COLOR=pen(1),$
			/DATA,$
			CHARSIZE=1.0,$
			ALI=0.0,$
			STRCOMPRESS(refname(pn-1),/RE)
ENDFOR

PLOTS,	[x1,x1,x2,x2,x1],[top,y1,y1,top,top],$
	THICK=2.0,$
	COLOR=pen(13),$
	/NORMAL
;
;Figure Title
;
XYOUTS,	0.5,0.97,/NORMAL,$
	STRUPCASE(rawfile),ALI=0.5,$
	COLOR=pen(1),$
	CHARSIZE=2.0
;
;X axis Title
;
fyear=MIN(year(WHERE(year GT 0)))

CCG_DEC2DATE,xmin,yr,mo,dy,hr
CCG_INT2MONTH,imon=mo,mon=mon,three=1
xtitle=STRCOMPRESS(STRING(dy)+' '+mon+' '+STRING(yr+fyear))
XYOUTS,	0.5,0.08,/NORMAL,$
	xtitle,$
	COLOR=pen(1),$
	ALI=0.5,CHARSIZE=2.0
;
;Y axis Title - Peak Heights
;
XYOUTS, fig_left*0.50,fig_top-yfmax/2.0,/NORMAL,$
	CHARSIZE=1.5,$
	COLOR=pen(1),$
	ALI=0.5,$
	ORI=90,$
	"PK HT (10!U6!n counts)"
;
;Y axis Title - Peak Areas
;
XYOUTS, fig_left*0.50,fig_top-3.0*yfmax/2.0,/NORMAL,$
	CHARSIZE=1.5,$
	COLOR=pen(1),$
	ALI=0.5,$
	ORI=90,$
	"PK AR (10!U6!n counts)"
;
;Y axis Title - Peak Widths
;
XYOUTS, (fig_left+0.02+2*xdelta)*1.085,fig_top-yfmax/2.0,/NORMAL,$
	CHARSIZE=1.5,$
	ALI=0.5,$
	COLOR=pen(1),$
	ORI=90,$
	"PK WTH (10!U-1!n minutes)"
;
;Y axis Title - Retention Times
;
XYOUTS, (fig_left+0.02+2*xdelta)*1.085,fig_top-3.0*yfmax/2.0,/NORMAL,$
	CHARSIZE=1.5,$
	ALI=0.5,$
	COLOR=pen(1),$
	ORI=90,$
	"RET TIME (seconds)"

IF KEYWORD_SET(update) THEN BEGIN
	;
	;Place raw file process summary on figure
	;
	GCRAW_SUMMARY,file=rawfile,site=site,sp=sp,arr=arr

	y=0.09
	FOR i=0,N_ELEMENTS(arr)-1 DO BEGIN
		XYOUTS, 0.02,y-(i*0.018),/NORMAL,$
			arr(i),$
			COLOR=pen(1),$
			ALI=0.0,$
			CHARSIZE=0.75
		PRINT,	arr(i)
	ENDFOR
ENDIF
;
;------------------------------------------------ ccg label
;
IF NOT KEYWORD_SET(ccg) THEN CCG_LABID,y=0.05,full=1
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PLOT GC MONTHLY DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
IF NOT KEYWORD_SET(dev) THEN WINDOW,	2,$
					xsize=500,ysize=600,$
					TITLE='GC monthly data '+rawfile
;
;Read temporary month file
;
z=CCG_GCISMONTH(file=monthfile,gmt=gmt,gmf=gmf,$
		htmr=htmr,armr=armr,flag=flag,/nomessages)
IF NOT z THEN RETURN
;
;Determine Y axis range
;for peak area mixing ratios
;
j=WHERE(flag NE '*')
z=MOMENT(armr(j),sdev=sdev)
ymin=CCG_ROUND(z(0)-(10*sdev),-3)
ymax=CCG_ROUND(z(0)+(10*sdev),-3)

PLOT,		gmf,armr,$
		/NODATA,$
		POSITION=[0.18,0.68,0.94,0.95],$
		CHARSIZE=2.0,$
		CHARTHICK=1.0,$
		TITLE=rawfile,$
		YSTYLE=1,$
		YRANGE=[ymin,ymax],$
		YTITLE=STRUPCASE(sp)+'!iAREA!n (ppb)',$
		YTHICK=1.0,$
		YMINOR=2, $
		YCHARSIZE=0.6,$
		XSTYLE=1,$
		XRANGE=[xmin,xmax],$
		XTICKS=nhours-1,$
		XTICKNAME=nxlbl,$
		XTHICK=1.0,$
		XMINOR=2, $
		XCHARSIZE=0.8,$
		COLOR=pen(1)

CCG_PLOTGAP,	gmf,armr,$
		gap=GAP,$
		LINESTYLE=0,$
		COLOR=pen(3)


k=WHERE(flag EQ '.')
IF k(0) NE -1 THEN BEGIN
	CCG_SYMBOL,	sym=2,fill=0
	OPLOT,		gmf(k),armr(k),$
			SYMSIZE=0.8,$
			COLOR=pen(3),$
			PSYM=8
ENDIF

k=WHERE(flag NE '.')
IF k(0) NE -1 THEN BEGIN
	CCG_SYMBOL,	sym=5,fill=1
print,xmin,xmax
print,gmf(k),armr(k)
	OPLOT,		gmf(k),armr(k),$
			SYMSIZE=1.0,$
			COLOR=pen(2),$
			PSYM=8
ENDIF
;
;Determine Y axis range
;for peak area minus peak height
;mixing ratios
;
diff=armr(j)-htmr(j)
z=MOMENT(diff,sdev=sdev)
ymin=CCG_ROUND(z(0)-(10*sdev),-3)
ymax=CCG_ROUND(z(0)+(10*sdev),-3)

PLOT,		gmf(j),diff,$
		/NODATA,$
		/NOERASE,$
		POSITION=[0.18,0.41,0.94,0.68],$
		CHARSIZE=1.8,$
		CHARTHICK=1.0,$
		YSTYLE=1,$
		YRANGE=[ymin,ymax],$
		YMINOR=2, $
		YTHICK=1.0,$
		YTITLE=STRUPCASE(sp)+'!iAREA!n - '+STRUPCASE(sp)+'!iHEIGHT!n (ppb)',$
		YCHARSIZE=0.6,$
		XSTYLE=1,$
		XTITLE=xtitle,$
		XRANGE=[xmin,xmax],$
		XTICKS=nhours-1,$
		XTHICK=1.0,$
		XTICKNAME=xlbl,$
		XMINOR=2, $
		XCHARSIZE=0.6,$
		COLOR=pen(1)

CCG_SYMBOL,	sym=2,fill=0

k=WHERE(flag(j) EQ '.')
IF k(0) NE -1 THEN BEGIN
	CCG_SYMBOL,	sym=2,fill=0
	OPLOT,		gmf(j(k)),diff(j(k)),$
			SYMSIZE=1.0,$
			COLOR=pen(3),$
			PSYM=8
ENDIF
;
k=WHERE(flag(j) NE '.')
IF k(0) NE -1 THEN BEGIN
	CCG_SYMBOL,	sym=5,fill=1
	OPLOT,		gmf(j(k)),diff(j(k)),$
			SYMSIZE=1.0,$
			COLOR=pen(2),$
			PSYM=8
ENDIF

OPLOT,		[xmin,xmax],[0,0],$
		LINESTYLE=1,$
		COLOR=pen(1)
;
;Should month files be plotted?
;
IF NOT nomonth THEN BEGIN
	;
	;Now read current and previous 
	;GC monthly data file.
	;
	CCG_DEC2DATE,	MIN(gmt),yr,mo,dy
	cyr=yr(0) & cmo=mo(0)

	IF cmo EQ 1 THEN lyr=cyr-1 ELSE lyr=cyr
	IF cmo EQ 1 THEN lmo=12 ELSE lmo=cmo-1
	currentmonth=	STRCOMPRESS(site+STRING(cyr)+STRING(FORMAT='(I2.2)',cmo)+$
			'.'+sp,/RE)
	lastmonth=	STRCOMPRESS(site+STRING(lyr)+STRING(FORMAT='(I2.2)',lmo)+$
			'.'+sp,/RE)

	file='/projects/'+sp+'/in-situ/'+site+'_data/month/'+lastmonth
	y=CCG_GCISMONTH(file=file,gmt=gmt,htmr=htmr,armr=armr,flag=flag,/nomessages)

	file='/projects/'+sp+'/in-situ/'+site+'_data/month/'+currentmonth
	z=CCG_GCISMONTH(file=file,gmt=a,htmr=b,armr=c,flag=d,/nomessages)

	IF NOT y AND NOT z THEN RETURN
	IF NOT y AND z THEN BEGIN 
		gmt=a
		htmr=b
		armr=c
		flag=d
	ENDIF
	IF y AND z THEN BEGIN 
		gmt=[gmt,a] 
		htmr=[htmr,b] 
		armr=[armr,c] 
		flag=[flag,d] 
	ENDIF
	;
	;Determine X axis range
	;for peak area mixing ratios
	;
	IF cmo EQ 12 THEN nmo=1 ELSE nmo=cmo+1
	IF cmo EQ 12 THEN nyr=cyr+1 ELSE nyr=cyr

	CCG_DATE2DEC,yr=lyr,mo=lmo,dy=1,hr=0,mn=0,dec=xmin
	CCG_DATE2DEC,yr=nyr,mo=nmo,dy=1,hr=0,mn=0,dec=xmax
	;
	;Determine Y axis range
	;for peak area mixing ratios
	;
	j=WHERE(flag NE '*')
	z=MOMENT(armr(j),sdev=sdev)
	ymin=CCG_ROUND(z(0)-(5*sdev),-3)
	ymax=CCG_ROUND(z(0)+(5*sdev),-3)
	CCG_INT2MONTH,imon=lmo,mon=lmon,three=1
	CCG_INT2MONTH,imon=cmo,mon=cmon,three=1
	tarr=[STRCOMPRESS(lmon+' '+STRING(lyr)),$
	      STRCOMPRESS(cmon+' '+STRING(cyr))]

	PLOT,		gmt,armr,$	
			/NODATA,$
			/NOERASE,$
			POSITION=[0.18,0.14,0.94,0.30],$
			YTITLE=STRUPCASE(sp)+'!iAREA!n (ppb)',$
			CHARSIZE=2.0,$
			YSTYLE=1,$
			YRANGE=[ymin,ymax],$
			YMINOR=2,$
			YCHARSIZE=0.6,$
			XSTYLE=1+4,$
			XRANGE=[xmin,xmax],$
			COLOR=pen(1)

	CCG_XLABEL,	x1=xmin,x2=xmax,y1=ymin,y2=ymax,$
			tarr=tarr,$
			COLOR=pen(1),$
			XTICKLEN=0.05,$
			XMINOR=4,$
			CHARSIZE=2.0
			
	k=WHERE(flag EQ '.')
	IF k(0) NE -1 THEN BEGIN
		CCG_SYMBOL,	sym=2,fill=0
		OPLOT,		gmt(k),armr(k),$
				COLOR=pen(3),$
				PSYM=3
	ENDIF

	k=WHERE(flag NE '.')
	IF k(0) NE -1 THEN BEGIN
		CCG_SYMBOL,	sym=5,fill=1
		OPLOT,		gmt(k),armr(k),$
				SYMSIZE=0.5,$
				COLOR=pen(2),$
				PSYM=1
	ENDIF
ENDIF
;
;------------------------------------------------ ccg label
;
IF NOT KEYWORD_SET(ccg) THEN CCG_LABID,y=0.05,full=1
;
;Clean up
;
SPAWN,	'rm -f '+tempfile
SPAWN,	'rm -f '+monthfile
;
;----------------------------------------------- close up shop
;
CCG_CLOSEDEV,dev=dev
END
