;+
; NAME:
;	CCG_HELP
;
; PURPOSE:
; 	Provide general description of CCG
;	library of IDL procedures.  Description
;	may be printed to the specified device.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	CCG_HELP
;
; INPUTS:
;	None.
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	IDL> CCG_HELP
;
; MODIFICATION HISTORY:
;	Written, KAM, September 1994.
;-
;
;************************************************
PRO helplist_ev,ev
;************************************************
;
COMMON	list,	libdir,prolist,helptext
;
i=0 & s=''
file=STRCOMPRESS(libdir+STRLOWCASE(prolist(ev.index))+'.pro',/REMOVE_ALL)
nlines=CCG_LIF(file=file)
str=MAKE_ARRAY(nlines,/STR,VALUE='')
;
OPENR,	unit,file,/GET_LUN
WHILE NOT EOF(unit) DO BEGIN
	READF,unit,s
	str(i)=s
	i=i+1
ENDWHILE
FREE_LUN,unit
;
b=WHERE(str EQ ';+')
e=WHERE(str EQ ';-')
;
;Write to temporary file
;
OPENW,unit,GETENV("HOME")+'/.ccg_help',/GET_LUN
FOR i=b(0),e(0) DO PRINTF,unit,FORMAT='(A0)',str(i)
FREE_LUN,unit
;
IF b(0) EQ -1 OR e(0) EQ -1 THEN BEGIN
	WIDGET_CONTROL,helptext,$
	SET_VALUE='No help on this procedure ...'
ENDIF ELSE BEGIN
	WIDGET_CONTROL,helptext,SET_VALUE=str(b(0):e(0))
ENDELSE
END
;
;************************************************
PRO	printmenu_ev,ev
;************************************************
;
COMMON	device,	devtext,cancel
COMMON	misc,	font

printbase=WIDGET_BASE(TITLE = 'Select Print Device',/COLUMN)
textbase=WIDGET_BASE(printbase,/ROW)
devlbl=WIDGET_LABEL(textbase,VALUE='Select Device ',FONT=font)
devtext=WIDGET_TEXT(textbase,VALUE='lp -dhp3 -onb ',XSIZE=30,/EDITABLE,FONT=font)
respbase=WIDGET_BASE(printbase,/ROW)
print=WIDGET_BUTTON(respbase,VALUE='Print',EVENT_PRO='printbtn_ev',FONT=font)
cancel=WIDGET_BUTTON(respbase,VALUE='Cancel',EVENT_PRO='printbtn_ev',FONT=font)
WIDGET_CONTROL, printbase, /REALIZE
END
;
;************************************************
PRO	printbtn_ev,ev
;************************************************
;
COMMON	device,		devtext,cancel


CASE ev.id OF
        cancel:         
        ELSE:           BEGIN
			WIDGET_CONTROL, devtext,GET_VALUE=dev
			;
			;Print temporary file
			;
			SPAWN,$
			STRCOMPRESS(dev+' '+GETENV("HOME")+'/.ccg_help')
                        END
ENDCASE
	WIDGET_CONTROL, ev.top, /DESTROY
END
;
;************************************************
PRO	donebtn_ev,ev
;************************************************
;
	WIDGET_CONTROL, ev.top, /DESTROY
	;
	;Remove temporary file
	;
	SPAWN,STRCOMPRESS('rm -f '+GETENV("HOME")+'/.ccg_help')
END
;
;************************************************
PRO 	ccg_help,file=file
;************************************************
;
COMMON	list,	libdir,prolist,helptext
COMMON	misc,	font

IF KEYWORD_SET(file) THEN BEGIN
	CCG_SHOWDOC,file=file
	RETURN
ENDIF
;
;get procedure list
;
;font='*times-medium-r-*140*'
;font='courB14'
SET_PLOT,	'X'

;;;libdir=!DIR+'/lib/ccglib/'
libdir='/home/gcarb/contrib/idllib/ccglib/'

CCG_DIRLIST,dir=STRCOMPRESS(libdir+'ccg_*.pro',/REMOVE_ALL),omitdir=1,prolist
FOR i=0,N_ELEMENTS(prolist)-1 DO $
	prolist(i)=STRUPCASE(STRMID(prolist(i),0,STRPOS(prolist(i),'.')))
;
helpbase=WIDGET_BASE(TITLE = libdir+'CCG HELP',/ROW)
leftbase=WIDGET_BASE(helpbase,/COLUMN)
helplist=WIDGET_LIST(leftbase,YSIZE=25,VALUE=prolist,EVENT_PRO='helplist_ev',FONT=font)
helptext=WIDGET_TEXT(helpbase,YSIZE=30,XSIZE=80,/SCROLL,FONT=font)
prnbtn=WIDGET_BUTTON(leftbase,VALUE='Print ...',EVENT_PRO='printmenu_ev',FONT=font)
donebtn=WIDGET_BUTTON(leftbase,VALUE='Done',EVENT_PRO='donebtn_ev',FONT=font)
;
WIDGET_CONTROL, helpbase, /REALIZE
XMANAGER, 	'ccg_help', helpbase
END
