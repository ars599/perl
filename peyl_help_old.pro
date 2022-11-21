;+
; NAME:
;	PEYL_HELP
;
; PURPOSE:
; 	Provide general description of PEYL
;	library of IDL procedures.  Description
;	may be printed to the specified device.
;
; CALLING SEQUENCE:
;	PEYL_HELP
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
; RESTRICTIONS:
;	None.
;
;
;************************************************
PRO helplist_ev,ev
;************************************************
;
COMMON	list, libdir,prolist,helptext
COMMON  peyl_index, list_index
;
list_index=ev.index

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
OPENW,unit,GETENV("HOME")+'/.peyl_help',/GET_LUN
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
COMMON	device,		devtext

printbase=WIDGET_BASE(TITLE = 'Select Print Device',/COLUMN)
textbase=WIDGET_BASE(printbase,/ROW)
devlbl=WIDGET_LABEL(textbase,VALUE='Select Device ')
devtext=WIDGET_TEXT(textbase,VALUE='lj -onb',XSIZE=30,/EDITABLE)
respbase=WIDGET_BASE(printbase,/ROW)
printbtn=WIDGET_BUTTON(respbase,VALUE='Print',EVENT_PRO='printbtn_ev')
cancelbtn=WIDGET_BUTTON(respbase,VALUE='Cancel',EVENT_PRO='cancelbtn_ev')
WIDGET_CONTROL, printbase, /REALIZE
END
;
;************************************************
PRO	printbtn_ev,ev
;************************************************
;
COMMON	device,	devtext

	WIDGET_CONTROL, devtext,GET_VALUE=dev
	;
	;Print temporary file
	;
	SPAWN,STRCOMPRESS('lp -d'+dev+' '+GETENV("HOME")+'/.peyl_help')
	WIDGET_CONTROL, ev.top, /DESTROY
END
;
;************************************************
PRO	cancelbtn_ev,ev
;************************************************
;
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
	SPAWN,STRCOMPRESS('rm -f '+GETENV("HOME")+'/.peyl_help')
END
;
;************************************************
PRO	modifbtn_ev,ev
;************************************************
;
COMMON	list,	libdir,prolist,helptext
COMMON  peyl_index,  list_index
;
	if GETENV("USER") ne 'peylin' then begin
	   print,'YOU DON"T HAVE RIGHT TO MODIFIE...'
	   return
	endif
	if list_index ne -1 then begin
	   file=STRCOMPRESS(libdir+STRLOWCASE(prolist(list_index))+'.pro',/REMOVE_ALL)
	   action='\emacs -fg white -bg black -geometry 75x40+250+60 '+file+' &'
	   print,action
	   SPAWN,action
	endif else begin
	   print,'NO FILE SELECTED'
	endelse
END
;
;************************************************
PRO 	peyl_help
;************************************************
;
COMMON	list,	libdir,prolist,helptext
COMMON  peyl_index,  list_index
list_index=-1
;
;get procedure list
;
SET_PLOT,	'X'
;libdir='/usr/local/IDL_4.0/idl_4/lib/ccglib/'
libdir='/home/users/peylin/idl/lib/PEYL/'

;CCG_DIRLIST,dir=STRCOMPRESS(libdir+'ccg_*.pro',/REMOVE_ALL),omitdir=1,prolist
CCG_DIRLIST,dir=STRCOMPRESS(libdir+'*.pro',/REMOVE_ALL),omitdir=1,prolist
FOR i=0,N_ELEMENTS(prolist)-1 DO $
	prolist(i)=STRUPCASE(STRMID(prolist(i),0,STRPOS(prolist(i),'.')))
;
helpbase=WIDGET_BASE(TITLE = 'PEYL Help',/ROW)
leftbase=WIDGET_BASE(helpbase,/COLUMN)
helplist=WIDGET_LIST(leftbase,YSIZE=35,VALUE=prolist,EVENT_PRO='helplist_ev')
helptext=WIDGET_TEXT(helpbase,YSIZE=40,XSIZE=80,/SCROLL)
prnbtn=WIDGET_BUTTON(leftbase,VALUE='Print ...',EVENT_PRO='printmenu_ev')
donebtn=WIDGET_BUTTON(leftbase,VALUE='Done',EVENT_PRO='donebtn_ev')
modifbtn=WIDGET_BUTTON(leftbase,VALUE='Modifie',EVENT_PRO='modifbtn_ev')
;
WIDGET_CONTROL, helpbase, /REALIZE
XMANAGER, 	'peyl_help', helpbase
END
;-

