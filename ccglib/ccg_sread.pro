;+
; NAME:
;	CCG_SREAD	
;
; PURPOSE:
; 	Read strings from the specified	file.
;
;	User may specify 'skip' lines to 
;	skip at the beginning of file.
;
;	User may suppress messages.
;
;
; CATEGORY:
;	Text Files.
;
; CALLING SEQUENCE:
;	CCG_SREAD,file=filename,skip=3,/nomessages,arr
;	CCG_SREAD,file='/home/gcarb/data/ch4/in-situ/mlo_data/month/mlo199512.ch4',result
;	CCG_SREAD,file='readme',/nomessages,skip=20,result
;
; INPUTS:
;	file:	  	source file name.
;
; OPTIONAL INPUT PARAMETERS:
;	skip:		integer specifying the number
;			of lines to skip at the beginning 
;			of the file. 
;
;	nomessages:	If non-zero, messages will be suppressed.
;
; OUTPUTS:
;	result:		The string vector containing contents (by line)
;			from the file,e.g., result(number of lines read).
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
;
;		Example:
;			CCG_SREAD,file='testfile',skip=2,result
;			.
;			.
;			PRINT,result
;			.
;			.
;			.
;			END
;
;		
; MODIFICATION HISTORY:
;	Written,  KAM, December 1995.
;-
;
PRO CCG_SREAD,	file=file,$
		skip=skip,$
		nomessages=nomessages,$
		arr
;
;-----------------------------------------------check input information 
;
IF NOT KEYWORD_SET(file) THEN BEGIN
	CCG_MESSAGE,"File  must be specified.  Exiting ..."
	CCG_MESSAGE,"(ex) CCG_SREAD,file='/users/ken/test',arr"
	RETURN
ENDIF
;
IF NOT KEYWORD_SET(skip) THEN skip=0
IF NOT KEYWORD_SET(nomessages) THEN nomessages=0
;
;Determine number of
;lines in file.
;
nr=CCG_LIF(file=file)

IF nr LE 0 THEN BEGIN
	arr=''
	RETURN
ENDIF
;
;dimension array
;
arr=STRARR(nr-skip)
s=''
;
OPENR, 	unit,file,/GET_LUN
;
IF NOT nomessages THEN CCG_MESSAGE,'Reading '+file+' ...'
;
;Skip [skip] number of lines
;
IF KEYWORD_SET(skip) THEN FOR i=0,skip-1 DO READF,unit,s
;
;Read data
;
i=0L
WHILE NOT EOF(unit) DO BEGIN
	READF,unit,s
;;Print,'I=',i
	arr(i)=s
	i=i+1
ENDWHILE
FREE_LUN, unit
;
IF NOT nomessages THEN CCG_MESSAGE,'Done reading '+file+' ...'
END
