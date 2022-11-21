;+
; NAME:
;	CCG_FREAD	
;
; PURPOSE:
; 	Read 'nc' columns of integers
;	or real numbers from the specified
;	file.
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
;	CCG_FREAD,file=filename,nc=3,skip=3,/nomessages,var
;	CCG_FREAD,file='/home/gcarb/data/ch4/in-situ/brw_data/year/brw93.ch4',nc=6,var
;	CCG_FREAD,file='/users/ken/mlo.co2',nc=12,skip=20,result
;
; INPUTS:
;	file:	  	source file name.
;
;	nc:		number of columns in the file.
;			columns must contain integers
;			or real numbers.
;
; OPTIONAL INPUT PARAMETERS:
;
;	skip:		integer specifying the number
;			of lines to skip at the beginning 
;			of the file. 
;
;	nomessages:	If non-zero, messages will be suppressed.
;
; OUTPUTS:
;	result:		The double precision array 
;			containing variables from file,
;			i.e., results(number of columns,number of lines read)
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Column values of rows not skipped 
;	must be integer or real numbers.
;
; PROCEDURE:
;
;		Example:
;			CCG_FREAD,file='testfile',nc=2,res
;			.
;			.
;			.
;			PLOT, res(0,*),res(1,*)
;			.
;			.
;			.
;			END
;
;		
; MODIFICATION HISTORY:
;	Written,  KAM, January 1994.
;	Modified, KAM, April 1994.
;-
;
PRO PEYL_FREAD,	file=file,$
		nc=nc,$
		skip=skip,$
		nomessages=nomessages,$
		var
;
;-----------------------------------------------check input information 
;
IF NOT KEYWORD_SET(file) OR NOT KEYWORD_SET(nc) THEN BEGIN
	CCG_MESSAGE,"File and number of columns must be specified.  Exiting ..."
	CCG_MESSAGE,"(ex) CCG_FREAD,file='/users/ken/test',nc=3,result"
	RETURN
ENDIF
;
IF NOT KEYWORD_SET(skip) THEN skip=0
IF NOT KEYWORD_SET(nomessages) THEN nomessages=0 ELSE nomessages=1
;
;Determine number of
;lines in file.
;
nr=CCG_LIF(file=file)
;
IF nr LE 0 THEN BEGIN
    print,'Probleme de lecture dans peyl_fread !!'
    stop
	var=0
	RETURN
ENDIF
;
;dimension array
;
var=DBLARR(nc,nr-skip)
v=DBLARR(nc)
s=''
;

;ccg_dirlist,dir=file,list
;if n_elements(list) ne 1 then message,'!! Probleme : pas de fichier valide ...'
;if n_elements(list) gt 1 then message,'!! Probleme : plusieurs fichiers...'
list = file(0)

OPENR, 	unit, list(0),/GET_LUN
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
	READF,unit,v
	var(0:nc-1,i)=v
	i=i+1
ENDWHILE
FREE_LUN, unit
;
IF NOT nomessages THEN CCG_MESSAGE,'Done reading '+file+' ...'
END
