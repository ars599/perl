;+
; NAME:
;	CCG_TMPNAM
;
; PURPOSE:
;	Return a temporary file name that points
;	to the user's HOME directory.  Naming 
;	scheme is $HOME/tmp.#### where #### is the 
;	next consecutive number number beginning with
;	zero (0).  The user is responsible for
;	managing the file.
;
;	WARNING: Even if the temporary file is not used, it exists.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	tmpfile=CCG_TMPNAM()
;	r=CCG_TMPNAM('/tmp')
;	file=CCG_TMPNAM('/usr/tmp/')
;
; INPUTS:
;	None.
;
; OPTIONAL INPUT PARAMETERS:
;	A directory path to associate with the temporary 
;	file name.  If no argument is specified, the user's 
;	HOME directory is used.
;
; OUTPUTS:
;	result:	String constant containing the temporary file name.  
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
;	Example 1:
;		IDL> PRINT,CCG_TMPNAM('/tmp')
;		IDL> /tmp/tmp.0000	if tmp.0000 doesn't already exist.
;
;	Example 2:
;
;		IDL> tmp1=CCG_TMPNAM()
;		IDL> PRINT,tmp1
;		IDL> $HOME/tmp.0000
;		IDL> CCG_SWRITE,file=tmp1,'test1'
;		IDL> tmp2=CCG_TMPNAM()
;		IDL> PRINT,tmp2
;		IDL> $HOME/tmp.0001
;		IDL> CCG_SWRITE,file=tmp2,'test2'
;		.
;		.
;		.
;		IDL> SPAWN, 'rm tmp.0000 tmp.0001'
;
; MODIFICATION HISTORY:
;	Written, KAM, November 1996.
;-
;
FUNCTION	CCG_TMPNAM,$
		dir

IF N_PARAMS() EQ 0 THEN $
	dir=GETENV("HOME")+'/' ELSE $
	IF CCG_STRRPOS(dir,'/') NE STRLEN(dir)-1 THEN dir=TEMPORARY(dir)+'/'
;
;Generate file number
;
i=0
REPEAT BEGIN
	f=dir+'tmp.'+STRCOMPRESS(STRING(FORMAT='(I4.4)',i),/RE)
	i=i+1
	j=FINDFILE(f)
ENDREP UNTIL j(0) EQ ''

SPAWN,'touch '+f
RETURN,f
END
