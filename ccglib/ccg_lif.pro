;+
; NAME:
;	CCG_LIF	
;
; PURPOSE:
; 	Determine the number of lines (rows) in
;	the passed file.
;
;	Returns the number of lines in the passed
;	file as an integer.
;
; CATEGORY:
;	Text Files.
;
; CALLING SEQUENCE:
;	result=CCG_LIF(file='filename')
;	result=CCG_LIF(file='/home/gcarb/data/co/flask/site/brw.co')
;
; INPUTS:
;	file:	Specified file.
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
;		Example:
;			.
;			.
;			.
;			nlines=CCG_LIF(file='/home/gcarb/data/co2/flask/site/brw.co2')
;			array=MAKE_ARRAY(nlines,/STR,VALUE='')
;			.
;			.
;			.
;		
; MODIFICATION HISTORY:
;	Written, KAM, August 1994.
;-
;
FUNCTION	CCG_LIF,	file=file
;
;Verify that file is passed
;
IF NOT KEYWORD_SET(file) THEN BEGIN
	CCG_MESSAGE,	'File name must be passed.  Exiting ...'
	CCG_MESSAGE,	"(ex)  nlines=CCG_LIF(file='/home/gcarb/data/co/flask/site/brw.co2')"
	RETURN,0
ENDIF
;

;;SPAWN, 'wc -l < '+file,s
;;RETURN, LONG(s(0))



;; Changed by M.Ramonet, 7 Sep 2000
SPAWN, 'wc -l < '+file,s
str0 = str_sep(s(0),' ')
w0=where(str0 ne '',nstr0)
if nstr0 le 1 then begin
    RETURN,        LONG(s(0))
endif else begin
    str1 = str_sep(s(1),' ')
    w1=where(str1 ne '',nstr1)
    RETURN, LONG(s(1))
endelse

END
