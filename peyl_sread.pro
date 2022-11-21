;+
; NAME:
;	PEYL_SREAD	
;
; PURPOSE:
; 	Read columns of characters
;	from the specified file.
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
;	PEYL_COLREAD,file=filename,skip=3,messages=1,var
;
; INPUTS:
;	file:	  	source file name.
;
; OPTIONAL INPUT PARAMETERS:
;
;	skip:		integer specifying the number
;			of lines to skip at the beginning 
;			of the file. 
;
;	noinfo:	        If non-zero, messages will be
;			suppressed.
;
; OUTPUTS:
;	result:		The character array 
;			containing variables from file,
;			i.e., results(number of columns,number of lines read)
;
;           nc:         number of column readed
;           nl:         number of ligne readed
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Column values of rows not skipped 
;
;		
; MODIFICATION HISTORY:
;	Modified, PEYL, July 2001.
;
;
PRO PEYL_SREAD,	file=file,nc=nc,nl=nr,skip=skip,noinfo=noinfo,var,$
                comment=comment,separator=separator,entete=entete
;
;-----------------------------------------------check input information 
;

if not keyword_set(comment) then comment=''
if not keyword_set(separator) then separator=' '

IF NOT KEYWORD_SET(file) THEN BEGIN
	CCG_MESSAGE,"File must be specified.  Exiting ..."
	RETURN
ENDIF
;
IF NOT KEYWORD_SET(skip) THEN skip=0
IF NOT KEYWORD_SET(noinfo) THEN noinfo=0
;
;Determine number of
;lines in file.
;
;nr=CCG_LIF(file=file)
OPENR, 	unit, file,/GET_LUN
s=''
nr=0L
WHILE NOT EOF(unit) DO BEGIN
    READF,unit,s
    nr=nr+1
ENDWHILE
point_lun,unit,0
;
IF nr LT 0 THEN BEGIN
	var=0
	RETURN
ENDIF
;

;;OPENR, 	unit, file,/GET_LUN
;
IF NOT noinfo THEN CCG_MESSAGE,'Reading '+file+' ...'
;
; Get the number of column
s=''
for n=0,skip-1 do READF,unit,s
READF,unit,s
v = str_sep(strcompress(strtrim(s,2)),separator)
nc = n_elements(v)
point_lun,unit,0

; dimension array 
;
var = STRARR(nc,nr-skip)
v=STRARR(nc)

;Skip [skip] number of lines
;
IF KEYWORD_SET(skip) THEN BEGIN
    FOR i=0,skip-1 DO BEGIN
        READF,unit,s
        if i eq 0 then begin
            foo = str_sep(strcompress(strtrim(s,2)),separator)
            entete = foo
        endif
    ENDFOR
ENDIF 
;
;Read data
;
i=0L
WHILE NOT EOF(unit) DO BEGIN
	READF,unit,s
        v = str_sep(strcompress(strtrim(s,2)),separator)
        if (s eq '' or v(0) eq comment) then goto,suite
        if n_elements(v) ne nc then begin
           print,'Erreur in pey_colread Nb col wrong..',n_elements(v),nc
           print,'Erreur in pey_colread Nb col wrong..',s
           stop
        endif
	var(0:nc-1,i)=v
	i=i+1
        suite:
ENDWHILE
FREE_LUN, unit

var = var(*,0:i-1)
nr = i

END
;-



