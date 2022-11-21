



;+
; NAME:
;	PEYL_COLREAD	
;
; PURPOSE:
; 	Read 'nc' columns of characters
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
;	PEYL_COLREAD,file=filename,nc=3,skip=3,messages=1,var
;	PEYL_COLREAD,file='brw93.ch4',nc=6,var
;	PEYL_COLREAD,file='/users/ken/mlo.co2',nc=12,skip=20,result
;
; INPUTS:
;	file:	  	source file name.
;
;	nc:		number of columns in the file.
;
; OPTIONAL INPUT PARAMETERS:
;
;	skip:		integer specifying the number
;			of lines to skip at the beginning 
;			of the file. 
;
;	messages:	If non-zero, messages will be
;			suppressed.
;
;       separator:      Specify separator, ' ' by default
;
; OUTPUTS:
;	result:		The character array 
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
;
;		
; MODIFICATION HISTORY:
;	Written,  KAM, January 1994.
;	Modified, PEYL, December 1997.
;
;
PRO PEYL_COLREAD,       file=file,nc=nc,skip=skip,messages=messages, $
                                separator=separator,var, comment=comment
;
;-----------------------------------------------check input information 
;

if not keyword_set(comment) then comment=''

IF NOT KEYWORD_SET(file) OR NOT KEYWORD_SET(nc) THEN BEGIN
        CCG_MESSAGE,"File and number of columns must be specified.  Exiting ..."
        RETURN
ENDIF
;
IF NOT KEYWORD_SET(skip) THEN skip=0
IF NOT KEYWORD_SET(messages) THEN messages=0
if not keyword_set(separator) then separator=' '
;
;Determine number of
;lines in file.
;
nr=CCG_LIF(file=file)
;
IF nr LT 0 THEN BEGIN
        var=0
        RETURN
ENDIF
;
;dimension array
;
var=STRARR(nc,nr-skip)
v=STRARR(nc)
s=''
;
OPENR,  unit, file,/GET_LUN
;
IF NOT messages THEN CCG_MESSAGE,'Reading '+file+' ...'
;
;Skip [skip] number of lines
;
IF KEYWORD_SET(skip) THEN FOR i=0L,skip-1 DO READF,unit,s
;
;Read data
;
i=0L
WHILE NOT EOF(unit) DO BEGIN
        READF,unit,s
        v = str_sep(strcompress(strtrim(s,2)),separator)
        if (s eq '' or v(0) eq comment) then goto,suite
        if n_elements(v) ne nc then begin
           print,'Erreur in pey_colread Nb col wrong..',v,nc
           stop
        endif
        var(0:nc-1,i)=v
        i=i+1
        suite:
ENDWHILE
FREE_LUN, unit

var = var(*,0:i-1)

;
IF NOT messages THEN CCG_MESSAGE,'Done reading '+file+' ...'
END
;-




