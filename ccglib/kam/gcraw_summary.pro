PRO	GCRAW_SUMMARY,$
	file=file,$
	sp=sp,$
	site=site,$
	arr=arr
;
;Read the appropriate GC in situ process summary file
;
j=CCG_STRRPOS(file,'/')
IF j NE -1 THEN rawfile=STRMID(file,CCG_STRRPOS(file,'/')+1,1000) ELSE rawfile=file

sp=STRMID(rawfile,CCG_STRRPOS(rawfile,'.')+1,1000)
year=STRMID(rawfile,0,4)
;
;Build log file name
;
logfile='/projects/'+$
	sp+$
	'/in-situ/'+$
	STRLOWCASE(site)+$
	'_data/raw/'+$
	year+$
	'/'+$
	year+$
	'.log.'$
	+sp
;
;Read logfile
;
s=''
OPENR,unit,logfile,ERROR=err,/GET_LUN
IF err THEN RETURN

n=CCG_LIF(file=logfile)

entry=''
WHILE NOT EOF(unit) DO BEGIN
	READF,unit,s
	IF STRPOS(s,rawfile) NE -1 THEN entry=s
ENDWHILE
FREE_LUN,unit
;
;Was a match found?
;
IF entry EQ '' THEN RETURN
;
;Now parse entry
;This is species specific.
;
refheader='  ref id'+'    pkht mn'+'    pkht sd'+$
                     '    pkar mn'+'    pkar sd'+'   #'
CASE sp OF
'ch4':	BEGIN
	nrefs=1
	flags= 	'     .'+'     *'+'     t'+'     +'+$
		'     %'+'     {'+'     ['+'     ]'+'     }'+'     #'+$
		'     ~'
	END
'co':	BEGIN
	nrefs=3
	flags= 	'     .'+'     *'
	END
'h2':	BEGIN
	flags= 	'     .'+'     *'
	nrefs=3
	END
ENDCASE

arr=STRARR(nrefs+3)
fchar=30
charstep=56

arr(0)=refheader
FOR i=0,nrefs-1 DO BEGIN
	arr(i+1)=STRMID(entry,fchar+(i*charstep),charstep)
ENDFOR
arr(nrefs+1)=flags
arr(nrefs+2)=STRMID(entry,fchar+(i*charstep),1000)
END
