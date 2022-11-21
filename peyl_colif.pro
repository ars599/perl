;+
;========================================================================
; NAME:
;       PEYL_COLF
;
; PURPOSE:
;       Determine the numver of colomnes of ASCII file
;       given a separator field.
;       
;
; CALLING SEQUENCE:
;       ncol = peyl_colf(file, sep=' ')
;
; INPUTS:
;     
;
; OPTIONAL INPUT PARAMETERS:
; 
;
; OUTPUTS:
;
;       
; RESTRICTIONS:
; 
;
;========================================================================
PRO PEYL_COLIF, file, $
        ncol = ncol, $
        nlig = nlig, $
        nlen = nlen, $
        sep = sep, $
        allscan = allscan


;-- check input parametes

if not keyword_set(sep) then sep = ' '
if not keyword_set(allscan) then allscan = 0

;--- Compute number of lines..

nlig = ccg_lif(file=file)


;--- Define outputs 

if allscan then nlig_loc = nlig $
else nlig_loc = 1
ncol = intarr(nlig_loc)
nlen = intarr(nlig_loc)

;---- Open and Read file 

openr,u,file,/get_lun
lect=''
for l=0,nlig_loc-1 do begin

    readf,u,lect
    nlen(l) = strlen(lect)
    if sep eq ' ' then lect = strcompress(strtrim(lect,2))
    ncol(l) = n_elements( str_sep (lect, sep) )

endfor

free_lun,u
END
;-


