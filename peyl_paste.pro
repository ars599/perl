;+
;========================================================================
; NAME:
;       PEYL_PASTE
;
; PURPOSE:
;       Paste several ASCII files.. like the unix paste comande
;       but not limited by numbre of caracters for a line..
;       given a separator field.
;       
;
; CALLING SEQUENCE:
;       ncol = peyl_paste(filein, fileou)
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
PRO PEYL_PASTE, filein, $
                fileout


;--- Number of input file
nfile = n_elements(filein)


;--- Determine max caracteres of output file per ligne and max number
;    of ligne between input files..
nlenmax = 0
nligmax = 0
for n=0,nfile-1 do begin    
    peyl_colif, filein(n), nlen=nlen, nlig=nlig, ncol=ncol, /allscan
    nlenmax = nlenmax + max(nlen) + 1
    nligmax = max([nligmax,nlig])
endfor


;--- Open all input files + output file

nlenmax = nlenmax + 10   ;--- for security...
u = 50+indgen(nfile)
for n=0,nfile-1 do begin 
    openr,u(n),filein(n)
endfor
openw,u2,fileout,width=nlenmax,/get_lun

;---- Read input files and print output: line by line

lect=''
for nl=0,nligmax-1 do begin    

    out = ''
    for n=0,nfile-1 do begin

        if not eof(u(n)) then begin
            readf,u(n),lect
            out = out + lect + ' '
        endif
        
    endfor

    printf,u2,strmid(out,0,strlen(out)-1)
    
endfor

free_lun,u2
for n=0,nfile-1 do free_lun,u(n)
END
;-


