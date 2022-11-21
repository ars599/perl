;+
;=========================================================================
; NAME:
;       PEYL_GRIDPRINT
;
; PURPOSE:
;       Write an array to a file with some specific feature
;	like formatted or unformatted.
;	specialy for geophysical fields (lon, lat, "mon")
;	Write either integer or floating..
;
; CALLING SEQUENCE:
;       peyl_gridprint, file_name, ff, date=date, unform=unform, 
;			double=double, f77=f77, /noinfo
;
; INPUTS:
;      file_name : path of the file to be created.
;
;      ff : array to be print out of dimension 1,2 or 3.
;	    if dim=3 then make a loop over the third dim
;	    to write each subset of the array ff.
;
; OPTIONAL INPUT PARAMETERS:
;      /date : if set then write the number of the month
;	       before writing the corresponding subset of data.
;	       Date contain these number at then end (reals from 1 to n)
;
;      /unform : if set then write unformatted data else free formatted.
;	         
;      /f77 : if set then print f77_unformatted data.
;
;      /double : if set force the output to be double-precision for floating
;		 else alway write single precision
;
;      /noinfo : to suppress messages.
;
;      /block3d: set if set a 3-D matrice as only one block..
;
; OUTPUTS:
;      a file..
;       
; RESTRICTIONS:
;      NO
;
;=========================================================================

pro peyl_gridprint, file_name, ff, date=date, unform=unform, $
		   double=double, f77=f77, noinfo=noinfo, block3d=block3d,$
                   swap_endian = swap_endian

if keyword_set(noinfo) then info=0 else info=1
if keyword_set(swap_endian) then swap_endian=1 else swap_endian=0

;---------- Determination des dimensions 
; (n_ecrit = nombre de mois a ecrire...)
s=size(ff)
n_dim=s(0)
n_type=s(n_dim+1)
n_ecrit=1
if (n_dim eq 3) then begin
   n_ecrit = s(n_dim)
   if keyword_set(block3d) then begin
      if keyword_set(date) then begin
	 print,'Impossible : block3d et date...'
	 stop
      endif
      n_ecrit = 1
      n_dim   = 2
   endif
endif

;-------- Check globaux
if (n_type ne 4 and n_type ne 2 and n_type ne 5) then begin
   print, 'ERREUR PEYL_GRIDPRINT : wrong type of data :',n_type
   stop
endif
if (n_dim le 0 or n_dim gt 3) then begin
   print, 'ERREUR PEYL_GRIDPRINT : dimension of data invalide :',n_dim
   stop
endif

;-------- Gestion de l'ecriture d'une date en plus 
;          ecriture de floating : 1. a n_ecrit...
; REM : la variable date contiendra en retour les datent ecritent..
;
ecriture_date=0
if (keyword_set(date)) then begin
   ecriture_date=1
   if (n_elements(date(*,0)) ne n_ecrit) then $
     date=findgen(n_ecrit)+1. 
endif

;-------- Gestion du format d'ecriture : formate ou non??
;         Et ouverture du fichier
;
if (keyword_set(unform)) then unform=1 else unform=0
if (keyword_set(f77)) then begin
   unform=1
   if swap_endian then openw,unit,file_name,/get_lun,/f77_unformatted,/SWAP_ENDIAN $
   else openw,unit,file_name,/get_lun,/f77_unformatted
endif else begin
    if unform then begin
        if swap_endian then openw,unit,file_name,/get_lun,/SWAP_ENDIAN $
        else openw,unit,file_name,/get_lun
    endif else begin
        width = 17 * s(1)
        openw,unit,file_name,width=width,/get_lun
    endelse
endelse

;-------------------------------------------------------------------- 
;			Ecriture du fichier
;
;-------- Gestion du type de donnees a lire...
; REM : xx represente le type ecrit et ff les donnes stoquees.. 
;       il y a conversion dans le cas ou le keyword "double" 
;       est selectionne mais ou ff est un simple reel..
;	Si ff est un double mais sans le keyword "double" on ecrit des 
;	simple (conversion)..

case 1 of 
    n_dim le 2 : begin
        if (keyword_set(double)) then xx=double(ff) $ 
        else xx = float(ff)                  
        if (unform eq 1) then begin
            if ecriture_date then writeu,unit,date
            writeu,unit,xx 
        endif else begin
            if ecriture_date then printf,unit,date
            printf,unit,xx 
        endelse
    end
    n_dim eq 3 : begin
        for k=0,n_ecrit-1 do begin
            if (keyword_set(double)) then xx=double(ff(*,*,k)) $ 
            else xx = float(ff(*,*,k))                  
            if (unform eq 1) then begin
                if ecriture_date then writeu,unit,date(k,*)
                writeu,unit,xx
            endif else begin 
                if ecriture_date then printf,unit,date(k,*)
                printf,unit,xx
            endelse 
        endfor
    end
endcase
;
if info then begin 
   print,'File writed :',file_name
   print,'   dim = ',auto_string(s(1:n_dim),0)
   if ecriture_date then begin
      print,'   dates ecrite = ',auto_string(date(0:n_ecrit-1,0),0)
   endif
endif 
;
free_lun,unit
end
;-





