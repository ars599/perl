;+
;=========================================================================
; NAME:
;       PEYL_GRIDREAD
;
; PURPOSE:
;       Read a geophysical array (lon, lat, "mon") from a file 
;	with some specific feature like formatted or unformatted data
;	and also f77_unformated..
;	READ FILE WRITEN BY PEYL_GRIDPRINT.
;
; CALLING SEQUENCE:
;       peyl_gridread, file_name, ff, date=date, unform=unform, 
;			double=double, f77=f77, /noinfo
;
; INPUTS:
;      file_name : path of the file to be read.
;
;      ff : array to read in (dimension 1,2 or 3).
;	    if dim=3 then make a loop over the third dim
;	    to read each subset of the array ff.
;
; OPTIONAL INPUT PARAMETERS:
;      /date : if set then read the number of the month
;	       before reading the corresponding subset of data.
;	       Date contain these number at then end (reals from 1 to n)
;
;      /unform : if set then read unformatted data else free formatted.
;
;      /f77 : if set then read f77_unformatted data.
;	         
;      /double : if set force to read double-precision for floating
;		 and then convert to the type of ff.
;
;      /noinfo : to suppress messages.
;
;      /block3d: set if read a 3-D matrice as only one block..
;
; OUTPUTS:
;      ff with the good values..
;       
; RESTRICTIONS:
;      
;=========================================================================

pro peyl_gridread, file_name, ff, date=date, unform=unform, f77=f77, $
		   double=double, noinfo=noinfo, block3d=block3d,$
                   swap_endian = swap_endian

if keyword_set(noinfo) then info=0 else info=1
if keyword_set(swap_endian) then swap_endian=1 else swap_endian=0

;------- Recherche des dimensions a lire
; (n_lect = nombre de mois a lire...)
;
s=size(ff)
n_dim=s(0)
n_type=s(n_dim+1)
n_lect=1
if (n_dim eq 3) then begin
   n_lect = s(n_dim)
   if keyword_set(block3d) then begin
      if keyword_set(date) then begin
	 print,'Impossible : block3d et date...'
	 stop
      endif
      n_lect = 1
      n_dim  = 2
   endif
endif

;------- Check globaux 
if (n_type ne 4 and n_type ne 2 and n_type ne 5) then begin
   print, 'ERREUR PEYL_GRIDREAD : wrong type of data :',n_type
   stop
endif
if (n_dim le 0 or n_dim gt 3) then begin
   print, 'ERREUR PEYL_GRIDREAD : dimension of data invalide :',n_dim
   stop
endif

;-------- Gestion de la lecture d'une date en plus de la matrice ff.
; REM : la variable date contiendra en retour les datent luent..
;
lecture_date=0
if (keyword_set(date)) then begin
   lecture_date=1
   dd=date
   if n_elements(date) eq 1 then dd=float(dd)
   date=fltarr(n_elements(dd),n_lect)
endif

;-------- Gestion du format de lecture : formate ou non et fortran??
;         Et ouverture du fichier.
;
if (keyword_set(unform)) then unform=1 else unform=0
if (keyword_set(f77)) then begin
   unform=1
   if swap_endian then openr,unit,file_name,/get_lun,/f77_unformatted,/SWAP_ENDIAN $
   else openr,unit,file_name,/get_lun,/f77_unformatted
endif else begin
    if unform then begin
        if swap_endian then openr,unit,file_name,/get_lun,/SWAP_ENDIAN $
        else openr,unit,file_name,/get_lun
    endif else begin
        width = 17 * s(1)
        openr,unit,file_name,width=width,/get_lun
    endelse
endelse

;-------- Gestion du type de donnees a lire...
; REM : xx represente le type lu et ff les donnes stoquees.. 
;       il y a conversion dans le cas ou le keyword "double" 
;       est selectionne mais ou ff est un simple reel..
;	Si ff est un double mais sans le keyword "double" on lit des 
;	simple pour les convertir en double..
xx=ff
if (n_type eq 5 and (not keyword_set(double))) then xx=float(ff)
if (keyword_set(double)) then xx=double(ff)
if n_dim gt 2 then xx=xx(*,*,0)

;-------------------------------------------------------------------- 
;                   Lecture du fichier
case 1 of 
     n_dim le 2 : begin
		  if (unform eq 1) then begin
	   	     if lecture_date then readu,unit,date
		     readu,unit,xx 
		  endif else begin
	   	     if lecture_date then readf,unit,date
		     readf,unit,xx 
	          endelse
		  ff=xx
		  end
     n_dim eq 3 : begin
		  for k=0,n_lect-1 do begin
		      if (unform eq 1) then begin
		      	 if lecture_date then readu,unit,dd
		         readu,unit,xx 
		      endif else begin 
		      	 if lecture_date then readf,unit,dd
			 readf,unit,xx
		      endelse 
		      ff(*,*,k) = xx
		      if lecture_date then date(*,k) = dd
;		      print,'date ',k,dd,date
		  endfor
		  end
endcase
;
if info then begin 
   print,'File read : ',file_name
   print,'   dim = ',auto_string(s(1:n_dim),0)
   if lecture_date then begin
      date_out=fix(date(0,0:n_lect-1))
      print,'   dates lues = ',auto_string(date_out(0:n_lect-1),0)
   endif
endif 
if (eof(unit) ne 1) then begin
   print,'ERREUR PEYL_GRIDREAD : file not read completely....'
   free_lun,unit
   if peyl_pause() then stop
endif
;
free_lun,unit
end

;-
