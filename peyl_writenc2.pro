;+
; NAME:
;	peyl_writenc
;
; PURPOSE:
;	Create a netCDF file
;
; CATEGORY:
;
; CALLING SEQUENCE:
;    peyl_writenc, var, $
;                  file=file,$
;                  var1=var1,var2=var2,var3=var3,var4=var4,var5=var5,$
;                  var_name=var_name,$
;                  var_title=var_title,$
;                  var_units=var_units,$
;                  dim_val=dim_val,$
;                  dim_name=dim_name,$
;                  dim_title=dim_title,$
;                  dim_unlimited=dim_unlimited,$
;                  var_misval=var_misval
;
;
; INPUTS:   var : variable principale a sauver
;
; OPTIONAL INPUT PARAMETERS:
;          file : nom du fichier a sauver (output.nc par defaut)
;          var1, .... var5 : variables optionnelles a sauvegarder
;                            de dimension =< dimension de var
;                            meme ordre des dim que pour var
;          var_name :    tableau du nom des variables (string)
;          var_title :   tableau du nom complet des variables (string)
;          var_units :   tableau des unites des variables (string)
;
;          dim_val : tableau des valeurs pour chaque dimension 
;                    fltarr(max(dim), nb_dim)
;                    1er indice : valeurs (surdimentionne a dim maximale)
;                    2em indice : no de la dimension
;
;          dim_name : tableau du nom des dimensions (string)
;          dim_title: tableau du nom complet des dimensions (string)
;          dim_units: tableau du nom des unites des dimensions (string)
;          dim_unlimited: nom de la dimension UNLIMITED si besoin (string)
;          
;          var_misval: valeur de la "missing value" (float,integer)
;
; OUTPUTS:
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;
; PROCEDURE:
;	
;
; MODIFICATION HISTORY:
;	Written, PEYLIN & LECOMTE, 11/2000.
; 
;===================================================================
pro peyl_writenc2, var, file=file,$
                  var1=var1,var2=var2,var3=var3,var4=var4,var5=var5,$
                  dim_var1=dim_var1,dim_var2=dim_var2,dim_var3=dim_var3,dim_var4=dim_var4,dim_var5=dim_var5,$
                  var_name=var_name,$
                  var_title=var_title,$
                  var_units=var_units,$
                  dim_val=dim_val,$
                  dim_name=dim_name,$
                  dim_title=dim_title,$
                  dim_units=dim_units,$
                  dim_unlimited=dim_unlimited,$
                  var_misval=var_misval


if (not keyword_set(file)) then file='output.nc'
nb_var = 1
if (keyword_set(var1)) then nb_var=nb_var+1
if (keyword_set(var2)) then nb_var=nb_var+1
if (keyword_set(var3)) then nb_var=nb_var+1
if (keyword_set(var4)) then nb_var=nb_var+1
if (keyword_set(var5)) then nb_var=nb_var+1

if (not keyword_set(var_name)) then begin
    var_name=replicate('VAR',nb_var)
    for n=1,nb_var-1 do var_name(n)=var_name(n)+auto_string(n,0)
endif
               

if (not keyword_set(var_title)) then var_title=var_name
if (not keyword_set(var_units)) then var_units=replicate('??',nb_var)

if (not keyword_set(dim_name)) then dim_name=['I','J','K','L','M','N','O','P','Q','R','S','T']
if (not keyword_set(dim_title)) then dim_title=dim_name
if (not keyword_set(dim_units)) then dim_units=replicate('?',11)
if (not keyword_set(dim_unlimited)) then dim_unlimited=''              


;----- Quelques checks..

if (n_elements(var_name) lt nb_var) then message,'var_name mal dimensionne'
if (n_elements(var_title) lt nb_var) then message,'var__title mal dimensionne'
if (n_elements(var_units) lt nb_var) then message,'var_units mal dimensionne'

 
;--------------------------------------------------------
; Ouverture Fichier
;--------------------------------------------------------
ncid_out=ncdf_create(file,/clobber)


;--------------------------------------------------------
; Quelques attributs globaux
;--------------------------------------------------------
; rien pour l'instant


;--------------------------------------------------------
; Definition des Dimensions
;--------------------------------------------------------
ss = size(var)
nb_dim = ss(0)
print,'NOMBRE de DIMENSIONS = ',nb_dim

;---- Quelques checks
if n_elements(dim_name) lt ss(0) then message,'Dim_name mal dimensionne...' 
if n_elements(dim_title) lt ss(0) then message,'Dim_name mal dimensionne...' 


dim_id = intarr(nb_dim)
for d=0,nb_dim-1 do begin
    if dim_unlimited ne '' then begin
        ii = where(dim_name eq dim_unlimited, cc)
    endif else ii=-1
    if (d eq ii(0)) then begin        
        dim_id(d) = ncdf_dimdef(ncid_out,dim_name(d),/UNLIMITED)
    endif else begin
        dim_id(d) = ncdf_dimdef(ncid_out,dim_name(d),ss(d+1))
    endelse
endfor


if (not keyword_set(dim_val)) then begin
    nb_max = max(ss(1:nb_dim))
    dim_val = fltarr(nb_max,nb_dim)
    for d=0,nb_dim-1 do dim_val(0:ss(d+1)-1,d) = findgen(ss(d+1)) + 1.
endif


;--------------------------------------------------------
; Definition des Variables Dimensions
;--------------------------------------------------------


dim_val_id = intarr(nb_dim)

for d=0,nb_dim-1 do begin   

    ssloc = size(dim_val)
    lll=0 & fff=0 & ddd=0 & bbb=0 & sss=0 & ccc=0
    case ssloc(ssloc(0)+1) of 
        1:bbb=1
        2:sss=1
        3:lll=1
        4:fff=1
        5:ddd=1
        7:ccc=1
        else:message,'type non prevu'
    endcase

    dim_val_id(d) = ncdf_vardef(ncid_out,dim_name(d),dim_id(d),$
                           byte=bbb,char=ccc,double=ddd,float=fff,long=lll,short=sss)

    ncdf_attput,ncid_out,dim_val_id(d),'title',dim_title(d)
    ncdf_attput,ncid_out,dim_val_id(d),'units',dim_units(d)
endfor


;--------------------------------------------------------
; Definition des variables elle memes
;--------------------------------------------------------

var_id = intarr(nb_var)

for n=0,nb_var-1 do begin

    ssloc = ss
    jj = indgen(ssloc(0))
    if n gt 0 then begin
        cmde = 'ssloc = size(var' + auto_string(n,0) + ')'
        res=execute(cmde)
        if (ssloc(0) gt nb_dim) then begin
            print,'Variable de dim > var de base : ',n
            stop
        endif
        cmde = 'jj = dim_var'+auto_string(n,0)
        res=execute(cmde)
        if (n_elements(jj) lt 0 or  n_elements(jj) gt nb_dim) then begin
           cmde="print,'Dimension de Variable mal positionnee',dim_var"+auto_string(n,0)
           res = execute(cmde)
           stop
        endif
    endif

    lll=0 & fff=0 & ddd=0 & bbb=0 & sss=0 & ccc=0
    case ssloc(ssloc(0)+1) of 
        1:bbb=1
        2:sss=1
        3:lll=1
        4:fff=1
        5:ddd=1
        7:ccc=1
        else:message,'type non prevu'
    endcase

    var_id(n) = ncdf_vardef(ncid_out,var_name(n),dim_id(jj),$
                           byte=bbb,char=ccc,double=ddd,float=fff,long=lll,short=sss)

    ncdf_attput,ncid_out,var_id(n),'title',var_title(n)
    ncdf_attput,ncid_out,var_id(n),'units',var_units(n)

    if (keyword_set(var_misval)) then begin
        if (abs(var_misval) gt 1.e34) then begin
            var_misval=1.e34
            print,'Warning Missing Value to large and reset to : ',var_misval
        endif
        ncdf_attput,ncid_out,var_id(n),'missing_value',var_misval
    endif
endfor


ncdf_control,ncid_out,/endef


;--------------------------------------------------------
; Ecriture des variables
;--------------------------------------------------------

;---- Dimensions

for d=0,nb_dim-1 do begin
    ncdf_varput,ncid_out,dim_id(d),dim_val(0:ss(d+1)-1,d)
endfor


;---- Variables elles memes


ncdf_varput,ncid_out,var_id(0),var

for n=1,nb_var-1 do begin
    cmde = 'ncdf_varput,ncid_out,var_id(n),var'+auto_string(n,0)
    res = execute(cmde)

endfor

;--------------------------------------------------------
; Fermeture fichier
;--------------------------------------------------------
ncdf_close,ncid_out


END

;-




