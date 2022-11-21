;+
; NAME:
;	peyl_readnc
;
; PURPOSE:
;	read a netCDF file
;
; CATEGORY:
;
; CALLING SEQUENCE:
;    peyl_readnc, var, $
;                 file=file,$
;                 var_name=var_name,$
;                 var_info=var_info,$
;                 dim_info=dim_info,$
;                 attrib=attrib
;
;
; INPUTS:   
;
; OPTIONAL INPUT PARAMETERS:
;          file : nom du fichier a lire (input.nc par defaut)
;          var_name : nom de variable (string)
;                     CHOIX INTERACTIF SI NON PRECISEE...
;
; OUTPUTS:
;          var : variable elle meme
;          var_info : Infos sur la variable 
;                     Structure : 
;                                NAME            STRING 
;                                DATATYPE        STRING
;                                NDIMS           LONG
;                                NATTS           LONG 
;                                DIM             LONG 
;
;          dim_info : Infos sur toutes les dimensions du fichier
;                     Structure :
;                                NAME            STRING
;                                SIZE            ARRAY(INT)
;
;          attrib :  Infos sur les attributs de la variable 
;                    Tableau de structure : 
;                                NAME            STRING 
;                                TYPE            STRING 
;                                VALUE           STRING 
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
;	Example:
;
;
; MODIFICATION HISTORY:
;	Written, PEYLIN & LECOMTE, 11/2000.
; 
;=================================================================
PRO peyl_readnc, var, file=file, $
                 count = count, $
                 offset = offset, $
                 var_name=var_name, $
                 hash_tag=hash_tag,$
                 var_multi_name=var_multi_name, $
                 var_info = var_info,$
                 attrib=attrib,info=info,$
                 gattrib=gattrib,$
                 only_var_names=only_var_names, $
                 dim_info = dim_info


if not(keyword_set(only_var_names)) then only_var_names=0 else only_var_names=1
if not(keyword_set(info)) then info=0 else info=1
read_attrib = 1
read_gattrib = 1
if not(keyword_set(count)) then read_part=0 else read_part=1
if not(keyword_set(offset)) then begin
    offset=replicate(0,8) 
endif

hash=0
if size(var,/type) eq 11 then hash = 1
if not(keyword_set(hash_tag)) then hash_tag=var_name
read_diminfo = 1

;----- Ouverture du fichier
if n_elements(file) eq 0 then file='input.nc'
if info then print, 'FICHIER TRAITE : ', file 
ncid=ncdf_open(file(0)) 


;----------------------------------------------------------------
; LECTURE DU NOM DES VARIABLES PRESENTES DANS FICHIER NetCDF : 

nc_inf = ncdf_inquire(ncid) 
nvar   = (nc_inf.nvars) 
ndim   = (nc_inf.ndims) 
ngatts = (nc_inf.NGATTS)

ncvarnames  = strarr(nvar) 
for i = 0, nvar-1 do begin 
    res = ncdf_varinq(ncid,i) 
    ncvarnames(i)=res.name
endfor 
ncvarnames2 = strupcase(ncvarnames)

if only_var_names then begin
    var = ncvarnames
    goto,gattrib_deb
endif


;---------------------------------------------------------------- 
; CHOIX D'UNE VARIABLE (INTERACTIF SI NON PRECISEE..)
 

if keyword_set(var_name) then begin
    for n=0,n_elements(var_name)-1 do begin
        ii = where(ncvarnames eq var_name(n)) 
        if ii(0) eq -1 then begin
            ii = where(ncvarnames2 eq strupcase(var_name(n)))
            if ii(0) ne -1 then print,'Case sensitive difference for ',var_name(n)
        endif
        if ii(0) ne -1 then begin
            var_name_loc = ncvarnames(ii(0))
            goto, suite_choix_var
        endif
    endfor 
    print, 'The variable does not exist : ', var_name


endif else if keyword_set(var_multi_name) then begin
    nvv = n_elements(var_multi_name)
    var_name_loc = strarr(nvv)
    for n=0,nvv-1 do begin
        ii = where(ncvarnames eq var_multi_name(n)) 
        if ii(0) eq -1 then $
          ii = where(ncvarnames2 eq strupcase(var_multi_name(n)))
        if ii(0) eq -1 then begin
            print,'problem variable multi non reconnue : ',var_multi_name(n)
            print,' -> Choix parmis : ',ncvarnames
            stop
        endif
        var_name_loc(n) = ncvarnames(ii(0))
    endfor 
    goto,suite_choix_var
endif 


print,'Choose one of the available variable : '
for n=0,n_elements(ncvarnames)-1 do begin
    print,n,' -> ',ncvarnames(n)
endfor
read, prompt='Which number  ? : ', no
var_name_loc = ncvarnames(no)
suite_choix_var:


;---------------------------------------------------------------- 
; RECUPERATION DE LA VARIABLE (ou variable multi):

if info then print, ' |-> VARIABLE TRAITEE : ', var_name_loc
nvv = n_elements(var_name_loc)

for nv=0,nvv-1 do begin
    varid = ncdf_varid(ncid, var_name_loc(nv)) 
    if read_part then $
      ncdf_varget, ncid, varid, var_tmp, offset=offset, count=count $
    else ncdf_varget, ncid, varid, var_tmp, offset=offset
    if hash eq 0 then begin
        var=var_tmp
    endif else begin
        var(hash_tag)=var_tmp
    endelse
    ss = size(var_tmp) 
                                ;--- Gestion des variables multi de
                                ;    meme dimension!!!
    if nvv gt 1 then begin
        if nv eq 0 then begin
            ss0 = ss
            var_multi = replicate(var(0),[ss0(1:ss0(0)),nvv])
            tt='' & for i=0,ss(0)-1 do tt = tt+'*,'
        endif
        if total(abs(ss-ss0)) ne 0 then message,'Problem : var multi of different dimension!'
        cmde = 'var_multi('+tt+'nv) = var'
        res = execute(cmde)
    endif 
    if info and nv eq 0 then print,'Dimensions de la variable :',ss(1:ss(0))
endfor

                                ;--- Avoid duplication of var_multi
if nvv gt 1 then var = temporary(var_multi)


;---------------------------------------------------------------- 
; RECHERCHE DU NOM DES DIMENSIONS POUR LA VARIABLE

if (read_diminfo) then begin
    
    IF ndim EQ 0 THEN BEGIN
        dim_info = {name : '', size :-1}
    ENDIF ELSE BEGIN
        dim_info ={name : REPLICATE([''],ndim), $
                   size : REPLICATE(-1,ndim)}
        for idim=0,ndim-1 do begin
            ncdf_diminq, ncid, idim,  dimname, dimsize
            dim_info.name[idim] = dimname
            dim_info.size[idim] = dimsize
        endfor
    ENDELSE

endif 


;---------------------------------------------------------------- 
; RECHERCHE DES ATTRIBUTS DE LA VARIABLE :

attrib_deb:
if (read_attrib) then begin

    var_info = ncdf_varinq(ncid, varid)
    natts = var_info.natts
    if info then PRINT, 'VARIABLE : ', var_name_loc, ' POSSEDE : ', natts, '  ATTRIBUTS'
    attrib = replicate({name:'',type:'',value:''}, natts>1)

    FOR J=0, natts-1 DO BEGIN
        attr_name = ncdf_attname(ncid, varid, j)
        IF attr_name ne '' THEN BEGIN
            ncdf_attget, ncid, varid, attr_name, value
            foo = ncdf_attinq(ncid, varid, attr_name)
            attr_type = foo.datatype
            CASE attr_type OF
                'CHAR': value2=STRING(value)
                'BYTE': value2=value
                'INT': value2=FIX(value)
                'LONG': value2=LONG(value)
                'FLOAT': value2=FLOAT(value)
                'DOUBLE': value2=DOUBLE(value)
                ELSE: value2=value ; ne pas modifier la valeur del'attribut
            ENDCASE
            if info then print, "ATTRIBUT ", j, " : ",  attr_name, '     VALEUR : ', value2
            attrib(j).name = attr_name
            attrib(j).type = attr_type
            attrib(j).value= peyl_concatstr(string(value),separator=' ')
        ENDIF
    ENDFOR
endif

;--------------------------------------------------------------- 
; RECHERCHE DES ATTRIBUTS GLOBAUX

gattrib_deb:
if (read_gattrib) then begin

    if ngatts gt 0 then $
      gattrib = replicate({name:'',type:'',value:''}, ngatts)

    for j=0,ngatts-1 do begin
        gattr_name = NCDF_ATTNAME(ncid, j , /GLOBAL) 
        if gattr_name eq '' then message,'problem global attribute'
        
        ncdf_attget, ncid, gattr_name, value, /global
        foo = ncdf_attinq(ncid, gattr_name, /global)
        gattr_type = foo.datatype
        CASE gattr_type OF
            'CHAR': value2=STRING(value)
            'BYTE': value2=value
            'INT': value2=FIX(value)
            'LONG': value2=LONG(value)
            'FLOAT': value2=FLOAT(value)
            'DOUBLE': value2=DOUBLE(value)
            ELSE: value2=value ; ne pas modifier la valeur del'attribut
        ENDCASE
        if info then print, "GLOBAL ATTRIBUT ", j, " : ",  gattr_name, '  VALEUR : ', value2
        gattrib(j).name = gattr_name
        gattrib(j).type = gattr_type
        gattrib(j).value= peyl_concatstr(string(value),separator=' ')
    endfor
endif

fin:
ncdf_close, ncid 

END

;-

