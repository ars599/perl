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
;    peyl_writenc, var0, $
;                  file=file,$
;                  var1=var1,var2=var2,var3=var3,var4=var4,var5=var5,$
;                  var_name=var_name,$
;                  var_title=var_title,$
;                  var_units=var_units,$
;                  var_dim=var_dim,$
;                  dim_val=dim_val,$
;                  dim_name=dim_name,$
;                  dim_title=dim_title,$
;                  dim_unlimited=dim_unlimited,$
;                  var_misval=var_misval
;
;
; INPUTS:   var0 : variable principale a sauver
;
; OPTIONAL INPUT PARAMETERS:
;          file : nom du fichier a sauver (output.nc par defaut)
;          var1, .... var22 : variables optionnelles a sauvegarder
;                            de dimension =< dimension de var
;                            meme ordre des dim que pour var
;          var_name  :   tableau du nom des variables (string)
;          var_title :   tableau du nom complet des variables (string)
;          var_units :   tableau des unites des variables (string)
;          var_dim   :   Structure contenant les indices des dimensions
;                        de chaque variable (SEE EXEMPLE) 
;
;          dim_size : tableau contenant les tailles des dimensions (optionnel)
;          dim_val : structure contenant les valeurs des dimensions
;          (optionel, SEE EXEMPLE)
;
;          dim_name : tableau du nom des dimensions (string)
;          dim_title: tableau du nom complet des dimensions (string)
;          dim_units: tableau du nom des unites des dimensions (string)
;          dim_unlimited: nom de la dimension UNLIMITED si besoin (string)
;
;          no_dim : set to 1 to suppres output of var dimension
;          
;          var_misval: valeur de la "missing value" (float,integer)
;
; OUTPUTS: Netcdf file
;
; EXEMPLE : creer un fichier avec data(nlon,nlat,ntime), data2(nlat,ntime)
;           et une variable de type STRING data3 (Meme Traitement!!)
;           dim_size = [nlon,nlat,ntime]
;           var_dim = {V1:[0,1,2], V2:[1,2], V3:[2]}
;           dim_val = {D1:lon, D2:lat, D3:time}
;       REM : LES NOMS DES TAGS "V1" ou "D1" SONT LIBRES, SEUL L'ORDRE
;       COMPTE !!!!
;
;
; MODIFICATION HISTORY:
;	Written, PEYLIN, 11/2003.
; 
;===================================================================
pro peyl_writenc, var0, var1,var2,var3,var4,var5,var6,var7,var8,var9,$
                  var10,var11,var12,var13,var14,var15,var16,var17,var18,var19,$
                  var20,var21,var22,var23,var24,var25,var26,var27,var28,var29,$
                  var30,var31,var32,var33,var34,var35,var36,var37,var38,var39,$
                  var40,var41,var42,var43,var44,var45,var46,var47,var48,var49,$
                  var50,var51,var52,var53,var54,var55,var56,var57,var58,var59,$
                  var60,var61,var62,var63,var64,var65,var66,var67,var68,var69,$
                  var70,var71,var72,var73,var74,var75,var76,var77,var78,var79,$
                  var80,var81,var82,var83,var84,var85,var86,var87,var88,var89,$
                  file      = file,$
                  var_dim   = var_dim,$
                  var_name  = var_name,$
                  var_title = var_title,$
                  var_units = var_units,$
                  dim_size  = dim_size,$
                  dim_val   = dim_val,$
                  dim_name  = dim_name,$
                  dim_title = dim_title,$
                  dim_units = dim_units,$
                  dim_unlimited = dim_unlimited,$
                  var_misval = var_misval,$
                  no_dim     = no_dim,$
                  nomessage  = nomessage

if (not keyword_set(file)) then file='output.nc'
if (not keyword_set(no_dim)) then add_dim = 1 else add_dim = 0
if (not keyword_set(nomessage)) then nomessage = 1


if (nomessage eq 0) then begin
    print,'!! Nouvelle version de PEYL_WRITENC (01/10/2003) !!'
    print,'   (Utiliser peyl_writenc2 pour ancienne version)'
endif

nb_var = N_params()
if (nomessage eq 0) then print,'Nombre de variables  ',nb_var
if (not keyword_set(var_name)) then begin
    var_name=replicate('VAR',nb_var)
    for n=1,nb_var-1 do var_name(n)=var_name(n)+auto_string(n,0)
endif
if (not keyword_set(var_title)) then var_title=var_name
if (not keyword_set(var_units)) then var_units=replicate('??',nb_var)

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

;----- Definition des tailles de chaque dim..

IF (NOT keyword_set(dim_size)) then begin

    ss = size(var0)
    dim_size = ss(1:ss(0))

    for n=1,nb_var-1 do begin
        cmde = 'ss = size(var'+auto_string(n,0)+')'
        res = execute(cmde)
        for l=0,ss(0)-1 do begin
            ii = where(dim_size eq ss(1+l), cc)
            if cc eq 0 then dim_size = [dim_size,ss(1+l)]
        endfor
    endfor

ENDIF    

nb_dim = n_elements(dim_size)
if (nomessage eq 0) then print,'NOMBRE de DIMENSIONS = ',nb_dim


if (not keyword_set(dim_name)) then dim_name='D'+string(indgen(nb_dim)+1,format='(i2.2)')
if (not keyword_set(dim_title)) then dim_title=dim_name
if (not keyword_set(dim_units)) then dim_units=replicate('?',nb_dim)
if (not keyword_set(dim_unlimited)) then dim_unlimited=''              

;---- Quelques checks
if n_elements(dim_name) lt nb_dim then message,'Dim_name sous dimensionne...' 
if n_elements(dim_title) lt nb_dim then message,'Dim_title sous dimensionne...' 
if n_elements(dim_units) lt nb_dim then message,'Dim_units sous dimensionne...' 



;----- CHECK si variables de type "string" ; Rajout de la dimension
;      string_lenght_max!!!

strlen_max = 0
special_string = 0
for n=0,nb_var-1 do begin
                                ;--- Recherche si variable de type
                                ;    string! 
    cmde = 'ss = size(var' + auto_string(n,0) + ')'
    res  = execute(cmde)
    if ss(ss(0)+1) eq 7 then begin
        cmde = 'len = strlen(var' + auto_string(n,0) + ')'
        res  = execute(cmde)
        strlen_max = max([strlen_max,max(len)])
        special_string = 1 
    endif 
endfor 
if special_string then begin
    dim_size  = [strlen_max,dim_size]
    dim_name  = ['strlen',dim_name]
    dim_title = ['string lenght',dim_title]
    dim_units = ['-',dim_units]
    tab_dim_string = findgen(strlen_max)+1.
    nb_dim = nb_dim + 1
endif


;----- Definition de l'ID des dimensions
dim_id = intarr(nb_dim)
for d=0,nb_dim-1 do begin
    if dim_unlimited(0) ne '' then begin
        ii = where(dim_name eq dim_unlimited(0), cc)
    endif else ii=-1
    if (d eq ii(0)) then begin        
        dim_id(d) = ncdf_dimdef(ncid_out,dim_name(d),/UNLIMITED)
    endif else begin
        dim_id(d) = ncdf_dimdef(ncid_out,dim_name(d),dim_size(d))
    endelse
endfor

;----- Definitions de la valeur des dimensions.

if add_dim then begin

    nb_max  = max(dim_size)
;    tab_dim_val = dblarr(nb_max,nb_dim)

    if (not keyword_set(dim_val)) then begin
;        for d=0,nb_dim-1 do tab_dim_val(0:dim_size(d)-1,d) = dindgen(dim_size(d)) + 1.
        cmde = 'dim_val = {D0:dblarr(dim_size(0))+1.'
        for d=1,nb_dim-1 do cmde = cmde + $
          ',D'+auto_string(d,0)+':dblarr(dim_size('+auto_string(d,0)+'))+1.'
        cmde = cmde + '}'
        res = execute(cmde)

    endif else begin
;        if special_string then $
;          tab_dim_val(0:strlen_max-1,0) = dindgen(strlen_max)+1.
        nn = n_tags(dim_val)
                                ;--- Dim_val sous forme de tableau.
        if (nn le 0) then begin
            for d=special_string,nb_dim-1 do begin
                d2  = d-special_string                
                nd = dim_size(d2)
;                tab_dim_val(0:nd-1,d) = dim_val(0:nd-1,d2)
                if d2 eq 0 then cmde = 'dim_val = {D0:dim_val(0:'+auto_string(nd-1,0)+',0)' else $
                  cmde = cmde + ',D'+auto_string(d2,0) + $
                  ':dim_val(0:'+auto_string(nd-1,0)+','+auto_string(d2,0)+')'
            endfor
            cmde = cmde + '}'
            res = execute(cmde)

                                ;--- Dim_val sous forme de structure...
        endif else begin
            if nn ne (nb_dim-special_string) then $
              message,'! Problem dim_val structure mal definiee...'
            tag = tag_names(dim_val)
            for d=special_string,nb_dim-1 do begin
                d2  = d-special_string
                res = execute('temp = dim_val.' + tag(d2))
                if n_elements(temp) ne dim_size(d) then $
                  message,'! Problem de dimension pour tag '+tag(d2)
;                nd = dim_size(d)
;                tab_dim_val(0:nd-1,d) = temp
            endfor
        endelse
    endelse
    
endif 

;--------------------------------------------------------
; Definition des Variables Dimensions automatiquement
;--------------------------------------------------------

if add_dim then begin

    dim_val_id = intarr(nb_dim)
    tag = tag_names(dim_val)
            
    for d=0,nb_dim-1 do begin   
        
;        ss = size(tab_dim_val(d))
        if special_string and d eq 0 then begin
            ss = size(tab_dim_string)
        endif else begin
            d2  = d-special_string
            res = execute('ss = size(dim_val.' + tag(d2)+')')
        endelse
        lll=0 & fff=0 & ddd=0 & bbb=0 & sss=0 & ccc=0
        case ss(ss(0)+1) of 
            1:bbb=1
            2:sss=1
            3:lll=1
            4:fff=1
            5:ddd=1
            7:ccc=1
            else:message,'type non prevu'
        endcase
        
        dim_val_id(d) = ncdf_vardef(ncid_out,dim_name(d),dim_id(d),$
                                    byte=bbb,char=ccc,double=ddd,float=fff,$
                                    long=lll,short=sss)

        ncdf_attput,ncid_out,dim_val_id(d),'title',dim_title(d)
        ncdf_attput,ncid_out,dim_val_id(d),'units',dim_units(d)
    endfor
    
endif

;--------------------------------------------------------
; Definition des variables elles-memes
;--------------------------------------------------------

var_id = intarr(nb_var)

if keyword_set(var_dim) then begin
    tags = tag_names(var_dim)
endif 

for n=0,nb_var-1 do begin
                                ;--- Definition de la taille de la
                                ;    variable. !! cas special pour les
                                ;    string !!
    cmde = 'ss = size(var' + auto_string(n,0) + ')'
    res=execute(cmde)
    ndd = ss(0)    
    lll=0 & fff=0 & ddd=0 & bbb=0 & sss=0 & ccc=0
    case ss(ss(0)+1) of 
        1:bbb=1
        2:sss=1
        3:lll=1
        4:fff=1
        5:ddd=1
        7:ccc=1
        else:message,'type non prevu'
    endcase

;    print,var_name(n),dim_id(*),bbb,sss,lll,fff,ddd,ccc


    if ccc eq 1 then string_flag = 1 else string_flag=0 
    if ((ndd+string_flag) gt nb_dim) then $
      message,'Erreur dans dimension de variable : '+var_name(n)
    if ((ndd+string_flag) eq 0) then goto,suite_defvar

                                ;--- Definition des index des
                                ;    differentes dimension de la
                                ;    variable. !! cas special pour les
                                ;    string !! Dim0 est tjs la dim de
                                ;    strlenght! 
    if NOT keyword_set(var_dim) then begin
        jj_var_dim = indgen(ndd+string_flag)
        if string_flag then begin
            cmde = 'len = strlen(var' + auto_string(n,0) + ')'
            res  = execute(cmde)
            dd   = max(len)
            if dd gt dim_size(0) then message,'HHUUMM, dim bizarre pour var string!!'
            jj_var_dim(0) = 0
        endif
        for d=0,ndd-1 do begin            
            ii = where(dim_size eq ss(1+d), cc)
            if cc eq 0 then message,'Probleme : Dimensions non definies pour '+var_name(n)
            if cc gt 1 then begin
                if (ii(0) gt 0 or (ii(0) eq 0 and cc gt 2)) then $
                  message,'ERR: Fournir var_dim car plusieurs Dim de meme taille! '+var_name(n) $
                else ii = ii(1)
            endif
            if string_flag then d1 = d+1 else d1 = d
            jj_var_dim(d1) = ii(0)
        endfor
         
                                ;--- Index de la variable deja
                                ;    definis.. 
    endif else begin
        cmde = 'jj_var_dim = var_dim.' + tags(n)
        res  = execute(cmde)
        if special_string then jj_var_dim = jj_var_dim + 1
        if string_flag then jj_var_dim = [0,jj_var_dim]
        ll = n_elements(jj_var_dim)

                                ;-- Check dim 
        if (ll ne (ndd+string_flag)) then begin
            print,'Probleme dimensions dans var_dim.'+tags(n)+' pour '+var_name(n)
            print,'     -> var_dim : ',ll,'  variable : ',ndd+string_flag
            stop
        endif
        for l=0,ll-1 do if jj_var_dim(l) ge nb_dim then $
          message,'Probleme dans var_dim.'+tags(n)+' pour '+var_name(n)

                                ;--- Check special pour variable
                                ;    string
        if string_flag then begin
            cmde = 'len = strlen(var' + auto_string(n,0) + ')'
            res  = execute(cmde)
            dd   = max(len)
            if dim_size(jj_var_dim(0)) lt dd then $ 
              message,'ERR: Var STRING, Mais nb char SUPERIEUR a dimension 0 !!'
        endif
        
    endelse

    suite_defvar:

    if (ndd+string_flag) eq 0 then $
      var_id(n) = ncdf_vardef(ncid_out,var_name(n),$
                              byte=bbb,char=ccc,double=ddd,$
                              float=fff,long=lll,short=sss) $
    else $
      var_id(n) = ncdf_vardef(ncid_out,var_name(n),dim_id(jj_var_dim),$
                              byte=bbb,char=ccc,double=ddd,float=fff,$
                              long=lll,short=sss)

    ncdf_attput,ncid_out,var_id(n),'title',var_title(n)
    ncdf_attput,ncid_out,var_id(n),'units',var_units(n)

                                ;--- Set missval attribute
    if (keyword_set(var_misval)) then begin
        if (abs(var_misval) gt 1.e34) then begin
            var_misval=1.e34
            if n eq 0 then print,'Warning Missing Value to large and reset to : ',var_misval
        endif
        ncdf_attput,ncid_out,var_id(n),'missing_value',var_misval
    endif
endfor


ncdf_control,ncid_out,/endef


;--------------------------------------------------------
; Ecriture des variables
;--------------------------------------------------------

;---- Dimensions

if add_dim then begin
    tag = tag_names(dim_val)
    for d=0,nb_dim-1 do begin
;        ncdf_varput,ncid_out,dim_id(d),tab_dim_val(0:dim_size(d)-1,d)
        if special_string and d eq 0 then $
          ncdf_varput,ncid_out,dim_id(d),tab_dim_string(*) $
        else begin
            d2  = d-special_string
            cmde = 'ncdf_varput,ncid_out,dim_id(d2),dim_val.'+tag(d2)
            res = execute(cmde)
        endelse
    endfor
endif

;---- Variables elles memes

for n=0,nb_var-1 do begin
    cmde = 'ncdf_varput,ncid_out,var_id(n),var'+auto_string(n,0)
    res = execute(cmde)
endfor


;--------------------------------------------------------
; Fermeture fichier
;--------------------------------------------------------
ncdf_close,ncid_out

END
;-




