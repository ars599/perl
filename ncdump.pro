;
;  Equivalent du ncdump unix : Decrit le contenu d'un fichier netcdf
;

PRO ncdump,filein
;
; Open the NetCDF file:
id = NCDF_OPEN(filein)
info = NCDF_INQUIRE(id)
;help,info,/STRUCTURE
;print,info.Ndims,info.nvars,info.Ngatts,info.RecDim
;
dimval = LONARR(info.Ndims)
dimnam = STRARR(info.Ndims)
FOR idim = 0,info.Ndims-1 DO BEGIN
   NCDF_DIMINQ,id,idim,sdimnam,sdimval
   dimval[idim] = sdimval
   dimnam[idim] = sdimnam
ENDFOR

FOR ivar = 0, info.nvars-1 DO BEGIN
   variable=NCDF_VARINQ(id,ivar)
   print,'==============   ',  variable.name,'   ==============='
   nd = variable.ndims
   CASE nd OF
      1: PRINT,variable.datatype,dimval[variable.dim],format='("TYPE:",A,"[",I4,"]")'
      2: PRINT,variable.datatype,dimnam[variable.dim],format='("TYPE:",A20,"[",A,", "   ,A,"]")'
      3: PRINT,variable.datatype,dimnam[variable.dim],format='("TYPE:",A20,"[",A,2(", ",A),"]")'
      4: PRINT,variable.datatype,dimnam[variable.dim],format='("TYPE:",A20,"[",A,3(", ",A),"]")'
      5: PRINT,variable.datatype,dimnam[variable.dim],format='("TYPE:",A20,"[",A,4(", ",A),"]")'
   ENDCASE
   FOR iatt=0,variable.Natts-1 DO BEGIN
     nam = NCDF_ATTNAME(id,ivar,iatt)
     NCDF_ATTGET,id,ivar,nam,val
     print,nam,string(val),format='(A13," : ",5A20)'
   ENDFOR
ENDFOR

NCDF_CLOSE,id

END
