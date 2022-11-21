PRO main

varname='pr'
path=''
coeff=86400.
;path='/bm/gscratch1/fdelage/'+scenario+'/'
path1='/bm/gdata1/fdelage/pattern_matching/outputs/cmip5/ENSO/historical/'+varname+'/boxmean/'
path2='/bm/gdata1/fdelage/pattern_matching/outputs/cmip5/ENSO/rcp85/'+varname+'/boxmean/'
 model_name = ['ACCESS1-0','ACCESS1-3','bcc-csm1-1','CCSM4','CESM1-BGC','CESM1-CAM5',$
                'CNRM-CM5','CanESM2','CSIRO-Mk3-6-0','GISS-E2-R','GFDL-CM3',$
                'GFDL-ESM2G','HadGEM2-ES','inmcm4','IPSL-CM5A-LR','IPSL-CM5A-MR','MIROC5',$
                'MIROC-ESM','MPI-ESM-LR','MRI-CGCM3','NorESM1-M']
; model_name = ['bccr_bcm2_0','cccma_cgcm3_1','cnrm_cm3','csiro_mk3_0','csiro_mk3_5',$
;               'gfdl_cm2_0','gfdl_cm2_1',$
;               'inmcm3_0','ipsl_cm4','miroc3_2_medres','miub_echo_g',$
;               'mpi_echam5','mri_cgcm2_3_2a','ncar_ccsm3_0','ncar_pcm1','ukmo_hadcm3','ukmo_hadgem1']

nnmod=n_elements(model_name)

ntime=150
model=replicate(-999.,nnmod,ntime)
;--
;varnamenc='PCs'
;file_mod=path1+varname+'_eofs2_JUN-DEC_1950-1999.nc'
file_mod=path1+varname+'-JUN-DEC_1950-1999_PEAST.nc'
varnamenc=varname
peyl_readnc,tmp1,file=file_mod,var_name=varnamenc
;--
model[*,7:42]=reform(TMP1[0,0,*,*])*coeff
;model[*,0:55]=reform(TMP1[0,0,*,*])*coeff
;--
;file_mod=path2+varname+'_eofs2_JUN-DEC_2006-2049.nc'
file_mod=path2+varname+'-JUN-DEC_2006-2049_PEAST.nc'
varnamenc=varname+'_raw'
peyl_readnc,tmp2,file=file_mod,var_name=varnamenc
;--
model[*,63:92]=reform(TMP2[0,0,*,*])*coeff
;model[*,56:99]=reform(TMP2[0,0,*,*])*coeff
;--
;file_mod=path2+varname+'_eofs2_JUN-DEC_2050-2099.nc'
file_mod=path2+varname+'-JUN-DEC_2050-2099_PEAST.nc'
varnamenc=varname
peyl_readnc,tmp3,file=file_mod,var_name=varnamenc
;--
model[*,107:142]=reform(TMP3[0,0,*,*])*coeff
;model[*,100:149]=reform(TMP3[0,0,*,*])*coeff
;--
openw,unit,path+'hf_peast.txt',/get_lun
printf,unit,format='(22A15)','YEAR',model_name
FOR ny=0,ntime-1 DO BEGIN
    printf,unit,format='(I15,X,21F15.3)',1950+ny,model(*,ny)
ENDFOR
free_lun,unit

END

