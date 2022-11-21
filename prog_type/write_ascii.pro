PRO main

varname='pr'
scenario='rcp85'
path='/bm/gscratch1/fdelage/'+scenario+'/'
 model_name = ['ACCESS1-0','ACCESS1-3','bcc-csm1-1','CCSM4','CESM1-BGC','CESM1-CAM5',$
                'CNRM-CM5','CanESM2','CSIRO-Mk3-6-0','GISS-E2-R','GFDL-CM3',$
                'GFDL-ESM2G','HadGEM2-ES','inmcm4','IPSL-CM5A-LR','IPSL-CM5A-MR','MIROC5',$
                'MIROC-ESM','MPI-ESM-LR','MRI-CGCM3','NorESM1-M']
; model_name = ['bccr_bcm2_0','cccma_cgcm3_1','cnrm_cm3','csiro_mk3_0','csiro_mk3_5',$
;               'gfdl_cm2_0','gfdl_cm2_1',$
;               'inmcm3_0','ipsl_cm4','miroc3_2_medres','miub_echo_g',$
;               'mpi_echam5','mri_cgcm2_3_2a','ncar_ccsm3_0','ncar_pcm1','ukmo_hadcm3','ukmo_hadgem1']

nnmod=n_elements(model_name)

ntime=201
model=replicate(-999.,nnmod,ntime)
;--
FOR nm =  0, nnmod-1 DO BEGIN

    print,model_name(nm)
    ;file_mod=path+varname+'_'+model_name(nm)+'_r1i1p1_2081_2100_r240x120_seasmean.nc'
    file_mod=path+varname+'_'+model_name(nm)+'-mean.nc'
    ;file_mod=path+varname+'-'+model_name(nm)+'.nc'
    peyl_readnc,tmp,file=file_mod,var_name=varname
    ntimetmp=n_elements(reform(tmp))
    model(nm,0:ntimetmp-1)=reform(tmp)*86400
ENDFOR

openw,unit,path+varname+'-'+scenario+'-mean.txt',/get_lun
printf,unit,format='(22A15)','YEAR',model_name
FOR ny=0,ntime-1 DO BEGIN
    printf,unit,format='(I4,X,21F8.3)',1900+ny,model(*,ny)
ENDFOR
free_lun,unit

END

