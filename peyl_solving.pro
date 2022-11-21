;+
;========================================================================
; NAME:
;       PEYL_SOLVING
;
; PURPOSE: Solve a linear set of equation in order to optimize some parameters.
;       
;
; CALLING SEQUENCE: See definition of programme
;       
;
; INPUTS:
;
;    * MAT : array of (npar, nobs): matrice of coefficients that relates
;             the derivative of the modeled output with respect to the parameters
;             npar=nb parametres / nobs= nb observations
;
;    * DOBS : vector of observations (nobs)
;
;    * TYPE = 0: Lu decomposition for square problems
;             1: Svd decompostion for non square problems (more or
;                less observations than parameters)
;             2/21/22: Complete inversion with errors (Tarantola approach)
;                Cost function with both errors on obs and on prior param 
;                Case for the inversion of a matrix of nparxnpar
;                 (Inversion: [Mat^T # sig_obs^-1 # Mat + sig_par^-1])
;                2: pour inverser selon cholesky, 21 selon LU, et 22
;                selon SVD
;             3/31/32: Same as the 2/21/22 cases but with the
;             formulation to invert a matrix of nobsxnobs
;      (DEFAULT = 1)
;
;
; OPTIONAL INPUT PARAMETERS:
; 
;    * mprior : vector of prior value for parameters (npar)
;               REQUESTED for type 2,3,4 
;
;    * sig_obs: vector of prior error value for the observations (nobs)
;               corrospond to the standart deviations.
;               REQUESTED for type 2,3,4 
;
;    * sig_par: vector of prior error value for the parameters (npar)
;               corrospond to the standart deviations.
;               REQUESTED for type 2,3,4 
;
;    * mat_R: matrix of variances/covariances for the observation
;    errors... REPLACE sig_obs!
;
;    * mat_P: matrix of variances/covariances for the parameter
;    errors... REPLACE sig_par!
;
;    * tolerance_wp : tolerance factor when the inversion
;    implies SVD (type = 1 or 2) in order to cut the low or negative
;    sigular values. The factor is time the maximum sigular value to
;    define the threshold.  NOT REQUESTED (default = 0)
;
;    * m0prior : vector of prior value for parameters (npar) of the
;     first loop, in the case of iterative solution (cf tarantola..)
;     NOT required if no looping..
;   
;    * mod_prior : Values of the model a priori in the case of non
;    linear model; else assume to be Mat # mprior
;      ONLY for type 2,3,4
;      NOT REQUESTED
;       
;    * NOERASE: to avoid araising MAT on output if type = 0 or 1
;      NOT REQUESTED
;
;    * NOINFO: to avoid messages..
;
;
; OUTPUTS:
;
;    * MPOSTE : vector of optimised parameters (npar).
;      REQUESTED
;
;    * mcov : matrice of variance/covariance on output for the
;    parameters (npar,npar)
;      NOT REQUESTED
;
;    * mcor : matrice des correlations construite a partir de mcov
;      NOT REQUESTED
;
;    * dcov : matrice of variance/covariance on output for the
;    observations (nobs,nobs)
;      NOT REQUESTED
;
;    * chi2val : valeur du chi-square (2 fois la fonction cout a son
;      minimum) divise par le nombre d'obs; donc chi2val doit etre 
;      proche ou inferieur a 1...
;
;    * wp : vector of sigular values if SVD utilised (type 1 or 2)
;      NOT REQUESTED
;
;
; RESTRICTIONS:
; 
;
;========================================================================
PRO peyl_solving, mat, $
                  dobs, $
                  mposte, $
                  type=type,$
                  mprior=mprior, $
                  sig_obs=sig_obs,$
                  sig_par=sig_par,$
                  mat_P=mat_P,$
                  mat_R=mat_R,$
                  mcov=mcov,$
                  mcor=mcor,$
                  dcov=dcov,$
                  flag_cov_out=flag_cov_out,$
                  chi2val=chi2val,$
                  m0prior=m0prior,$
                  mod_prior=mod_prior, $
                  wp=wp,$
                  tolerance_wp=tolerance_wp,$
                  noinfo=noinfo,$
                  noerase=noerase,$
                  double=double


;---------------------------------------------------------------
;       Few checks..
;---------------------------------------------------------------
FORWARD_FUNCTION inverse_mat

ON_ERROR,2

if not ccg_vdef(type) then type = 1
if not ccg_vdef(noerase) then noerase = 0
if not KEYWORD_SET(noinfo) then info = 1 else info = 0
if not ccg_vdef(tolerance_wp) then tolerance_wp = 0. 
if not ccg_vdef(flag_cov_out) then flag_cov_out = 1 
if not ccg_vdef(double) then double = 1 

if double then typeval=0.D else typeval=0.

;------- Recalcul de npar/nobs
npar = n_elements(mat(*,0))
nobs = n_elements(dobs)

if (type ne 0 and type ne 1 and $
    type ne 2 and type ne 21 and type ne 22 and $
    type ne 3 and type ne 31 and type ne 32) then $
  message,'type inconnue !!'


;---------------------------------------------------------------
;   resolution simple sans erreurs associees pour Square matrix
;---------------------------------------------------------------

if type eq 0 then begin
    if npar ne nobs then message,'Type impossible selon npar/nobs (need square matrix)'
    if info then print,'LU Resolution dans cas npar=nobs sans erreurs..'
    if noerase then mat_temp = mat
    ludc, mat, index
    mposte = lusol(mat, index, dobs)
    if noerase then mat = temporary(mat_temp)    
    return
endif


;---------------------------------------------------------------
;   resolution simple sans erreurs associees selon SVD
;---------------------------------------------------------------

if type eq 1 then begin
    if info then print,'SVD Resolution sans erreurs..'
    if noerase then mat_temp = mat
    la_svd, mat, wp, u, v
    n = n_elements(wp)
    MWP = fltarr(n,n)   
    wmax = max(wp)
    THRESH=tolerance_wp*WMAX
    for k=0,n-1 do $
      if (abs(wp(k)) gt THRESH) then MWP(k,k) = 1. / wp(k)
    mposte = reform (v ## MWP ## transpose(u) ## dobs )
    if noerase then mat = temporary(mat_temp)    
    return
endif


;---------------------------------------------------------------
;  Resolution complete selon tarantola: FIRST CHECK / Initialisation
;  REM : il faut travailler en double precision sinon 
;       la matrice a inverser n'est pas symetrique...
;
; si a = (m,n) : m column, n rows 
; et b = (l,m) : l column, m rows
; a ## b = matrix_multiply(b,a)
; sinon:
; a ## transpose(b) = matrix_multiply(b,a,/atranspose)
; transpose(a) ## b = matrix_multiply(b,a,/btranspose)
;
;---------------------------------------------------------------


if info then print,'TARANTOLA Resolution avec erreurs.. (BAYESIEN)'

                                ;--- Petites verif..
if n_elements(mprior) ne npar then message,'Probleme mprior mal defini..' 
mat_P_flag = 0
if n_elements(mat_P) eq long(npar)^2 then begin
    if info then print,'Use mat_P for prior error variances/covariances ' 
    mat_P_flag = 1
endif else if n_elements(sig_par) ne npar then message,'Probleme sig_par mal defini..' 
mat_R_flag = 0
if n_elements(mat_R) eq long(nobs)^2 then begin
    if info then print,'Use mat_R for Obs error variances/covariances '
    mat_R_flag = 1    
endif else if n_elements(sig_obs) ne nobs then message,'Probleme sig_obs mal defini..' 

                                ;--- Rem: si prbl non lineaire
                                ;    mod_prior est fournit sinon on le
                                ;    calcul..
if n_elements(mod_prior) ne nobs then mod_prior = mat ## mprior

                                ;--- Special pour inclure cas
                                ;    iteratif..
if n_elements(m0prior) ne npar then m0prior=mprior

                                ;--- Choice of type of inversion (see
                                ;    below).. 
type_inv = 'chol'
if (type eq 21 or type eq 31) then type_inv = 'lu'
if (type eq 22 or type eq 32) then type_inv = 'svd'

                                ;---- Calcul de l'eccart aux Obs.
ddobs = dobs - mod_prior

                                ;--- Rajout de l'ecart a mprior
                                ;    initial (m0prior) pour une
                                ;    solution iterative dans cas non
                                ;    lineaire.. 
dprior = mprior - m0prior
if total(abs(dprior)) ne 0. then $
  ddobs = temporary(ddobs) + (mat ## dprior) 
    

;---------------------------------------------------------------
;       Formulation sur "npar" de Tarantola (Ht.R-1.H + P-1)-1
;---------------------------------------------------------------

if (type eq 2 or type eq 21 or type eq 22) then begin
    print,'Resolution selon formulation [Ht.R-1.H + P-1]-1'



                                ;--- calcul de Ht.R-1
    if mat_R_flag eq 0 then begin
        mat_mn = transpose(mat) ;--- (nobs,npar)
        for j=0,nobs-1 do mat_mn(j,*) = mat_mn(j,*) * 1./(sig_obs(j))^2
    endif else begin
        mat_R1 = INVERSE_MAT(mat_R,type=type_inv,tolerance_wp=tolerance_wp,info=info)
        mat_mn = matrix_multiply(mat_R1,mat,/btranspose)
    endelse
   
                                ;--- Compute matrix to be inverted:
                                ;    Ht.R-1.H + P-1
    mat_nn = mat_mn ## mat
    if mat_P_flag eq 0 then begin
        for i=0,npar-1 do mat_nn(i,i) = mat_nn(i,i) + 1./(sig_par(i))^2 
    endif else begin
        mat_P1 = INVERSE_MAT(mat_P,type=type_inv,tolerance_wp=tolerance_wp,info=info)
        mat_nn = temporary(mat_nn) + mat_P1 
    endelse

                                ;--- Inversion de mat_nn ce qui donne
                                ;    la matrice de covariance mcov
    mcov = INVERSE_MAT(mat_nn,type=type_inv,tolerance_wp=tolerance_wp,info=info)

    
                                ;--- Calcul des covariances des
                                ;    observations: H.mcov.Ht (on
                                ;    ecrase mat!)
    if ccg_vdef(dcov) then begin
        dcov = mat ## mcov ## transpose(mat) ;-- a ameliorer pour espace!
    endif
    
                                ;--- Calcul final des flux a
                                ;    posteriori
    mposte = mat_mn ## ddobs
    mposte = m0prior + mcov ## mposte
endif 


;---------------------------------------------------------------
;       Formulation sur "ntot" de Tarantola 
;      X = Xb + P.Ht.[H.P.Ht + R]-1.(Y-H.Xb)
;      Pa = P - P.Ht.(H.P.Ht+R)-1.H.P 
;---------------------------------------------------------------

if (type eq 3 or type eq 31 or type eq 32) then begin
    print,'Resolution selon formulation [H.P.Ht + R]-1'

                                ;---- Calcul de P.Ht
    if mat_P_flag eq 0 then begin
        mat_nm = transpose(mat) ;-- (nobs,npar)
        for i=0,npar-1 do mat_nm(*,i) = mat_nm(*,i) * sig_par(i)^2 
    endif else begin
        mat_nm = matrix_multiply(mat,mat_P,/atranspose)
    endelse

                                ;---- Calcul de H.P.Ht + R
    mat_mm = mat ## mat_nm
    if mat_R_flag eq 0 then begin
        for j=0,nobs-1 do mat_mm(j,j) = mat_mm(j,j) + sig_obs(j)^2
    endif else begin
        mat_mm = temporary(mat_mm) + mat_R
    endelse

                                ;--- Inversion of main matrix
    mat_mm1 = INVERSE_MAT(mat_mm,type=type_inv,tolerance_wp=tolerance_wp,info=info)
    
                                ;--- Calcul final des flux a
                                ;    posteriori Xb +
                                ;    P.Ht.[H.P.Ht+R]-1.(Y-H.Xb) 
;;;; check
;    mat_nm2 = mat_nm
    mat_nm = mat_nm ## mat_mm1
;    for i=0,npar-1 do begin
;        temp = mat_nm2(*,i) ## mat_mm1 ;;; check!
;        mat_nm2(*,i) = temp
;    endfor
;    print,max(abs(mat_nm - mat_nm2))
;    stop
    mposte = m0prior + mat_nm ## ddobs

                                ;--- Calcul des covariances des
                                ;    parametres du model:
                                ;    P-P.Ht.(H.P.Ht+R)-1.H.P 
    if flag_cov_out ge 1 then begin
        mcov = mat_nm ## mat
        if mat_P_flag eq 0 then begin
            for i=0,npar-1 do mcov(i,*) = mcov(i,*) * sig_par(i)^2 ;; check
        endif else begin
            mcov = mcov ## mat_P
;;;; Depend du check precedent !
;            stop
;            for i=0,npar-1 do begin
;                temp = mcov(*,i) ## mat_P ;;; check!
;                mcov(*,i) = temp
;            endfor
            mcov = mat_P - temporary(mcov)
        endelse
    endif   

endif


;-------- Calcul des correlations entre parametres du model
if ccg_vdef(mcor) then begin
    mcor = fltarr(npar,npar)
    for j=0,npar-1 do for i=0,npar-1 do $
      mcor(i,j) = mcov(i,j) / sqrt(mcov(i,i)*mcov(j,j))
endif
                                
;--------- Calcul des chi2
if ccg_vdef(chi2val) then begin
    chi2val = fltarr(3,2)

                                ;--- Rem: si prbl non lineaire
                                ;    calcul impossible car il faut
                                ;    calculer mod_poste completement..
    mod_poste = mat ## mposte

                                ;-- Obs part
    if mat_R_flag eq 0 then begin
        chi2val(1,0) = total( ((dobs-mod_poste)/sig_obs)^2 ) 
        chi2val(1,1) = total( ((dobs-mod_prior)/sig_obs)^2 ) 
    endif else begin
        if n_elements(mat_R1) eq 0 then $
          mat_R1 = INVERSE_MAT(mat_R,type=type_inv,tolerance_wp=tolerance_wp,info=info)
        chi2val(1,0) = (dobs-mod_poste) ## mat_R1 ## transpose(dobs-mod_poste) ;;; check
        chi2val(1,1) = (dobs-mod_prior) ## mat_R1 ## transpose(dobs-mod_prior) ;;; check
    endelse
                                ;-- param part
    if mat_P_flag eq 0 then begin
        chi2val(2,0) = total( ((m0prior-mposte)/sig_par)^2 )
    endif else begin
        if n_elements(mat_P1) eq 0 then $
          mat_P1 = INVERSE_MAT(mat_P,type=type_inv,tolerance_wp=tolerance_wp,info=info)
        chi2val(2,0) = (m0prior-mposte) ## mat_P1 ## transpose(m0prior-mposte) ;;; check
    endelse 
    chi2val(0,0) = chi2val(1,0)+chi2val(2,0)
    chi2val(0,1) = chi2val(1,1)+chi2val(2,1)
    chi2val = chi2val / nobs

endif
   
END


;--------------------------------------------------------
;  Petite fonction locale pour inverser une matrice..
;--------------------------------------------------------
FUNCTION inverse_mat, mat_in, type=type, $
                      tolerance_wp=tolerance_wp, $
                      info=info,double=double

if not ccg_vdef(tolerance_wp) then tolerance_wp = 0. 
if not ccg_vdef(info) then info = 0 
if not ccg_vdef(double) then double = 1 

if double then typeval=0.D else typeval=0.

if type ne 'lu' and type ne 'chol' and type ne 'svd' then $
  message,'type pour inversion mal definit...'

s = size(mat_in)
if s(0) eq 1 then begin
    return,1./mat_in
endif
nn = n_elements(mat_in(*,0))

;--- pb : condition number of mat_in
if info then begin
    cond_num = cond(mat_in)
    print, 'CONDITION NUMBER OF THE MATRIX to invert : ',cond_num
    if cond_num eq -1 then print,'Matrix not invertible !!'
endif

                                ;---- Inversion mat_mm selon cholesky
if type eq 'chol' then begin
    if info then print,'Inversion selon cholesky decomposition'
    mat_inv = mat_in
    CHOLDC,mat_in,p,/double
    for n=0,nn-1 do begin
        b = dblarr(nn) & b(n) = 1.
        mat_inv(n,*) = CHOLSOL(mat_in,p,b,/double)
    endfor
endif
                                ;--- Inversion selon LU
if type eq 'lu' then begin
    if info then print,'Inversion selon LU decomposition'
    mat_inv = mat_in
    LUDC,mat_in,index,/double
    for n=0,nn-1 do begin
        b = dblarr(nn) & b(n) = 1.
        mat_inv(n,*) = LUSOL(mat_in,index,b,/double)
    endfor
endif 

                                ;--- Inversion selon SVD
if type eq 'svd' then begin
    if info then print,'Inversion selon SVD'
    la_svd,mat_in, wp, u, v, /double

                                ;--- Calcul de cw
    cw = wp
    foo = where(CW LT 0., nfoo)
    if nfoo gt 0. then begin
        print,'valeurs propres negative dans la svd... ',cw(foo) 
        stop 
    endif
    
                                ;---- coupure des valeurs propres trop
                                ;     petites de la matrice CW
    if tolerance_wp gt 0. then begin
        wmax = max(wp)
        foo = where(CW LT (tolerance_wp*WMAX), nfoo)
        if nfoo gt 0. then begin
            print,'valeurs propres petites reset to zero... '
            CW(foo) = 0.
        endif
    endif    
                                ;--- Calcul de ww
    ww = replicate(typeval,nn,nn)
    for mu=0,nn-1 do if (cw(mu) ne 0.) then ww(mu,mu)=1./cw(mu)

                                ;--- Inversion de cnn
    mat_inv = v ## ww ## transpose(u)
endif


return, mat_inv
END

;-
