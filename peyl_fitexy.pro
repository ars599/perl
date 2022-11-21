;+
; NAME:
;       FITEXY
; PURPOSE:
;       Best straight-line fit to data with errors in both coordinates
; EXPLANATION:
;       Linear Least-squares approximation in one-dimension (y = a + b*x),
;               when both x and y data have errors
;
; CALLING EXAMPLE:
;       FITEXY, x, y, A, B, X_SIG= , Y_SIG= , [sigma_A_B, chi_sq, q, TOL=]
;
; INPUTS:
;       x = array of values for independent variable.
;       y = array of data values assumed to be linearly dependent on x.
;
; REQUIRED INPUT KEYWORDS:
;       X_SIGMA = scalar or array specifying the standard deviation of x data.
;       Y_SIGMA = scalar or array specifying the standard deviation of y data.
;
; OPTIONAL INPUT KEYWORD:
;       TOLERANCE = desired accuracy of minimum & zero location, default=1.e-3.
;
; OUTPUTS:
;       A_intercept = constant parameter result of linear fit,
;       B_slope = slope parameter, so that:
;                       ( A_intercept + B_slope * x ) approximates the y data.
; OPTIONAL OUTPUT:
;       sigma_A_B = two element array giving standard deviation of 
;                A_intercept and B_slope parameters, respectively.
;                The standard deviations are not meaningful if (i) the
;                fit is poor (see parameter q), or (ii) b is so large that
;                the data are consistent with a vertical (infinite b) line.
;                If the data are consistent with *all* values of b, then
;                sigma_A_B = [1e33,e33]  
;       chi_sq = resulting minimum Chi-Square of Linear fit, scalar
;       q - chi-sq probability, scalar (0-1) giving the probability that
;              a correct model would give a value equal or larger than the
;              observed chi squared.   A small value of q indicates a poor
;              fit, perhaps because the errors are underestimated.
;
; COMMON:
;       common fitexy, communicates the data for computation of chi-square.
;
; PROCEDURE CALLS:
;       CHISQ_FITEXY()            ;Included in this file
;       MINF_BRACKET, MINF_PARABOLIC, ZBRENT    ;Included in this file 
;       MOMENT(), CHISQR_PDF()     ;In standard IDL distribution
;
; PROCEDURE:
;       From "Numerical Recipes" column by Press and Teukolsky: 
;       in "Computer in Physics",  May, 1992 Vol.6 No.3
;       Also see the 2nd edition of the book "Numerical Recipes" by Press et al.
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC  September 1992.
;       Now returns q rather than 1-q   W. Landsman  December 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use CHISQR_PDF, MOMENT instead of STDEV,CHI_SQR1 W. Landsman April 1998
;       Fixed typo for initial guess of slope, this error was nearly
;             always insignificant          W. Landsman   March 2000
;-
function chisq_fitexy, B_angle
;
; NAME:
;       chisq_fitexy
; PURPOSE:
;       Function minimized by fitexy  (computes chi-square of linear fit).
;       It is called by minimization procedures during execution of fitexy.
; CALLING SEQUENCE:
;               chisq = chisq_fitexy( B_angle )
; INPUTS:
;       B_angle = arc-tangent of B_slope of linear fit.
; OUTPUTS:
;       Result of function = chi_square - offs  (offs is in COMMON).
; COMMON:
;       common fitexy, communicates the data from pro fitexy.
; PROCEDURE:
;       From "Numerical Recipes" column: Computer in Physics Vol.6 No.3
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.

  common fitexy, xx, yy, sigx, sigy, ww, Ai, offs

        B_slope = tan( B_angle )
        ww = 1/( ( (B_slope * sigx)^2 + sigy^2 ) > 1.e-30 )
        if N_elements( ww ) EQ 1 then sumw = ww * N_elements( xx ) $
                                 else sumw = total( ww )
        y_Bx = yy - B_slope * xx
        Ai = total( ww * y_Bx )/sumw

return, total( ww * (y_Bx - Ai)^2 ) - offs
end
;-------------------------------------------------------------------------------
;----------------------------------------------------------------------------
function ZBRENT, x1, x2, FUNC_NAME=func_name,    $
                         MAX_ITERATIONS=maxit, TOLERANCE=TOL
;+
; NAME:
;     ZBRENT
; PURPOSE:
;     Find the zero of a 1-D function up to specified tolerance.
; EXPLANTION:
;     This routine assumes that the function is known to have a zero.
;     Adapted from procedure of the same name in "Numerical Recipes" by
;     Press et al. (1992), Section 9.3
;
; CALLING:
;       x_zero = ZBRENT( x1, x2, FUNC_NAME="name" )
;
; INPUTS:
;       x1, x2 = scalars, 2 points which bracket location of function zero,
;                                               that is, F(x1) < 0 < F(x2).
;       Note: computations are performed with
;       same precision (single/double) as the inputs and user supplied function.
;
; REQUIRED INPUT KEYWORD:
;       FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px )
;               where:  px = scalar independent variable, input.
;                       F = scalar value of function at px,
;                           should be same precision (single/double) as input.
;
; OPTIONAL INPUT KEYWORD:
;       MAX_ITER = maximum allowed number iterations, default=100.
;       TOLERANCE = desired accuracy of minimum location, default = 1.e-3.
;
; OUTPUTS:
;       Returns the location of zero, with accuracy of specified tolerance.
;
; PROCEDURE:
;       Brent's method to find zero of a function by using bracketing,
;       bisection, and inverse quadratic interpolation,
;
; EXAMPLE:
;       Find the root of the COSINE function between 1. and 2.  radians
;
;        IDL> print, zbrent( 1, 2, FUNC = 'COS')
;
;       and the result will be !PI/2 within the specified tolerance
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       FV.1994, mod to check for single/double prec. and set zeps accordingly.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
        if N_params() LT 2 then begin
             print,'Syntax - result = ZBRENT( x1, x2, FUNC_NAME = ,
             print,'                  [ MAX_ITER = , TOLERANCE = ])
             return, -1
        endif

        if N_elements( TOL ) NE 1 then TOL = 1.e-3
        if N_elements( maxit ) NE 1 then maxit = 100

        sz1 = size( x1 )
        sz2 = size( x2 )

        if (sz1[sz1[0]+1] EQ 5) OR (sz2[sz2[0]+1] EQ 5) then begin
                xa = double( x1 )
                xb = double( x2 )
                zeps = 1.d-14   ;machine epsilon in double precision.
          endif else begin
                xa = x1
                xb = x2
                zeps = 1.e-7    ;machine epsilon, smallest add, single prec.
           endelse

        fa = call_function( func_name, xa )
        fb = call_function( func_name, xb )
        fc = fb

        if (fb*fa GT 0) then begin
                message,"root must be bracketed by the 2 inputs",/INFO
                return,xa
           endif

        for iter = 1,maxit do begin

                if (fb*fc GT 0) then begin
                        xc = xa
                        fc = fa
                        Din = xb - xa
                        Dold = Din
                   endif

                if (abs( fc ) LT abs( fb )) then begin
                        xa = xb   &   xb = xc   &   xc = xa
                        fa = fb   &   fb = fc   &   fc = fa
                   endif

                TOL1 = 0.5*TOL + 2*abs( xb ) * zeps     ;Convergence check
                xm = (xc - xb)/2.

                if (abs( xm ) LE TOL1) OR (fb EQ 0) then return,xb

                if (abs( Dold ) GE TOL1) AND (abs( fa ) GT abs( fb )) then begin

                        S = fb/fa       ;attempt inverse quadratic interpolation

                        if (xa EQ xc) then begin
                                p = 2 * xm * S
                                q = 1-S
                          endif else begin
                                T = fa/fc
                                R = fb/fc
                                p = S * (2*xm*T*(T-R) - (xb-xa)*(R-1) )
                                q = (T-1)*(R-1)*(S-1)
                           endelse

                        if (p GT 0) then q = -q
                        p = abs( p )
                        test = ( 3*xm*q - abs( q*TOL1 ) ) < abs( Dold*q )

                        if (2*p LT test)  then begin
                                Dold = Din              ;accept interpolation
                                Din = p/q
                          endif else begin
                                Din = xm                ;use bisection instead
                                Dold = xm
                           endelse

                  endif else begin

                        Din = xm    ;Bounds decreasing to slowly, use bisection
                        Dold = xm
                   endelse

                xa = xb
                fa = fb         ;evaluate new trial root.

                if (abs( Din ) GT TOL1) then xb = xb + Din $
                                        else xb = xb + TOL1 * (1-2*(xm LT 0))

                fb = call_function( func_name, xb )
          endfor

        message,"exceeded maximum number of iterations: "+strtrim(iter,2),/INFO

return, xb
end
;-----------------------------------------------------------------------
pro peyl_fitexy, x, y, A_intercept, B_slope, sigma_A_B, chi_sq, q, TOLERANCE=Tol, $
                                        X_SIGMA=x_sigma, Y_SIGMA=y_sigma
  common fitexy, xx, yy, sigx, sigy, ww, Ai, offs

  if N_params() LT 4 then begin
     print,'Syntax -  fitexy, x, y, A, B, X_SIG=sigx, Y_SIG=sigy,' 
     print,'                  [sigma_A_B, chi_sq, q, TOLERANCE = ]'
     return
  endif
        dummy = moment(x,sdev=stdevx) & dummy = moment(y,sdev=stdevy)
        scale =  stdevx/stdevy       ;Updated 14-Mar-2000
        xx = x
        yy = y * scale
        sigx = x_sigma
        sigy = y_sigma * scale

;Compute first guess for B_slope using standard 1-D Linear Least-squares fit,
; where the non-linear term involving errors in x are ignored.
; (note that Tx is a transform to reduce roundoff errors)

        ww = sigx^2 + sigy^2
        if N_elements( ww ) EQ 1 then sumw = ww * N_elements( xx ) $
                                 else sumw = total( ww )
        Sx = total( xx * ww )
        Tx = x - Sx/sumw
        B = total( ww * yy * Tx ) / total( ww * Tx^2 )

;Find the minimum chi-sq while including the non-linear term (B * sigx)^2
; involving variance in x data (computed by function chisq_fitexy):
; using minf_bracket (=MNBRAK) and minf_parabolic (=BRENT)
        offs = 0
        ang = [ 0, atan( B ), 1.571 ]
        chi = fltarr( 3 )
        for j=0,2 do chi[j] = chisq_fitexy( ang[j] )    ;this is for later...

        if N_elements( Tol ) NE 1 then Tol=1.e-3
        a0 = ang[0]
        a1 = ang[1]
        minf_bracket, a0,a1,a2, c0,c1,c2, FUNC="chisq_fitexy"
        minf_parabolic, a0,a1,a2, Bang, chi_sq, FUNC="chisq_fitexy", TOL=Tol

        if N_params() EQ 7 then q = 1 - chisqr_pdf( chi_sq, N_elements(x) - 2 )
        A_intercept = Ai        ;computed in function chisq_fitexy
        ang = [a0,a1,a2,ang]
        chi = [c0,c1,c2,chi]

;Now compute the variances of estimated parameters,
; by finding roots of ( (chi_sq + 1) - chisq_fitexy ).
;Note: ww, Ai are computed in function chisq_fitexy.

        offs = chi_sq + 1
        wc = where( chi GT offs, nc )

        if (nc GT 0) then begin

                angw = [ang[wc]]
                d1 = abs( angw - Bang ) MOD !PI
                d2 = !PI - d1
                wa = where( angw LT Bang, na )

                if (na GT 0) then begin
                        d = d1[wa]
                        d1[wa] = d2[wa]
                        d2[wa] = d
                   endif

                Bmax = zbrent( Bang,Bang+max(d1),F="chisq_fitexy",T=Tol ) -Bang
                Amax = Ai - A_intercept
                Bmin = zbrent( Bang,Bang-min(d2),F="chisq_fitexy",T=Tol ) -Bang
                Amin = Ai - A_intercept

                if N_elements( ww ) EQ 1 then r2 = 2/( ww * N_elements( x ) ) $
                                         else r2 = 2/total( ww )

                sigma_A_B = [ Amin^2 + Amax^2 + r2 , Bmin^2 + Bmax^2 ]
                sigma_A_B = sqrt( sigma_A_B/2 ) / (scale*[1,cos(Bang)^2])

          endif else sigma_A_B = [1.e33,1.e33]

;Finally, transform parameters back to orignal units.

        A_intercept = A_intercept/scale
        B_slope = tan( Bang )/scale
return
end
;--------------------------------------------------------------------------
pro minF_parabolic, xa,xb,xc, xmin, fmin, FUNC_NAME=func_name,    $
                                          MAX_ITERATIONS=maxit,   $
                                          TOLERANCE=TOL,          $
                                          POINT_NDIM=pn, DIRECTION=dirn
;+
; NAME:
;       MINF_PARABOLIC
; PURPOSE:
;       Minimize a function using Brent's method with parabolic interpolation
; EXPLANATION:
;       Find a local minimum of a 1-D function up to specified tolerance.
;       This routine assumes that the function has a minimum nearby.
;       (recommend first calling minF_bracket, xa,xb,xc, to bracket minimum).
;       Routine can also be applied to a scalar function of many variables,
;       for such case the local minimum in a specified direction is found,
;       This routine is called by minF_conj_grad, to locate minimum in the 
;       direction of the conjugate gradient of function of many variables.
;
; CALLING EXAMPLES:
;       minF_parabolic, xa,xb,xc, xmin, fmin, FUNC_NAME="name"  ;for 1-D func.
;  or:
;       minF_parabolic, xa,xb,xc, xmin, fmin, FUNC="name", $
;                                         POINT=[0,1,1],   $
;                                         DIRECTION=[2,1,1]     ;for 3-D func.
; INPUTS:
;       xa,xb,xc = scalars, 3 points which bracket location of minimum,
;               that is, f(xb) < f(xa) and f(xb) < f(xc), so minimum exists.
;               When working with function of N variables
;               (xa,xb,xc) are then relative distances from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION.
; INPUT KEYWORDS:
;      FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px )
;               where:
;                       px = scalar or vector of independent variables, input.
;                       F = scalar value of function at px.
;
;      POINT_NDIM = when working with function of N variables,
;               use this keyword to specify the starting point in N-dim space.
;               Default = 0, which assumes function is 1-D.
;      DIRECTION = when working with function of N variables,
;               use this keyword to specify the direction in N-dim space
;               along which to bracket the local minimum, (default=1 for 1-D).
;               (xa, xb, xc, x_min are then relative distances from POINT_NDIM)
;      MAX_ITER = maximum allowed number iterations, default=100.
;      TOLERANCE = desired accuracy of minimum location, default=sqrt(1.e-7).
; OUTPUTS:
;       xmin = estimated location of minimum.
;               When working with function of N variables,
;               xmin is the relative distance from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION,
;               so that min. Loc. Pmin = Point_Ndim + xmin * Direction.
;       fmin = value of function at xmin (or Pmin).
; PROCEDURE:
;       Brent's method to minimize a function by using parabolic interpolation.
;       Based on function BRENT in Numerical Recipes in FORTRAN (Press et al. 
;       1992),  sec.10.2 (p. 397).
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
        zeps = 1.e-7                    ;machine epsilon, smallest addition.
        goldc = 1 - (sqrt(5)-1)/2       ;complement of golden mean.

        if N_elements( TOL ) NE 1 then TOL = sqrt( zeps )
        if N_elements( maxit ) NE 1 then maxit = 100

        if N_elements( pn ) LE 0 then begin
                pn = 0
                dirn = 1
           endif

        xLo = xa < xc
        xHi = xa > xc
        xmin = xb
        fmin = call_function( func_name, pn + xmin * dirn )
        xv = xmin  &  xw = xmin
        fv = fmin  &  fw = fmin
        es = 0.

        for iter = 1,maxit do begin

                goldstep = 1
                xm = (xLo + xHi)/2.
                TOL1 = TOL * abs(xmin) + zeps
                TOL2 = 2*TOL1

                if ( abs( xmin - xm ) LE ( TOL2 - (xHi-xLo)/2. ) ) then return

                if (abs( es ) GT TOL1) then begin

                        r = (xmin-xw) * (fmin-fv)
                        q = (xmin-xv) * (fmin-fw)
                        p = (xmin-xv) * q + (xmin-xw) * r
                        q = 2 * (q-r)
                        if (q GT 0) then p = -p
                        q = abs( q )
                        etemp = es
                        es = ds

                        if (p GT q*(xLo-xmin)) AND $
                           (p LT q*(xHi-xmin)) AND $
                           (abs( p ) LT abs( q*etemp/2 )) then begin
                                ds = p/q
                                xu = xmin + ds
                                if (xu-xLo LT TOL2) OR (xHi-xu LT TOL2) then $
                                        ds = TOL1 * (1-2*((xm-xmin) LT 0))
                                goldstep = 0
                           endif
                   endif

                if (goldstep) then begin
                        if (xmin GE xm) then  es = xLo-xmin  else  es = xHi-xmin
                        ds = goldc * es
                   endif

                xu = xmin + (1-2*(ds LT 0)) * ( abs( ds ) > TOL1 )
                fu = call_function( func_name, pn + xu * dirn )

                if (fu LE fmin) then begin

                        if (xu GE xmin) then xLo=xmin else xHi=xmin
                        xv = xw  &  fv = fw
                        xw = xmin  &  fw = fmin
                        xmin = xu  &  fmin = fu
                
                  endif else begin

                        if (xu LT xmin) then xLo=xu else xHi=xu

                        if (fu LE fw) OR (xw EQ xmin) then begin

                                xv = xw  &  fv = fw
                                xw = xu  &  fw = fu

                          endif else if (fu LE fv) OR (xv EQ xmin) $
                                                   OR (xv EQ xw) then begin
                                xv = xu  &  fv = fu
                           endif
                   endelse
          endfor

        message,"exceeded maximum number of iterations: "+strtrim(iter,2),/INFO
return
end
;------------------------------------------------------------------------
pro minF_bracket, xa,xb,xc, fa,fb,fc, FUNC_NAME=func_name, $
                                                POINT_NDIM=pn, DIRECTION=dirn
;+
; NAME:
;       MINF_BRACKET
; PURPOSE:
;       Bracket a local minimum of a 1-D function with 3 points,
; EXPLANATION:
;       Brackets a local minimum of a 1-d function with 3 points,
;       thus ensuring that a minimum exists somewhere in the interval.
;       This routine assumes that the function has a minimum somewhere....
;       Routine can also be applied to a scalar function of many variables,
;       for such case the local minimum in a specified direction is bracketed,
;       This routine is called by minF_conj_grad, to bracket minimum in the 
;       direction of the conjugate gradient of function of many variables
; CALLING EXAMPLE:
;       xa=0  & xb=1                                    
;       minF_bracket, xa,xb,xc, fa,fb,fc, FUNC_NAME="name"      ;for 1-D func.
;  or:
;       minF_bracket, xa,xb,xc, fa,fb,fc, FUNC="name",     $
;                                         POINT=[0,1,1],   $
;                                         DIRECTION=[2,1,1]     ;for 3-D func.
; INPUTS:
;       xa = scalar, guess for point bracketing location of minimum.
;       xb = scalar, second guess for point bracketing location of minimum.
; KEYWORDS:
;       FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px )
;               where:
;                       px = scalar or vector of independent variables, input.
;                       F = scalar value of function at px.
;       POINT_NDIM = when working with function of N variables,
;               use this keyword to specify the starting point in N-dim space.
;               Default = 0, which assumes function is 1-D.
;       DIRECTION = when working with function of N variables,
;               use this keyword to specify the direction in N-dim space
;               along which to bracket the local minimum, (default=1 for 1-D).
;               (xa,xb,xc) are then relative distances from POINT_NDIM.
; OUTPUTS:
;       xa,xb,xc = scalars, 3 points which bracket location of minimum,
;               that is, f(xb) < f(xa) and f(xb) < f(xc), so minimum exists.
;               When working with function of N variables
;               (xa,xb,xc) are then relative distances from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION.
; OPTIONAL OUTPUT:
;       fa,fb,fc = value of function at 3 points which bracket the minimum,
;                       again note that fb < fa and fb < fc if minimum exists.
; PROCEDURE:
;       algorithm from Numerical Recipes (by Press, et al.), sec.10.1 (p.281).
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
        goldm = (sqrt(5)+1)/2           ;golden mean factor to march with.
        glimit = 100                    ;maximum factor to try.
        tiny = 1.e-19                   ;a tiny number to avoid divide by zero.

        if N_elements( pn ) LE 0 then begin
                pn = 0
                dirn = 1
           endif

        if (xa EQ xb) then xb = xa + 1
        fa = call_function( func_name, pn + xa * dirn )
        fb = call_function( func_name, pn + xb * dirn )

        if (fb GT fa) then begin
                x = xa  &  xa = xb  &  xb = x
                f = fa  &  fa = fb  &  fb = f
           endif

        xc = xb + goldm * (xb-xa)
        fc = call_function( func_name, pn + xc * dirn )

        while (fb GE fc) do begin

                zba = xb-xa
                zbc = xb-xc
                r = zba * (fb-fc)
                q = zbc * (fb-fa)
                delta = q-r
                sign = 1 - 2 * (delta LT 0)
                xu = xb - (zbc * q - zba * r)/(2* sign * (abs( delta ) > tiny) )
                ulim = xb + glimit * (xc-xb)

                if ( (xb-xu)*(xu-xc) GT 0 ) then begin

                        fu = call_function( func_name, pn + xu * dirn )

                        if (fu LT fc) then begin
                                xa = xb  &  xb = xu
                                fa = fb  &  fb = fu
                                return
                          endif else if (fu GT fb) then begin
                                xc = xu
                                fc = fu
                                return
                           endif

                        xu = xc - goldm * zbc
                        fu = call_function( func_name, pn + xu * dirn )

                 endif else if ( (xc-xu)*(xu-ulim) GT 0 ) then begin

                        fu = call_function( func_name, pn + xu * dirn )

                        if (fu LT fc) then begin
                                xb = xc  &  fb = fc
                                xc = xu  &  fc = fu
                                xu = xc + goldm * (xc-xb)
                                fu = call_function( func_name, pn + xu * dirn )
                           endif

                  endif else if ( (ulim-xc)*(xu-ulim) GE 0 ) then begin

                        xu = ulim
                        fu = call_function( func_name, pn + xu * dirn )

                   endif else begin

                        xu = xc + goldm * (xc-xb)
                        fu = call_function( func_name, pn + xu * dirn )
                    endelse
        
                xa = xb  &  xb = xc  &  xc = xu
                fa = fb  &  fb = fc  &  fc = fu
          endwhile
return
end
