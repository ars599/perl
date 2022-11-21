;$Id: moment.pro,v 1.3 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       PEYL_MOMENT
;
; PURPOSE:
;       This function computes the mean, variance, skewness and kurtosis
;       of an n-element vector.  
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Moment(X)
;
; INPUTS:
;       X:      An n-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;       MDEV:   Use this keyword to specify a named variable which returns
;               the mean absolute deviation of X.
;
;       SDEV:   Use this keyword to specify a named variable which returns
;               the standard deviation of X.
;
; EXAMPLE:
;       Define the n-element vector of sample data.
;         x = [65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71, 66, 65, 70]
;       Compute the mean, variance, skewness and kurtosis.
;         result = moment(x)
;       The result should be the 4-element vector: 
;       [66.7333, 7.06667, -0.0942851, -1.18258]
;
; PROCEDURE:
;       MOMENT computes the first four "moments" about the mean of an 
;       n-element vector of sample data. The computational formulas 
;       are given in the Univariate Statistics section of the Mathematics
;       Guide.
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, August 1994

function peyl_moment, x, mdev = mdev, sdev = sdev

  on_error, 2
  nx = n_elements(x)
  mean = total(x) / nx

  ;Mean absolute deviation (returned through the MDEV keyword).
  mdev = total(abs(x - mean)) / nx
  
  if nx le 1 then begin
;    print, 'WARNING IN PEYL_MOMENT : Not defined for scalar inputs !!!' 
    var=0. & sdev=0. & skew=0. & kurt=0.
    return, [mean, var, skew, kurt]
  endif
  
  var1 = total((x - mean)^2) / (nx-1.0)
  var2 = (total((x-mean)^2) - ((total((x-mean)))^2)/nx)/(nx-1.0)
  var =  (var1 + var2)/2.0
  ;Standard deviation (returned through the SDEV keyword).
  sdev = sqrt(var)

  if var ne 0 then begin 
    skew = total(((x-mean)/sdev)^3)/nx 
    kurt = total(((x-mean)/sdev)^4)/nx - 3.0
  endif else begin
;      print,'WARNING IN PEYL_MOMENT : Skewness and Kurtosis not defined for a sample variance of zero.'
      skew=0. & kurt=0.
  endelse
  return, [mean, var, skew, kurt]
end
;-







