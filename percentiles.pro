;+
; NAME:
;        PERCENTILES
;
; PURPOSE:
;        compute percentiles of a data array
;
; CATEGORY:
;        statistical function
;
; CALLING SEQUENCE:
;        Y = PERCENTILES(DATA [,VALUE=value-array])
;
; INPUTS:
;        DATA --> the vector containing the data
;
; KEYWORD PARAMETERS:
;        VALUE --> compute specified percentiles
;        default is a standard set of min, 25%, median (=50%), 75%, and max
;        which can be used for box- and whisker plots.
;        The values in the VALUE array must lie between 0. and 1. !
;
; OUTPUTS:
;        The function returns an array with the percentile values or
;        -1 if no data was passed or value contains invalid numbers.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;      x = (findgen(31)-15.)*0.2     ; create sample data
;      y = exp(-x^2)/3.14159         ; compute some Gauss distribution
;      p = percentiles(y,value=[0.05,0.1,0.9,0.95])
;      print,p
;
;      IDL prints :  3.92826e-05  0.000125309     0.305829     0.318310

;
; MODIFICATION HISTORY:
;        mgs, 03 Aug 1997: VERSION 1.00
;        mgs, 20 Feb 1998: - improved speed and memory usage
;                (after tip from Stein Vidar on newsgroup)
;        mgs, 26 Aug 2000: - changed copyright to open source
;                          - median now correctly returned as average
;                            of two central values for data sets with
;                            even number of elements
;                          - modernized look and [] array notation
;       ct  19 Oct 2004:  formula of index calculation corrected
;                         according to definition given below
;        mgs, 31 Jan 2005: ct bug fix destroyed function for unsorted arrays. now fixed
;                          - added test_percentiles procedure for diehards
;
; PERCENTILE : DEFINITION
; 
; 100*p th percentile of a population is the value(p) for which:  
; 100*p    % of the population  <= value(p) 
; 100(1-p) % of the population  >  value(p) 
;
; 95th percentile 
; 95 % of the population is smaller or equal to the value(p)
; 5  % of the population is larger than the value(p)
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000 Martin Schultz
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
;
FUNCTION percentiles,data,value=value

   result = -1L
   n = n_elements(data)
   IF n LE 0 THEN RETURN,result ;; error : data not defined

   ;; Check if speficic percentiles requested - if not: set standard
   IF NOT keyword_set(value) THEN value = [ 0., 0.25, 0.5, 0.75, 1.0 ]

   ;; Save the sorted index array
   ix = sort(data)

   ;; Loop through percentile values, get indices and add to result
   ;; This is all we need since computing percentiles is nothing more
   ;; than counting in a sorted array

   FOR i=0L,n_elements(value)-1 DO BEGIN

      IF value[i] LT 0. OR value[i] GT 1. THEN RETURN,-1L

      ind=long(value[i]*n-1) > 0

      ;; Special treatment for median: 50*p % of the population >
      ;; value(p) and 50*p % of the population < value(p).
      ;; For of data sets with even number of elements: 
      ;; compute average between two center values
      IF ABS(value[i]-0.5) LT 1.e-3 THEN BEGIN
          IF  n MOD 2 EQ 0 AND n GT 1 THEN $
            thisresult = 0.5 * ( data[ix[ind]] + data[ix[ind+1]] )   $
;;;         thisresult = 0.5 * ( data[long(n/2)-1]+data[long(n/2)] )   $
;;;          ELSE thisresult = data[ix[ind]+1]
          ELSE thisresult = data[ix[ind+1]]
      ENDIF ELSE BEGIN
          thisresult = data[ix[ind]]
      ENDELSE
      IF i EQ 0 THEN result = thisresult  $
      ELSE result = [result, thisresult ]
   ENDFOR

   RETURN,result
END


pro test_percentiles

   a1 = findgen(100)+1.
   print,'Test 1: findgen(100)+1.'
   print,'   result : ',percentiles(a1),format='(A12,5f8.2)'
   print,'   should be ',1., 25., 50.5, 75., 100.,format='(A12,5f8.2)'
   print

   a2 = findgen(101)
   print,'Test 2: findgen(101)'
   print,'   result : ',percentiles(a2),format='(A12,5f8.2)'
   print,'   should be ',0., 24., 50., 74., 100.,format='(A12,5f8.2)'
   print

   a3 = [ 1., 2., 3., 4., 5. ]
   print,'Test 3: [ 1, 2, 3, 4, 5 ]'
   print,'   result : ',percentiles(a3),format='(A12,5f8.2)'
   print,'   should be ',1., 1., 3., 3., 5.,format='(A12,5f8.2)'
   print

   a4 = [ 1., 2., 4., 5. ]
   print,'Test 4: [ 1, 2, 4, 5 ]'
   print,'   result : ',percentiles(a4),format='(A12,5f8.2)'
   print,'   should be ',1., 1., 3., 4., 5.,format='(A12,5f8.2)'
   print

   a5 = [ 1., 2., 3., 4., 5., 6., 7., 8., 9., 10. ]
   print,'Test 5: [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]'
   print,'   result : ',percentiles(a5),format='(A12,5f8.2)'
   print,'   should be ',1., 2., 5.5, 7., 10.,format='(A12,5f8.2)'
   print

   a6 = [ 3., 4., 1., 2., 9., 7., 5., 6., 8., 10. ]
   print,'Test 6: [ 3, 4, 1, 2, 9, 7, 5, 6, 8, 10 ]'
   print,'   result : ',percentiles(a6),format='(A12,5f8.2)'
   print,'   should be the same as above!'

end

