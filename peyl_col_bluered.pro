;+
PRO peyl_col_bluered,NC=N
;
;  This makes a nice blue to red color table, with black for 0
; and white for the max value
;
tvlct,r,g,b,/get
N=N_ELEMENTS(r)

H = FLTARR(N) & H(0:N/2-1)=270. & H(N/2:N-1)=0.
L = 0.05+0.9*(1.-abs(indgen(N)/(N/2.)-1.)) <1.
S = FLTARR(N)*0. +0.8
L(0) = 0.
L(N-1) = 1.

!P.COLOR = 0
!P.BACKGROUND=N-1

tvlct,h,l,s,/HLS
END
;-
