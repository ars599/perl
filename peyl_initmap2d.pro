;+
;====================================================================
; NAME:
;       PEYL_INITMAP2D
;
; PURPOSE:
;       Set default values for the map2d program
;
; CALLING SEQUENCE:
;       peyl_initmap2d
;
; INPUTS:
;       none
;
; OPTIONAL INPUT PARAMETERS:
;	none
;
; OUTPUTS:
;	none
;       
;====================================================================

PRO peyl_initmap2d
@map2d_com

min_col=32 & max_col=172  & nct=900.
units =' '
slabel=' ' 
label=' '
nleg =9    & legsiz=1.  & legform=''
imap=1      & tmap=1

END
;-


