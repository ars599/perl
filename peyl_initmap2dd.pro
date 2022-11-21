;+
;====================================================================
; NAME:
;       PEYL_INITMAP2DD
;
; PURPOSE:
;       Set default values for the map2dd program
;
; CALLING SEQUENCE:
;       peyl_initmap2dd
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

PRO peyl_initmap2dd
@map2d_com

min_col=32 & max_col=175  & nct=999.
units =' '
slabel=' ' 
label=' '
nleg =10    & legsiz=1.  & legform=''
imap=1      & tmap=1

END
;-


