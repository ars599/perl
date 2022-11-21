;+
;=====================================================================
; NAME:
;       PEYL_REGDEF
;
; PURPOSE:
;       Function that give the full name of a region given its
;       tag (oceanic and terrestrial regions)...
;
; CALLING SEQUENCE:
;       result = peyl_regdef (tag)
;
; INPUTS:
;	tag : array of string(s) containing the tag(s)
;
;
; OPTIONAL INPUT PARAMETERS:
;
;	/noinfo : to avoid message print out 
;
; OUTPUTS:
;	result : an (array) of string containing the full 
;                name for the region(s)
;
; RESTRICTIONS:
; 
;
;======================================================================

FUNCTION peyl_regdef, tag, $
               noinfo=noinfo

if keyword_set(noinfo) then info=0 else info=1

nreg = n_elements(tag)
if nreg gt 1 then aout  = strarr(nreg) else aout=''

for n=0,nreg-1 do begin

    case tag(n) of 
        'na10' : aout(n) = 'Tundra'
        'na4' : aout(n) = 'N. Amer. Conif.'
        'eu4' : aout(n) = 'Europe Conif.'
        'as4' : aout(n) = 'Siberia Conif.'
        'na12' : aout(n) = 'N. Amer. Decid.'
        'eu12' : aout(n) = 'Europe Decid.'
        'as12' : aout(n) = 'Asia Decid.'
        'as7' : aout(n) = 'Asia Savana'
        'sa1' : aout(n) = 'S. Amer. Trop.'
        'sa6' : aout(n) = 'S. Amer. Savana'
        'sa12' : aout(n) = 'S. Amer. Decid.'
        'af1' : aout(n) = 'Africa Trop.'
        'af6n' : aout(n) = 'N. Africa Savana'
        'af6s' : aout(n) = 'S. Africa Savana'
        'as1' : aout(n) = 'Asia Trop.'
        'aus' : aout(n) = 'Austral'
        'sud' : aout(n) = 'Southern Hemisph.'
        'spac' : aout(n) = 'S. Pacific'
        'satl' : aout(n) = 'S. Atlantic'
        'sindo' : aout(n) = 'S. Indian West'
        'sinde' : aout(n) = 'S. Indian East'
        'paceqo' : aout(n) = 'Eq. Pacific West'
        'paceqe' : aout(n) = 'Eq. Pacific East'
        'atleq' : aout(n) = 'Eq. Atlantic'
        'indeq' : aout(n) = 'Eq. Indian'
        'npacs' : aout(n) = 'N. Pacific South'
        'npacn' : aout(n) = 'N. Pacific North'
        'natls' : aout(n) = 'N. Atlantic South'
        'natln' : aout(n) = 'N. Atlantic North'
        'nord' : aout(n) = 'N. Artic'
        'sub' : aout(n) = 'Sub Artic'
        'eq' : aout(n) = 'Equatorial'
        'atl' : aout(n) = 'Atlantic'
        'pac' : aout(n) = 'Pacific'
        'sam' : aout(n) = 'S. America'
        'afr' : aout(n) = 'Africa'
        'asi' : aout(n) = 'Asia'
        'glo' : aout(n) = 'Global'
        else : aout(n) = '?'
    endcase
      
endfor

return,aout
END
;-

