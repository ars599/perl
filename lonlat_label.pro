PRO lonlat_label,xtickv=xtickv,ytickv=ytickv,XTICKNAME=XTICKNAME,YTICKNAME=YTICKNAME,TICKNAME_XINTERVAL=TICKNAME_XINTERVAL,TICKNAME_YINTERVAL=TICKNAME_YINTERVAL

IF n_elements(TICKNAME_XINTERVAL) eq 0 then TICKNAME_XINTERVAL=1
IF n_elements(TICKNAME_YINTERVAL) eq 0 then TICKNAME_YINTERVAL=1

XTICKNAME=''
FOR uu=0,n_elements(xtickv)-1 DO BEGIN
    tmp_str= ' '

;         IF xtickv(uu) LT   0 AND uu mod TICKNAME_XINTERVAL EQ 0 THEN tmp_str=string(180+xtickv(uu),format='(I3)')+'E'
;         IF xtickv(uu) GT   0 AND uu mod TICKNAME_XINTERVAL EQ 0 THEN tmp_str=string(180-xtickv(uu),format='(I3)')+'W'
;         IF xtickv(uu) EQ   0 AND uu mod TICKNAME_XINTERVAL EQ 0 OR  xtickv(uu) EQ 0 THEN tmp_str=string(180,format='(I3)')+''
         IF xtickv(uu) GT 180 AND uu mod TICKNAME_XINTERVAL EQ 0 THEN tmp_str=string(360-xtickv(uu),format='(I3)')+'W'
         IF xtickv(uu) LT 180 AND uu mod TICKNAME_XINTERVAL EQ 0 THEN tmp_str=string(xtickv(uu),format='(I3)')+'E'
         IF xtickv(uu) EQ 180 AND uu mod TICKNAME_XINTERVAL EQ 0 OR  xtickv(uu) EQ 0 THEN tmp_str=string(xtickv(uu),format='(I3)')+''
;       IF xtickv(uu) GT 180 THEN tmp_str=string(360-xtickv(uu),format='(I3)')+'W'
;       IF xtickv(uu) LT 180 THEN tmp_str=string(xtickv(uu),format='(I3)')+'E'
;       IF xtickv(uu) EQ 180 OR  xtickv(uu) EQ 0 THEN tmp_str=string(xtickv(uu),format='(I3)')
    Xtickname=[XTICKNAME,tmp_str]
ENDFOR

YTICKNAME=''
FOR uu=0,n_elements(ytickv)-1 DO BEGIN
    tmp_str= ' '
    IF ytickv(uu) GT 0 AND uu mod TICKNAME_YINTERVAL EQ 0 THEN tmp_str=string(ytickv(uu),format='(I3)')+'N   '
    IF ytickv(uu) LT 0 AND uu mod TICKNAME_YINTERVAL EQ 0 THEN tmp_str=string(fix(-ytickv(uu)))+'S   '
    IF ytickv(uu) EQ 0 AND uu mod TICKNAME_YINTERVAL EQ 0 THEN tmp_str=string(fix(ytickv(uu)))+'   '
;     IF ytickv(uu) GT 0 THEN tmp_str=string(ytickv(uu),format='(I3)')+'!Eo!NN  '
;     IF ytickv(uu) LT 0 THEN tmp_str=string(fix(-ytickv(uu)))+'!Eo!NS  '
;     IF ytickv(uu) EQ 0 THEN tmp_str=string(fix(ytickv(uu)))+'!Eo!N  '
     YTICKNAME=[YTICKNAME,tmp_str]
ENDFOR
XTICKNAME=XTICKNAME[1:n_elements(xtickv)]
YTICKNAME=YTICKNAME[1:n_elements(ytickv)]

END
