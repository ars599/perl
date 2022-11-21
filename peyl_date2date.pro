;+
;========================================================================
; NAME:
;       PEYL_DATE2DATE
;
; PURPOSE:  change a give date array from one format to another format
;           accepted format are:
;               - strdate : string type of format = yy:mo:dy:hr:mn
;               - date : 1 to 5 integer for yy, mo, dy, hr, mn
;               - decdate : decimal date as computed by ccg_date2dec.
;               - juldate : standard idl julian date
;
; CALLING SEQUENCE:
;       
;
; INPUTS:
;     
;
; OPTIONAL INPUT PARAMETERS:
; 
;
; OUTPUTS:
;
;       
; RESTRICTIONS:
; 
;
;========================================================================
FUNCTION PEYL_DATE2DATE, yr, mo, dy, hr, mn, $
                         date=date,$
                         juldate=juldate,$
                         decdate=decdate,$
                         strdate = strdate,$
                         sep=sep, $
                         nb_field=nb_field,$
                         no_leap=no_leap,$
                         type_in=type_in,$
                         type_out=type_out

if n_elements(sep) eq 0 then sep=':'
if n_elements(type_out) eq 0 then type_out='strdate'
if n_elements(no_leap) eq 0 then no_leap=0
np = n_params()

;-------------------------------------------------------------
;   Define the type of date passed
;-------------------------------------------------------------

if n_elements(type_in) gt 0 then begin
    case type_in of 
        'strdate': if n_elements(strdate) eq 0 then message,'Error type in'
        'date': if (n_elements(date) eq 0 or np eq 0) then message,'Error type in'
        'decdate': if n_elements(decdate) eq 0 then message,'Error type in'
        'juldate': if n_elements(juldate) eq 0 then message,'Error type in'
        else: message,'Error date2date: type in not recognized '
  endcase 

endif else begin
    if n_elements(strdate) gt 0 then type_in = 'strdate'
    if n_elements(juldate) gt 0 then type_in = 'juldate'
    if n_elements(decdate) gt 0 then type_in = 'decdate'
    if n_elements(date) gt 0 then begin
        type_in = 'date'
        ntag = n_elements(date(*,0))
        if ntag ge 1 then yr = reform(date(0,*))
        if ntag ge 2 then mo = reform(date(1,*))
        if ntag ge 3 then dy = reform(date(2,*))
        if ntag ge 4 then hr = reform(date(3,*))
        if ntag ge 5 then mn = reform(date(4,*))
    endif
    if np gt 0 then begin
        type_in = 'date'
        nt = n_elements(yr)
        ntag = np
        date = intarr(ntag,nt)
        if ntag ge 1 then date(0,*) = yr
        if ntag ge 2 then date(1,*) = mo
        if ntag ge 3 then date(2,*) = dy
        if ntag ge 4 then date(3,*) = hr
        if ntag ge 5 then date(4,*) = mn
    endif

    if n_elements(type_in) eq 0 then message,'Problem : no input date !'
endelse

;-------------------------------------------------------------
;  Compute for each case all other type of date
;-------------------------------------------------------------

;---- String date furnished
if type_in eq 'strdate' then begin
    nt = n_elements(strdate)
    str = strsplit(strdate(0),sep,/extract)
    ntag = n_elements(str)
    date = intarr(ntag,nt)
    for t=0L,nt-1 do date(*,t) = strsplit(strdate(t),sep,/extract)
    if ntag ge 1 then yr = reform(date(0,*))
    if ntag ge 2 then mo = reform(date(1,*))
    if ntag ge 3 then dy = reform(date(2,*))
    if ntag ge 4 then hr = reform(date(3,*))
    if ntag ge 5 then mn = reform(date(4,*))
    CCG_DATE2DEC,yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=decdate
    juldate = julday(mo,dy,yr,hr,mn)
endif

;---- Standard date furnished
if type_in eq 'date' then begin
    nt = n_elements(date(0,*))
    ntag = n_elements(date(*,0))
    if ntag ge 1 then yr = reform(date(0,*))
    if ntag ge 2 then mo = reform(date(1,*))
    if ntag ge 3 then dy = reform(date(2,*))
    if ntag ge 4 then hr = reform(date(3,*))
    if ntag ge 5 then mn = reform(date(4,*))
    CCG_DATE2DEC,yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=decdate
    juldate = julday(mo,dy,yr,hr,mn)
    strdate = strarr(nt)
    if n_elements(nb_field) eq 0 then nf = ntag else nf = nb_field < ntag
    for t=0L,nt-1 do strdate(t) = $
      strjoin((strtrim(auto_string(fix(date(*,t)),0),2))(0:nf-1),sep)
endif

;---- Decimal date furnished
if type_in eq 'decdate' then begin
    ss = size(decdate)
    if ss(ss(0)+1) ne 5 then decdate = double(decdate)
    nt = n_elements(decdate(*))
    ntag = 5
    CCG_DEC2DATE,reform(decdate),yr,mo,dy,hr,mn
    date = intarr(ntag,nt)
    date(0,*) = yr
    date(1,*) = mo
    date(2,*) = dy
    date(3,*) = hr
    date(4,*) = mn
    juldate = julday(mo,dy,yr,hr,mn)
    strdate = strarr(nt)
    if n_elements(nb_field) eq 0 then nf = 3 else nf = nb_field < ntag
    for t=0L,nt-1 do strdate(t) = $
      strjoin((strtrim(auto_string(fix(date(*,t)),0),2))(0:nf-1),sep)
endif

;---- Julian date furnished
if type_in eq 'juldate' then begin
    nt = n_elements(juldate(*))
    ntag = 5
    caldat,juldate,mo,dy,yr,hr,mn
    CCG_DATE2DEC,yr=yr,mo=mo,dy=dy,hr=hr,mn=mn,dec=decdate
    date = intarr(ntag,nt)
    date(0,*) = yr
    date(1,*) = mo
    date(2,*) = dy
    date(3,*) = hr
    date(4,*) = mn
    strdate = strarr(nt)
    if n_elements(nb_field) eq 0 then nf = 3 else nf = nb_field < ntag
    for t=0L,nt-1 do strdate(t) = $
      strjoin((strtrim(auto_string(fix(date(*,t)),0),2))(0:nf-1),sep)
endif


;-------------------------------------------------------------
;    Get read of 29 Feb if No_leap is asked!
;-------------------------------------------------------------

if no_leap then begin
    foo = where(date(1,*) eq 2 and date(2,*) eq 29,complement=fooinv,ncomplement=ccinv,cc)
    if cc gt 0 then begin
        if ccinv eq 0 then message,'Problem : only 29 Fev with no_leap!'
        print,'Eliminate 29 of February!'
        date = date(*,fooinv)
        juldate = juldate(fooinv)
        decdate = decdate(fooinv)
        yr = yr(fooinv)
        mo = mo(fooinv)
        dy = dy(fooinv)
        hr = hr(fooinv)
        mn = mn(fooinv)      
    endif
endif


;-------------------------------------------------------------
;    Define output
;-------------------------------------------------------------

if type_out eq 'strdate' then return,strdate
if type_out eq 'date' then return,date
if type_out eq 'decdate' then return,decdate
if type_out eq 'juldate' then return,juldate

print,'Type out not recognized!!'
print,'   -> choose betw : strdate, date, decdate, juldate '
stop

END
;-
