;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		Begin auto_string
;
;  Return a string containing the value of a number 
;  with the specified number of decimal
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function auto_string,number,num_decimal_places,nb_digit=nb_digit
;

ss=string(format=auto_format(number,num_decimal_places),number)

if keyword_set(nb_digit) then begin
    for k=0,n_elements(ss)-1 do begin
        str = str_sep(strcompress(strtrim(ss(k),2)),'.')
        nn = n_elements(str)
        ll = strlen(str(0))

        if nb_digit le ll then $
          str(0) = strmid(str(0),ll-nb_digit) $
        else $
          str(0) = peyl_concatstr(replicate('0',nb_digit-ll)) + str(0) 
        if nn gt 1 then $
          ss(k) = peyl_concatstr([str(0),str(1)],separator='.') $
        else $
          ss(k) = str(0)
    endfor
endif 
;
return,ss
end
;-
