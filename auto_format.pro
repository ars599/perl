;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		Begin auto_format
;
;  Return the IDL format for a given number and 
;  the number of decimal places
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function auto_format,number,num_decimal_places
;
abs_num=abs(number)
;--- if we deal with an array of number we take the largest....
abs_num = max([abs_num])
;
; leave room for the decimal point and the negative sign, if any
if min([number]) lt 0. then num_sign_chars=1 else num_sign_chars=0
;
; IDL's definition of the type_code
;0          Undefined
;1          Byte
;2          Integer
;3          Longword integer
;4          Floating-point
;5          Double-precision floating
;6          Complex floating
;7          String
;8          Structure
;
; Determine what we're dealing with
type_code=(size(number))((size(number))(0)+1) 
;
if ((type_code eq 4) or (type_code eq 5)) then begin
;number is floating point
;pre-assign the format_type just in case number falls through if statements
    format_type='f'
; number is a float, more or less
    if abs_num ge 1.e6 then begin
        num_prefix_chars=1
        num_sci_not_chars=4
        format_type='g'
    endif else if ((abs_num lt 1.e6) and (abs_num ge 1.)) then begin
        num_prefix_chars=fix(ceil(alog10(round(abs_num))))+1
        num_sci_not_chars=0
        format_type='f'
    endif else if ((abs_num lt 1.) and (abs_num ge 1.e-3)) then begin
; unfortunately, it seems impossible to get rid of leading zeroes before formatting
        num_prefix_chars=1
        num_sci_not_chars=0
        format_type='f'
    endif else if (abs_num eq 0.) then begin
        num_prefix_chars=1
        num_sci_not_chars=0
        format_type='f'
    endif else begin 
        num_prefix_chars=1
        num_sci_not_chars=4
        format_type='g'
    endelse
;
    if format_type eq 'e' then $
        num_postfix_chars=num_decimal_places $
    else if format_type eq 'f' then $
        num_postfix_chars=num_decimal_places $
    else if format_type eq 'g' then $
        num_postfix_chars=num_decimal_places
;
    num_total_chars=num_sign_chars+ $
                num_prefix_chars+ $
                1+ $
                num_decimal_places+ $
                num_sci_not_chars
;
    if num_total_chars lt 10 then $
        format_string=string(format='("(",A1,I1,".",I1,")")', $
                                format_type, $
                                num_total_chars, $
                                num_postfix_chars) $
    else $
        format_string=string(format='("(",A1,I2,".",I1,")")', $
                                format_type, $
                                num_total_chars, $
                                num_postfix_chars)
;
endif else if ((type_code eq 2) or (type_code eq 3)) then begin
;number is an integer
    format_type='i'
    if abs_num ne 0 then $
	num_digits=fix(alog10(abs_num))+1 $
    else $
	num_digits=1 
	
    num_total_chars=num_digits+num_sign_chars
;
    if num_total_chars lt 10 then $
        format_string=string(format='("(",A1,I1,")")', $
                                format_type, $
                                num_total_chars) $
    else $
        format_string=string(format='("(",A1,I2,")")', $
                                format_type, $
                                num_total_chars)
;
endif else begin; end if number is integer
;number is currently unformattable
   print,'unformattable number'
endelse;
;
;print,'format_string = ',format_string
;print,format=format_string,number
;
return,format_string
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End auto_format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-


