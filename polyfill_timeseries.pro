PRO polyfill_timeseries,time,data,ii,nn,newdata,newtime
;
; Created by Francois delage 16/08/2013
;
; polyfill_timeseries is a function which add zeros on boundaries of
; negative or positive values in a time serie. This help to draw
; shading with polyfill with no wrong shading next to the zeros line.
;
;-- INPUT
; time : time of selected values
; data : data of selected values
; ii   : pointer of selected values
; nn   : number of selected values
;
;-- OUTPUT
; newtime : time input for polyfill
; newdate : data input for polyfill
;
;-- CALLING SEQUENCE
; EX : we want to shade negative values
; > plot,[time[0],time[-1]],[ymin,ymax],POS=pos,/NODATA,/NORMAL
; > ii=where(array lt 0.,nn )
; > polyfill_timeseries,time[ii],data[ii],ii,nn,newdata,newtime
; > polyfill,[newtime],[newdata],/data,color=cgcolor('blu7')
;
;---

;-- add zero at the beginning
newdata=[0.,data]
newtime=[time[0],time]
ii=[0,ii]
nn=nn+1

;-- loop on selected values
for np = 1,nn-2 do begin
    
    ;print,ii[np],np,nn,(ii[np+1]),(ii[np]+1)
    ;if np eq 27 then stop
    ;-- add zeros at boundaries
    if (ii[np+1]) ne (ii[np]+1) then begin
        ;print,ii[np+1],ii[np]+1,nn
        newdata=[newdata[0:np],0.,0.,newdata[np+1:-1]]
        newtime=[newtime[0:np],newtime[np]+0.0001,newtime[np+1]-0.0001,newtime[np+1:-1]]
        ii=[ii[0:np],ii[np]+1,ii[np+1]-1,ii[np+1:-1]]
        ;--
        nn=nn+2
        np=np+2
    endif
    
endfor

;-- add zero at the end
newdata=[newdata,0.]
newtime=[newtime,time[-1]]

END
