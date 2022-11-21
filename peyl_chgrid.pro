;+
;========================================================================
; NAME:
;       PEYL_CHGRID
;
; PURPOSE: change a 2D grided variable from one grid to another
;       
;
; CALLING SEQUENCE:
;       PEYL_CHGRID,data_in_,grid_in,grid_out,$
;                     itypedata=itypedata, valmask=valmask, fraction=fr,$
;                     lmdz_extra_lon_out = lmdz_extra_lon_out,$
;                     NS_inverse_in  = NS_inverse_in,$
;                     NS_inverse_out = NS_inverse_out
;
; INPUTS:
;        grid_in (or grid_out): structure containing
;                    grid = {lon:fltarr(nlon), dlon:fltarr(nlon),
;                            lat:fltarr(nlat), dlat:fltarr(nlat)}
;                 where lon/lat are center of grid boxes
;                       dlon/dlat are wide of grid boxes
;
;
; OPTIONAL INPUT PARAMETERS:
; x
;
; OUTPUTS:
;
;       
; RESTRICTIONS:
; 
;
;========================================================================
FUNCTION PEYL_CHGRID,data_in_,grid_in,grid_out,$
                     itypedata=itypedata, valmask=valmask, fraction=fr,$
                     lmdz_extra_lon_out = lmdz_extra_lon_out,$
                     NS_inverse_in  = NS_inverse_in,$
                     NS_inverse_out = NS_inverse_out,$
                     info = info

if not keyword_set(valmask) then valmask = 1.e32
if not keyword_set(itypedata) then itypedata = 1
if not keyword_set(lmdz_extra_lon_out) then lmdz_extra_lon_out = 0
if not keyword_set(NS_inverse_in) then NS_inverse_in = 0
if not keyword_set(NS_inverse_out) then NS_inverse_out = 0
if n_elements(fraction) eq 0 then compute_fraction=1 else compute_fraction=0
if not keyword_set(info) then info=0

im1 = n_elements(grid_in.lon)
jm1 = n_elements(grid_in.lat)
im2 = n_elements(grid_out.lon)
jm2 = n_elements(grid_out.lat)
ii = where(grid_in.lon gt 180., cc)
if cc gt 0 then begin
    print,'Reajustement des lon_in > 180 pour i= ', ii
    grid_in.lon(ii) = grid_in.lon(ii) - 360.
endif
ii = where(grid_out.lon gt 180., cc)
if cc gt 0 then begin
    print,'Reajustement des lon_out > 180 pour i= ', ii
    grid_out.lon(ii) = grid_out.lon(ii) - 360.
endif

if NS_inverse_in then data_in = data_in_(*,reverse(indgen(jm1)),*,*,*) else data_in = data_in_

ss = size(data_in)
if ss(0) le 1 or ss(0) gt 4 then message,'data_in doit etre de dimension 2 ou 3 ou 4...'
if ss(1) ne im1 then message,'data_in : nlon different de grille..'
if ss(2) ne jm1 then message,'data_in : nlat different de grille..'
if ss(0) ge 3 then nlev = ss(3) else nlev = 1
if ss(0) ge 4 then ntime = ss(4) else ntime = 1


pi=!pi
ae=6.371229e6
d2r=!pi/180.


;---------------------------------------------------------------
;            Calcul des fractions imbriques..
;---------------------------------------------------------------

if compute_fraction eq 0 then begin
    if (n_elements(fr.fx(*,0)) ne (im2+1) or n_elements(fr.fx(0,*)) ne (im1) or $
        n_elements(fr.fy(*,0)) ne (jm2) or n_elements(fr.fy(0,*)) ne (jm1) ) then $
      message,'Problem with the the array fr given to the routine...'
    goto,suite
endif

print,'Compute the fraction of G1 into G2....' 

fr = {fx:fltarr(im2+1,im1),$
      fy:fltarr(jm2,jm1),$
      dyy:fltarr(jm2,jm1),$
      lcx:intarr(im2+1,im1),$
      lcy:intarr(jm2,jm1),$
      ninci:intarr(im2),$
      nincj:intarr(jm2)}

;               !Zonal calculations
;               ===================

for i1=0,im1-1 do begin    
    for i2=0,im2-1 do begin
        e_lon1=grid_in.lon(i1)+grid_in.dlon(i1)/2.
        e_lon2=grid_out.lon(i2)+grid_out.dlon(i2)/2.
        w_lon1=e_lon1-grid_in.dlon(i1)
        w_lon2=e_lon2-grid_out.dlon(i2)
        
;---------------------------------------
; traitement de la cyclicite...(pour 180)
; les boites a cheval sur la date line sont
; repositionnees.. Ceci nescessite l'utilisation
; du bord EST des boites comme base..

;---- Old formulation Ramonet
if 0 then begin
        IF ((e_lon1 GE -180.) AND (w_lon1 LT -180.)) THEN BEGIN
            IF (e_lon2 GT 0.) THEN BEGIN
                e_lon2=e_lon2-360.
                w_lon2=w_lon2-360.
            ENDIF 
        ENDIF ELSE IF (e_lon1 GT 0.) THEN BEGIN
            IF ((e_lon2 GT -180.) AND (w_lon2 LE -180.)) THEN BEGIN
                e_lon2=e_lon2+360.
                w_lon2=w_lon2+360.
            ENDIF
        ENDIF

;---- new formulation (peylin Sept 2009)
endif else begin
    IF (e_lon1 LT 0) THEN BEGIN
        IF (w_lon2 GT 0.) THEN BEGIN
            e_lon2=e_lon2-360.
            w_lon2=w_lon2-360.
        ENDIF 
    ENDIF ELSE IF (w_lon1 GT 0) THEN BEGIN
        IF (e_lon2 LT 0.) THEN BEGIN
            e_lon2=e_lon2+360.
            w_lon2=w_lon2+360.
        ENDIF 
    ENDIF
endelse 

;if i2 eq 0 then begin
;    print,w_lon1,e_lon1,w_lon2,e_lon2
;endif

;-----
; calcul

        IF (e_lon2 GE e_lon1) THEN BEGIN
            IF (w_lon2 LT e_lon1) THEN BEGIN
                dxlon = min([e_lon1-w_lon2,grid_in.dlon(i1)])
                fr.fx(i2,fr.ninci(i2))  = dxlon/grid_in.dlon(i1)
                fr.lcx(i2,fr.ninci(i2)) = i1
                fr.ninci(i2)            = fr.ninci(i2)+1
            ENDIF
        ENDIF  else if (e_lon2 GT w_lon1) then BEGIN
            dxlon = min([e_lon2-w_lon1,grid_out.dlon(i2)])
            fr.fx(i2,fr.ninci(i2))  = dxlon/grid_in.dlon(i1)
            fr.lcx(i2,fr.ninci(i2)) = i1
            fr.ninci(i2)            = fr.ninci(i2)+1
        endif 


    endfor 
endfor

;-----
;               !Meridional calculations
;               ========================

for j1=0,jm1-1 do begin
    for j2=0,jm2-1 do begin

        n_lat1 = grid_in.lat(j1) + grid_in.dlat(j1)/2.
        n_lat2 = grid_out.lat(j2) + grid_out.dlat(j2)/2.
        s_lat1 = n_lat1-grid_in.dlat(j1)
        s_lat2 = n_lat2-grid_out.dlat(j2)

        dd1 = grid_in.dlat(j1)/10.
        dd2 = grid_out.dlat(j2)/10.
        IF ((s_lat1 LT (-90.-dd1)) OR (s_lat2 lt (-90.-dd2))) THEN BEGIN
            print,'Erreur de lat.. ',s_lat1,s_lat2,j1,j2
            stop
        ENDIF
        IF ((n_lat1 GT (90.+dd1)) OR (n_lat2 gt (90.+dd2))) THEN BEGIN
            print,'Erreur de lat.. ',n_lat1,n_lat2,j1,j2
            stop
        ENDIF

        if (n_lat2 GE n_lat1) THEN BEGIN
            IF (s_lat2 LT n_lat1) THEN BEGIN
                dxlat = min([n_lat1-s_lat2,grid_in.dlat(j1)])
                fr.fy(j2,fr.nincj(j2))  = dxlat/grid_in.dlat(j1)
                fr.lcy(j2,fr.nincj(j2)) = j1
                fr.dyy(j2,fr.nincj(j2)) = 2.*(ae*ae)*sin(0.5*dxlat*d2r)*cos((n_lat1-(dxlat/2.))*d2r)
                fr.nincj(j2)            = fr.nincj(j2)+1
            ENDIF
        endif else if (n_lat2 GT s_lat1) then begin
            dxlat = min([n_lat2-s_lat1,grid_out.dlat(j2)])
            fr.fy(j2,fr.nincj(j2))  = dxlat/grid_in.dlat(j1)
            fr.lcy(j2,fr.nincj(j2)) = j1
            fr.dyy(j2,fr.nincj(j2)) = 2.*(ae*ae)*sin(0.5*dxlat*d2r)*cos((n_lat2-(dxlat/2.))*d2r)
            fr.nincj(j2)            = fr.nincj(j2)+1
        endif

    endfor 
endfor 

;----- Define Fraction total pour chaque pt de grille finale
ninc_max =  max(fr.nincj) * max(fr.ninci)
fr = create_struct(fr,'frac_surf',fltarr(im2,jm2,ninc_max))

for j=0,jm2-1 do begin
    for i=0,im2-1 do begin
        temp=0.
        for jp=0,fr.nincj(j)-1 do for ip=0,fr.ninci(i)-1 do $
          temp = temp + fr.fx(i,ip)*grid_in.dlon(fr.lcx(i,ip))*d2r*fr.dyy(j,jp)
        for jp=0,fr.nincj(j)-1 do for ip=0,fr.ninci(i)-1 do begin
            k = jp*fr.ninci(i) + ip            
            fr.frac_surf(i,j,k) = (fr.fx(i,ip)*grid_in.dlon(fr.lcx(i,ip))*d2r*fr.dyy(j,jp))/temp
        endfor
    endfor
endfor

suite:

;---------------------------------------------------------------
;            Calcul finaux du changement de grille
;---------------------------------------------------------------

type = ss(ss(0)+1)
if (type ge 2 and type le 4) then begin
    dd=float(valmask) 
    if type le 3 then print,'Warning Integer data converted to Float!'
endif else if type eq 5 then dd = double(valmask) $
else message,'probleme de type pour data_in (type non traite!)'	

if lmdz_extra_lon_out then data_out = replicate(dd,im2+1,jm2,nlev,ntime) else $
  data_out = replicate(dd,im2,jm2,nlev,ntime)

dxyp3 = fltarr(im2,jm2)

; calculate the ponderate values in G2 boxes
; Rem : the value of the fraction of the grid G1 
; that is included in G2 is given by : fx * dlon1 * dyy
; (cf the calcul of those values in fraction).
; REM : lon and lat are in degre..
data_out(*,*,*,*) = 0.

for t=0,ntime-1 do begin
if ntime gt 1 and (t mod 20 eq 0) then print,'time = ',t
for k=0,nlev-1 do begin
if nlev gt 1 and (k mod 20 eq 0) then print,'nlev = ',k

; initialisation du tableaux des surfaces pour chaque niveau...
    dxyp3(*,*) = 0.

;cccc Interpolation standard : grandeur par unite de surface...
    if (itypedata eq 1) then begin
    	for j=0,jm2-1 do begin
            for i=0,im2-1 do begin
                for jp=0,fr.nincj(j)-1 do begin
                    if fr.ninci(i) gt 0 then begin
                        ii = indgen(fr.ninci(i))
                        jj = where(data_in(fr.lcx(i,ii),fr.lcy(j,jp),k,t) NE valmask,cc)
                        if cc gt 0 then begin
                            temp = fr.fx(i,jj)*grid_in.dlon(fr.lcx(i,jj))*d2r*fr.dyy(j,jp) 
                            data_out(i,j,k,t) = data_out(i,j,k,t) + $
                              total( temp*data_in(fr.lcx(i,jj),fr.lcy(j,jp),k,t) )
                            dxyp3(i,j) = dxyp3(i,j) + total(temp)
                        endif 
                    endif
                endfor
                if dxyp3(i,j) gt 0 then data_out(i,j,k,t) = data_out(i,j,k,t)/dxyp3(i,j) else $
                  data_out(i,j,k,t) = valmask
            endfor
        endfor

;ccccc Interpolation pour une grandeur totale NON par unite de surface

    endif else if (itypedata eq 2) then begin
    	for j=0,jm2-1 do begin
            for i=0,im2-1 do begin
                for jp=0,fr.nincj(j)-1 do begin
                    if fr.ninci(i) gt 0 then begin
                        ii = indgen(fr.ninci(i))
                        jj = where(data_in(fr.lcx(i,ii),fr.lcy(j,jp),k,t) NE valmask,cc)
                        if cc gt 0 then begin
                            data_out(i,j,k,t) = data_out(i,j,k,t) + $
                              total( fr.fx(i,jj)*grid_in.dlon(fr.lcx(i,jj))*d2r $
                                     *data_in(fr.lcx(i,jj),fr.lcy(j,jp),k,t)$
                                     *fr.dyy(j,jp) $
                                     /grid_in.dxyp(fr.lcx(i,jj),fr.lcy(j,jp)) )
                            dxyp3(i,j) = dxyp3(i,j) + $
                              total( fr.fx(i,jj)*grid_in.dlon(fr.lcx(i,jj))*d2r*fr.dyy(j,jp) )
                        endif 
                    endif
                endfor
                if dxyp3(i,j) eq 0 then data_out(i,j,k,t) = valmask
            endfor
        endfor

;ccccc Interpolation sans prendre en compte la surface (moyenne simple)

    endif else if (itypedata eq 3) then begin
    	for j=0,jm2-1 do begin
            for i=0,im2-1 do begin
                for jp=0,fr.nincj(j)-1 do begin
                    if fr.ninci(i) gt 0 then begin
                        ii = indgen(fr.ninci(i))
                        jj = where(data_in(fr.lcx(i,ii),fr.lcy(j,jp),k,t) NE valmask,cc)
                        if cc gt 0 then begin
                            data_out(i,j,k,t) = data_out(i,j,k,t) + $
                              total( data_in(fr.lcx(i,jj),fr.lcy(j,jp),k,t) )
                            dxyp3(i,j) = dxyp3(i,j) + cc
                        endif
                    endif
                endfor
                if dxyp3(i,j) gt 0 then data_out(i,j,k,t) = data_out(i,j,k,t)/dxyp3(i,j) else $
                  data_out(i,j,k,t) = valmask
            endfor
        endfor

;ccccc Interpolation en prenant la valeur majoritaire compte tenu de
;la surface 
                            
    endif else if (itypedata eq 4) THEN BEGIN
        data_out(*,*,k,t) = valmask
        print,'Cas special: valeur des boites majoritaire selon surface...'
    	for j=0,jm2-1 do begin
            for i=0,im2-1 do begin
                nll = fr.nincj(j)*fr.ninci(i)
                if nll eq 0 then continue
                temp_val  = dblarr(nll)
                temp_surf = dblarr(nll)
                l = 0
                for jp=0,fr.nincj(j)-1 do begin
	            for ip=0,fr.ninci(i)-1 do begin
                        temp_val(l)  = data_in(fr.lcx(i,ip),fr.lcy(j,jp),k,t)
                        temp_surf(l) = fr.fx(i,ip)*grid_in.dlon(fr.lcx(i,ip))*d2r*fr.dyy(j,jp)
                        l = l + 1
                    endfor
                endfor
                ii = sort(temp_val)
                temp_val  = temp_val(ii)
                temp_surf = temp_surf(ii) 
                jj = where(temp_val NE valmask, njj)
                if njj eq 0 then continue                
                val_max  = temp_val(jj(0))
                surf_max = temp_surf(jj(0))
                val_cum  = temp_val(jj(0))
                surf_cum = temp_surf(jj(0))
                for l1=1,njj-1 do begin
                    l=jj(l1)
                    if temp_val(l) eq val_cum then begin
                        surf_cum = surf_cum + temp_surf(l)
                    endif else begin
                        if surf_cum ge surf_max then begin
                            surf_max = surf_cum 
                            val_max  = val_cum 
                        endif 
                        surf_cum = temp_surf(l)
                        val_cum  = temp_val(l)
                    endelse
                endfor
                if surf_cum ge surf_max then val_max = val_cum
                data_out(i,j,k,t) = val_max
            endfor 
        endfor 
        
;ccccc Interpolation en prenant la valeur de la boite majoritaire en surf 
        
    endif else if (itypedata eq 5) THEN BEGIN
        data_out(*,*,k,t) = valmask
        print,'!!! Attention cas special avec valeur de la derniere boite majoritaire sans sommation...'
    	for j=0,jm2-1 do begin
            for i=0,im2-1 do begin
                xx0 = 0.
                for jp=0,fr.nincj(j)-1 do begin
                    for ip=0,fr.ninci(i)-1 do begin
                        if data_in(fr.lcx(i,ip),fr.lcy(j,jp),k,t) NE valmask then begin
                            xx = fr.fx(i,ip)*grid_in.dlon(fr.lcx(i,ip))*d2r*fr.dyy(j,jp)  
                            if (xx  GE (xx0*0.999)) then begin 
                                data_out(i,j,k,t) = data_in(fr.lcx(i,ip),fr.lcy(j,jp),k,t)
                                xx0 = xx   
                            endif
	                endif 
	            endfor
                endfor 
            endfor
        endfor

;ccccc Interpolation bilineaire simple sans prendre surface boites

    endif else if (itypedata eq 6) then begin
    	for j=0,jm2-1 do begin
            for i=0,im2-1 do begin
                print,'cas itypedata 6 a traiter !!!'
                ;utiliser bilinear...
                stop
            endfor
        endfor

    endif else begin
        print,'BUG!!! Mauvaise valeur de itypedata..',itypedata
        STOP
    ENDELSE

endfor
endfor

if lmdz_extra_lon_out then data_out(im2,*,*,*) = data_out(im2-1,*,*,*)    
if NS_inverse_out then data_out = data_out(*,reverse(indgen(jm2)),*,*,*)

;---------------------------------------------------------------
;            Print des statistiques 
;---------------------------------------------------------------

if info eq 0 then goto,suite_stat

openw,uu,'chgrid_info.txt',/get_lun,width=200

printf,uu,'surface total grille in (m2) ',total(grid_in.dxyp)
printf,uu,'surface total grille out (m2) ',total(grid_out.dxyp)

printf,uu,'Min on grid in/out ',peyl_min(data_in(*,*,*,*),mask=valmask),peyl_min(data_out(0:im2-1,*,*,*),mask=valmask)
printf,uu,'Max on grid in/out ',peyl_max(data_in(*,*,*,*),mask=valmask),peyl_max(data_out(0:im2-1,*,*,*),mask=valmask)

tot1 = 0.
tot2 = 0.
for t=0,ntime-1 do begin
    for k=0,nlev-1 do begin
        tot1 = tot1 + peyl_total(data_in(*,*,k,t)*grid_in.dxyp(*,*),mask=valmask)
        tot2 = tot2 + peyl_total(data_out(0:im2-1,*,k,t)*grid_out.dxyp(*,*),mask=valmask)
    endfor
endfor
printf,uu,'Total on grid in/out',tot1,tot2

printf,uu
printf,uu,'======= Fraction en longitude ======='
printf,uu
for i2=0,im2-1 do begin
    if fr.ninci(i2) eq 0 then printf,uu,'i2=',i2,' lon2=',grid_out.lon(i2),' => pas boites..'
    for i=0,fr.ninci(i2)-1 do begin
        if i eq 0 then $
          printf,uu,'i2=',auto_string(i2,0),' lon2=',grid_out.lon(i2),' => ',fr.lcx(i2,i),':',grid_in.lon(fr.lcx(i2,i)),',',fr.fx(i2,i),'%' $
        else $
          printf,uu,'                    ',fr.lcx(i2,i),':',grid_in.lon(fr.lcx(i2,i)),',',fr.fx(i2,i),'%' 
    endfor
endfor

printf,uu
printf,uu,'======= Fraction en latitude ======='
printf,uu
for j2=0,jm2-1 do begin
    if fr.nincj(j2) eq 0 then printf,uu,'j2=',j2,' lat2=',grid_out.lat(j2),' => pas boites..'
    for j=0,fr.nincj(j2)-1 do begin
        if j eq 0 then $
          printf,uu,'j2=',auto_string(j2,0),' lat2=',grid_out.lat(j2),' => ',fr.lcy(j2,j),':',grid_in.lat(fr.lcy(j2,j)),',',fr.fy(j2,j),'%' $
        else $
          printf,uu,'                    ',fr.lcy(j2,j),':',grid_in.lat(fr.lcy(j2,j)),',',fr.fy(j2,j),'%' 
    endfor
endfor

if 1 then begin
printf,uu
printf,uu,'======= Fraction totale ======='
printf,uu
for j=0,jm2-1 do begin
    for i=0,im2-1 do begin
        for jp=0,fr.nincj(j)-1 do for ip=0,fr.ninci(i)-1 do begin
            k = jp*fr.ninci(i) + ip            
            printf,uu,grid_out.lon(i),grid_out.lat(j),' => ',grid_in.lon(fr.lcx(i,ip)),grid_in.lat(fr.lcy(j,jp)),fr.frac_surf(i,j,k) 
        endfor
    endfor
endfor
endif

suite_stat:

return,data_out
END
;-


