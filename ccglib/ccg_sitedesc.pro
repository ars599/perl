;+
; NAME:
;        CCG_SITEDESC
;
; PURPOSE:
;        Returns an IDL structure containing a brief site 
;        and program description for the specified site(s).
;        See OUTPUTS for a complete description.
;
; CATEGORY:
;        CCG.
;
; CALLING SEQUENCE:
;        CCG_SITEDESC,site='brw',res
;        CCG_SITEDESC,site=['brw','lef','nwr'],res
;        CCG_SITEDESC,site=['brw_00','brw_01','brw_04'],res
;        CCG_SITEDESC,site=sitedesc,data
;
; INPUTS:
;        site:        May be a vector or a string constant.
;
; OPTIONAL INPUT PARAMETERS:
;        None.
;
; OUTPUTS:
;        desc:           This array is a structure:
;
;                       desc().err           -> error status  
;                                                1 if an error is detected
;                                                0 if no error is detected
;                       desc().str           -> "site" string passed 
;                       desc().site_code     -> site code 
;                       desc().site_name     -> site name
;                       desc().site_type     -> type of sampling site
;                                                e.g., land, aircraft, tower, shipboard
;                       desc().sample        -> sampling strategy
;                                                e.g., flask, in situ
;                       desc().sample_code   -> sampling strategy code
;                       desc().lab_name      -> name of measurement laboratory
;                       desc().lab_code      -> laboratory identification code
;                                                e.g., 00, 01, 23
;                       desc().lab_acronym   -> laboratory acronym
;                       desc().agency        -> cooperating agency
;                       desc().lat           -> site position degree latitude
;                       desc().sinlat        -> site position sine of latitude
;                       desc().long          -> site position degree longitude
;                       desc().alt           -> site position altitude (masl)
;                       desc().lst2utc       -> hour conversion from LST to UTC
;
;        NOTE:          The returned structure name is determined by user.
;
;                Type 'HELP, <structure name>, /str' at IDL prompt for
;                a description of the structure.
;
; COMMON BLOCKS:
;        None.
;
; SIDE EFFECTS:
;        None.
;
; RESTRICTIONS:
;        None.
;
; PROCEDURE:
;
;        Example:
;                IDL> CCG_SITEDESC,site='brw',arr
;                IDL> PRINT,arr.lat,arr.sinlat
;                IDL> 71.3200     0.947322
;
;                IDL> CCG_SITEDESC,site=['asc','ryo_19'],r
;                IDL> PRINT,r.agency
;                IDL> DOD/U.S.A.F. and Pan American World Airways
;                IDL> Japan Meteorological Agency (JMA)
;
;                IDL> CCG_SITEDESC,site=sitevector,result
;                        where sitevector is a string vector containing 
;                        site codes with or without laboratory extensions.
;
; MODIFICATION HISTORY:
;        Written, KAM, April 1997.
;        Modified, KAM, May 2000.
;-
;
PRO        CCG_SITEDESC,$
        site=site,$
        info

IF NOT KEYWORD_SET(site) THEN CCG_FATALERR, $
        "'site' keyword must be specified. Exiting ..."

DEFAULT=(-999.999)
UNK='Unknown'
ns=N_ELEMENTS(site)
IF ns EQ 1 THEN s=[site] ELSE s=site

info=REPLICATE({        ccg_sitedesc,$
                        err:                 1,$
                        str:                 UNK,$
                        site_code:           UNK,$
                        site_name:           UNK,$
                        site_type:           UNK,$
                        sample:              UNK,$
			sample_code:         UNK,$
                        platform:            UNK,$
                        platform_code:       UNK,$
                        lab_name:            UNK,$
                        lab_code:            UNK,$
                        lab_acronym:         UNK,$
                        agency:              UNK,$
                        lat:                 DEFAULT,$
                        sinlat:              DEFAULT,$
                        long:                DEFAULT,$
                        alt:                 DEFAULT,$
                        lst2utc:             DEFAULT},$
                        ns)

sdir='/home/gcarb/data/dei/ext/'

descfile=sdir+'work/cadip.desc'
CCG_SREAD,file=descfile,/nomessages,desc

labfile=sdir+'work/cadip.labs'
CCG_SREAD,file=labfile,/nomessages,lab

namefile=sdir+'work/cadip.filenames'
CCG_SREAD,file=namefile,/nomessages,names

FOR i=0,ns-1 DO BEGIN
        f12='???'
        f3='??'
        f4='?'
        f5='?'
        ;
        ;Parse passed site name into fields.
        ;
        j=STRPOS(s[i],'_')

        IF j[0] NE -1 THEN BEGIN
                f12=STRUPCASE(STRMID(s(i),0,j[0])) 
                z=STRMID(s(i),j[0]+1,100)
		IF STRLEN(z) GE 2 THEN f3=STRMID(z,0,2)
		IF STRLEN(z) GE 3 THEN f4=STRMID(z,2,1)
		IF STRLEN(z) GE 4 THEN f5=STRMID(z,3,1)
	ENDIF ELSE BEGIN
                f12=STRUPCASE(s(i))
        ENDELSE
        ;
        ;get site description
        ;
        k=WHERE(desc EQ f12)
        IF k(0) EQ -1 THEN $
                CCG_FATALERR, s(i)+' not found in '+descfile+'.  Exiting ...'

        READS,desc(k(0)+4),lat,long,alt,lst2utc
        info(i).str=s(i)
        info(i).site_code=f12
        info(i).site_type=desc(k(0)+1)
        info(i).site_name=desc(k(0)+2)
        info(i).agency=desc(k(0)+3)
        info(i).lat=lat
        info(i).sinlat=SIN(lat*!PI/180.)
        info(i).long=long
        info(i).alt=alt
        info(i).lst2utc=lst2utc
        ;
        ;get lab description
        ;
        IF f3 NE '??' THEN BEGIN
		k=WHERE(STRPOS(lab,f3) NE -1)
		IF k(0) EQ -1 THEN $
			CCG_FATALERR,f3+' not found in '+labfile+'.  Exiting ...'
		info(i).lab_name=lab(k(0)+1)
		info[i].lab_code=f3
	ENDIF
        ;
        ;get sampling strategy
        ;
        IF f4 NE '?' THEN BEGIN
		k=WHERE(STRPOS(names,'_??'+f4,0) NE -1)
		IF k(0) EQ -1 THEN $
			CCG_FATALERR,$
			 'Sampling code '+f4+' not found in '+namefile+'.  Exiting ...'
		info(i).sample=STRTRIM(STRMID(names(k(0)),5,200),2)
		info(i).sample_code=f4
	ENDIF
        ;
	;get platform description
        ;
	IF f5 NE '?' THEN BEGIN
		k=WHERE(STRPOS(names,'_???'+f5,0) NE -1)
		IF k(0) EQ -1 THEN $
			CCG_FATALERR,$
			 'Platform code '+f5+' not found in '+namefile+'.  Exiting ...'
		info(i).platform=STRTRIM(STRMID(names(k(0)),6,200),2)
		info(i).platform_code=f5
	ENDIF
        ;
        ;get lab acronym
        ;
        z1=CCG_STRRPOS(info(i).lab_name,'(')
        z2=CCG_STRRPOS(info(i).lab_name,')')
        info(i).lab_acronym=STRMID(info(i).lab_name,z1+1,z2-z1-1)
        info(i).err=0
ENDFOR
END
