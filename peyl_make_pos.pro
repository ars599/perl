;+
;========================================================================
; NAME:
;       PEYL_MAKE_POS
;
; PURPOSE:
;       CREATES AN ARRAY FILLED WITH THE POSITIONS OF DIFFERENT PLOTS
;       TO PUT ON THE SAME PAGE, DEPENDING ON THIS NUMBER OF PLOTS PER PAGE
;
; CALLING SEQUENCE:
;       Nplots = xx
;       Result = PEYL_MAKE_POS(Nplots [,RIGHTBORD=rightbord,BOTBORD=botbord,$
;       BOTTOM=bottom,TOP=top,LEFT=left,RIGHT=right,BETWEEN=betw,$
;       ORDER=order, Ncol = Ncol] )
;       EXAMPLE :
;           Nplots = 7
;           pos = peyl_make_pos(Nplots,BOTBORD=0.1,BETWEEN=0.15)
;           for i=0,Nplots-1 DO 
;           plot,randomu(seed,10),randomu(seed,10),pos=pos[*,i],/NOERASE
;
; INPUTS:
;       Nplots, NUMBER OF PLOTS TO BE DRAWN PER PAGE
;
; OPTIONAL INPUT PARAMETERS:
;       RIGHTBORD: ALLOWS TO KEEP ROOM ON THE RIGHT, TO PUT A COLORBAR
;                  BY REDUCING THE XSIZE OF THE PLOTS
;                  (EXAMPLE: RIGHTBORD=0.9)
;       LEFTBORD: ALLOWS TO KEEP ROOM ON THE LEFT, TO PUT A COLORBAR
;                 BY REDUCING THE YSIZE OF THE PLOTS
;                 (EXAMPLE: LEFTBORD=0.1)
;       TOPBORD: ALLOWS TO KEEP ROOM ON THE TOP, TO PUT A TITLE
;                  BY REDUCING THE XSIZE OF THE PLOTS
;                  (EXAMPLE: TOPBORD=0.9)
;       BOTBORD: ALLOWS TO KEEP ROOM ON THE BOTTOM, TO PUT A COLORBAR
;                 BY REDUCING THE YSIZE OF THE PLOTS
;                 (EXAMPLE: BOTBORD=0.1)
;       BOTTOM: ALLOWS TO KEEP ROOM ON THE BOTTOM, TO PUT A LEGEND
;               BY REDUCING THE YSIZE OF THE PLOTS BUT STARTING FROM
;               THE SAME TOP Y POSITION AS WHEN NOT REDUCED
;               (DEFAULT: BOTTOM=0.15)
;       TOP: ALLOWS TO KEEP ROOM ON TOP, TO PUT A TITLE
;            BY REDUCING THE YSIZE OF THE PLOTS BUT FINISHING AT
;            THE SAME BOTTOM Y POSITION AS WHEN NOT REDUCED
;            (DEFAULT: TOP=0.01)
;       LEFTFIX: ALLOWS TO KEEP ROOM ON THE LEFT, TO PUT AN AXIS TITLE
;                BY REDUCING THE XSIZE OF THE PLOTS BUT FINISHING AT
;                THE SAME RIGHT X POSITION AS WHEN NOT REDUCED
;                (DEFAULT: LEFTFIX=0.15)
;       RIGHTFIX: ALLOWS TO KEEP ROOM ON THE RIGHT
;                 BY REDUCING THE XSIZE OF THE PLOTS BUT STARTING FROM
;                 THE SAME LEFT X POSITION AS WHEN NOT REDUCED
;                 (DEFAULT: RIGHTFIX=0.01)
;       BETWEEN: THE SPACING BETWEEN THE PLOTS
;                BY REDUCING THE XSIZE AND YSIZE OF THE PLOTS
;                (DEFAULT: BETWEEN=0.1)
;       /ORDER: TO ORDINATE THE PLOTS FROM TOP LEFT TO BOTTOM LEFT
;               AND THEN FROM LEFT TO RIGHT
;               (OTHERWISE,FROM TOP LEFT TO TOP RIGHT, AND
;               THEN FROM TOP TO BOTTOM)
;       Ncol: TO INDICATE THE NUMBER OF COLUMNS OF PLOTS
;             (DEFAULT: THE NUMBER OF COLUMNS IS CHOSEN AS NEAR AS POSSIBLE
;             FROM THE NUMBER OF LINES OF PLOTS)
;
; OUTPUTS:
;       RESULT=THE ARRAY OF POSITIONS, [PLOT COORDINATE INDEX (0 TO 3 = XMIN,
;       YMIN, XMAX, YMAX),PLOT INDEX]
;       
; RESTRICTIONS:
; 
;
;========================================================================

FUNCTION peyl_make_pos,Nplots,RIGHTBORD=rightbord,BOTBORD=botbord,$
                       LEFTBORD=leftbord,TOPBORD=topbord,$
                       BOTFIX=botfix,TOPFIX=topfix,$
                       LEFTFIX=leftfix,RIGHTFIX=rightfix,$
                       BETWEEN=betw,$
                       ORDER=order, Ncol = Ncol

IF n_elements(rightbord) eq 0  THEN rightbord=1.
IF n_elements(botbord)   eq 0  THEN botbord=0.
IF n_elements(leftbord)  eq 0  THEN leftbord=0.
IF n_elements(topbord)   eq 0  THEN topbord=1.
IF n_elements(botfix   ) eq 0  THEN botfix= 0.15
IF n_elements(topfix   ) eq 0  THEN topfix= 0.01
IF n_elements(leftfix  ) eq 0  THEN leftfix=0.15
IF n_elements(rightfix ) eq 0  THEN rightfix= 0.01
IF n_elements(betw     ) eq 0  THEN betw =0.1

IF n_elements(Ncol) eq 0 THEN BEGIN
    Nlin = FIX(SQRT(Nplots-1)) + 1
    Ncol = (Nplots-1)/Nlin + 1
ENDIF ELSE  Nlin = (nplots-1)/Ncol + 1

if (rightbord-leftbord) le 0. then begin
    print,'Error make_pos : rightbord - leftbord !!! should be >0 and <=1'
    stop
endif
if (topbord-botbord) le 0. then begin
    print,'Error make_pos : topbord - botbord !!! should be >0 and <=1'
    stop
endif

dx = (rightbord-leftbord) /(Ncol-betw/(1.+betw))
dy = (topbord-botbord)/(Nlin-betw/(1.+betw))

IF KEYWORD_SET(order) THEN BEGIN
    ilin = INDGEN(Nplots) MOD Nlin
    icol = INDGEN(Nplots)/Nlin
ENDIF ELSE BEGIN
    icol = INDGEN(Nplots) MOD Ncol
    ilin = INDGEN(Nplots)/Ncol
ENDELSE

pos = FLTARR(4,Nplots)
pos[0,*] = icol*dx  + leftbord        + dx/(1.+betw) * leftfix/(1.+leftfix+rightfix)
pos[1,*] = (Nlin-1-ilin)*dy + botbord + dy/(1.+betw) * botfix/(1.+botfix+topfix)
pos[2,*] = pos[0,*]                   + dx/(1.+betw) / (1.+leftfix+rightfix)
pos[3,*] = pos[1,*]                   + dy/(1.+betw) / (1.+botfix+topfix)


RETURN, pos
END
;-

