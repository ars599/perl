;+
; NAME:
;	CCG_RGBLOAD
;
; PURPOSE:
; 	Load an RGB color table that has
;	previously been saved using
;	CCG_RGBSAVE.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	CCG_RGBLOAD, file=filename
;	CCG_RGBLOAD, file='/users/ken/idl/lib/xcolor_table'
;
; INPUTS:
;	Filename:  file must be a color table that 
;		   was created using CCG_RGBSAVE.
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;	RGB color table.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Will write over currently defined 
;	IDL color table.
;
; RESTRICTIONS:
;	Color table file must have
;	been created using RGBSAVE.
;
; PROCEDURE:
;	CCG_RGBLOAD may be called from the IDL
;	command line or from an IDL procedure.
;	Example:
;		.
;		.
;		.
;		CCG_RGBLOAD, 	file='colortable'
;		SHADE_SURF,	grid
;		.
;		.
;		.
;		
; MODIFICATION HISTORY:
;	Written, KAM, April 1993.
;-
;
PRO 	fanch_RGBGET,	tag=tag,r,g,b
;
CASE tag OF
    'precip': $
      BEGIN
        r=bytarr(256) & r(*)=0 & b=r & g=r
        r(0:11)=[255,255,220,150,100,  0,  0,150,220,255,210,1]
        g(0:11)=[255,235,255,255,255,200,100,100,100,  0,160,1]
        b(0:11)=[255,190,200,150,255,255,255,255,255,255, 60,1]
        r(12:25)=[175,200,235,254,251,253,255, 255, 230,205,160, 90,  0,  0]
        g(12:25)=[  0,  0, 50,150,202,240,250, 255, 255,247,230,200,100,  0]
        b(12:25)=[ 70, 50, 10, 10, 77,170,230, 255, 255,237,220,237,255,200]
    END
    'sst':$
      BEGIN
     ;--
     TVLCT, R, G, B, /GET
     CCG_RGBLOAD,file='/flurry/home/fdelage/LIB/9color_table'
     ;--
     R(255)=0
     G(255)=0
     B(255)=0
     ;-- vert1
     R(251)=193
     G(251)=255
     B(251)=193
     ;-- vert2
     R(253)=152
     G(253)=251
     B(253)=152
     ;-- blue pastel
     R(252)=153
     G(252)=204
     B(252)=255
     ;-- Grey
     R(254)=176
     G(254)=196
     B(254)=222
     ;-- light yellow
     R(125)=255
     G(125)=239
     B(125)=219
     ;-- light green
     R(126)=245
     G(126)=255
     B(126)=250
     END
     'anom':$
       BEGIN
         steps = 128
         scaleFactor = FINDGEN(steps) / (steps - 1)
         scaleFactor2 = FINDGEN(steps) / (steps - 1)
         ;--  Do first 100 colors (yellow to blue).
   
         ; Red vector: 255 -> 0
         redVector = reverse(REPLICATE(255, steps)) ;255 + (0 - 255) * scaleFactor
         ; Green vector: 255 -> 0
         greenVector = reverse(255* reverse(scaleFactor2))
         ; Blue vector: 0 -> 255
         blueVector = reverse(255* reverse(scaleFactor2))
   
         ; Do second 100 colors (blue to red).
   
         ; Red vector: 0 -> 255
         redVector = [redVector, 255* reverse(scaleFactor2)]
         ; Green vector: 0 -> 0
         greenVector = [greenVector, 255* reverse(scaleFactor2)]
         ; Blue vector: 255 -> 0
         blueVector = [blueVector, REPLICATE(255, steps)]

         redVector(0)=0
         greenVector(0)=0
         blueVector(0)=0

         r=redVector
         g=greenVector
         b=blueVector
     END
 
 ENDCASE

END
