;+
; NAME:
;      wind_vector
;
; PURPOSE:
;       Overplot the wind vectors at their positions on an existing contour map
; EXPLANATION:
;       This procedure plots the win vectors 
;
; CATEGORY:
;       Plotting, Two-dimensional.
;
; CALLING SEQUENCE:
;        WIND_VECTOR,U,V,grid=grid,color=color,pos=posm,limits=limits,density=density,length=length,thick=thick
;
; INPUTS:
;       U:        An array of any dimension, containing the x-components
;                 of the particle velocities.
;       V:        An array of the same dimension as velx, containing the
;                 y-components of the particle velocities.
;       GRID:     structure containing, longitude and latitude of the domain
;       COLOR :   color of wind vectors
;       POS :     position of the plot
;       LIMITS :  LIMITS of the plots in the main domain
;       DENSITY : density of wind vectors; 2 mens one vector every two
;                 grid points
;       LENGHT :  Set this keyword equal to the length factor. The
;                 default of 1.0 makes the longest (U,V) vector the
;                 length of a cell. 
;       THICK :   Thickness of wind vectors 
;
;
; OUTPUTS:
;       This procedure plots the velocity vectors (U,V) at the
;       positions of the particles. 
;
; SIDE EFFECTS:
;       Plotting on the current device is performed.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;       Written by:  Francois Delage
;-

PRO wind_vector,u,v,grid=grid,color=color,pos=posm,limits=limits,density=density,length=length,thick=thick

IF N_ELEMENTS(density) EQ 0 THEN density=1
IF N_ELEMENTS(color) EQ 0 THEN color=cgCOLOR('Black')

;-- Set position
wtickname = replicate(' ',60)
plot,[limits(1),limits(3)],[limits(0),limits(2)],$
  POS=posm,/NODATA,/NORMAL, XSTYLE=5,YSTYLE=5,/NOERASE,XTICKNAME=wtickname,YTICKNAME=wtickname
;-- Get lat bounds
ii= where(grid.lat GT limits(0)) & latmin=ii(0)
ii= where(grid.lat LT limits(2),nn) & latmax=ii(nn-1)
;-- Get lon bounds
ii= where(grid.lon GT limits(1)) & lonmin=ii(0)
ii= where(grid.lon LT limits(3),nn) & lonmax=ii(nn-1)
;-- Call velovect
velovect,reform(u(lonmin:lonmax:density,latmin:latmax:density)),$
  reform(v(lonmin:lonmax:density,latmin:latmax:density)),$
  grid.lon(lonmin:lonmax:density),grid.lat(latmin:latmax:density),$
  /overplot,thick=thick,length=length,color=color

END
