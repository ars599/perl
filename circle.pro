;Ex.:
;dataMax = Max(radius)
;percent25 = Circle(0, 0, 0.25*dataMax,steps=100)
;;Circle
;PlotS, percent25, Color=2
;;Half circle
;PlotS, percent25[0,0:steps/2-1], percent25[1,0:steps/2-1], Color=2
FUNCTION CIRCLE, xcenter, ycenter, radius,steps=steps
steps=keyword_set(steps) ? steps : 101
points = (2 * !PI / float(steps-1)) * FINDGEN(steps)
x = xcenter + radius * COS(points )
y = ycenter + radius * SIN(points )
RETURN, TRANSPOSE([[x],[y]])
END

