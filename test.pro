pro test

ccg_opendev, saveas='test.psc',dev='psc',portrait=portrait
loadct,39

data=fltarr(256,256)

for ind=0,254 do begin
    data(ind,*)=ind
;data(ind,*)=250
endfor

data(*,*)=120
data(100,*)=125

peyl_map2d, data,sature_minmax=0

ccg_closedev,dev='psc'

end
