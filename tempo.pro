pro tempo

n1 = 10
n2 = 20
n3 = 3
a= findgen(n1)
b= findgen(n2,n1,n3)
c= ['e','aazzzz','ss']

dim_size = [n1,n2,n3]
var_dim = {V1:[0], V2:[1,0,2], V3:[2]}
dim_val = {D1:findgen(n1), D2:findgen(n2), D3:findgen(n3)}

peyl_writenc, a,b,c,$
  file='tempo.nc',$
  var_dim   = var_dim,$
  var_name  = var_name,$
  var_title = var_title,$
  var_units = var_units,$
  dim_size  = dim_size,$
  dim_val   = dim_val,$
  dim_name  = dim_name,$
  dim_title = dim_title,$
  dim_units = dim_units,$
  dim_unlimited = dim_unlimited,$
  var_misval = var_misval,$
  no_dim = no_dim,$
  nomessage = nomessage


stop
end
