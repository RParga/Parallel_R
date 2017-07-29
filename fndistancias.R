euclideana = function(p1,p2)
{ # p1 y p2 pueden ser vectores o valores, de ambas maneras hace la operacion por componente	   
  return(sqrt(sum((p2-p1)**2)))
}
manhattan = function(p1,p2)
{
  return(sum(abs(p2-p1)))
}
ed_origen = function(p)
{
  dim = length(p)
  orig = rep(0,dim)
  return(euclideana(orig,p))
}
md_origen = function(p)
{
  dim = length(p)
  orig = rep(0,dim)
  return(manhattan(orig,p))
}