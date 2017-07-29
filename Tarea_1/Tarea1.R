pos = 0
mayor = 0
dur = 100
for (t in 1:dur)
{ 
  if (runif(1) < 0.5)
  {
    pos = pos + 1
  }
  else
  {
    pos = pos - 1 
  }
  dist = abs(pos)
  if(dist>mayor)
  {
    mayor = dist
  }
}

print(pos)
print(mayor)