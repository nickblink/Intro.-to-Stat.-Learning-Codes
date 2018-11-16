find_pve = function(m){
  numer = 0 
  denom = 0
  for (i in 1:nrow(dsc)){
    load = 0
    for (j in 1:ncol(dsc)){
      load = load + uspc$rotation[j,m]*dsc[i,j]
      denom = denom + dsc[i,j]^2
    }
    numer = numer + load^2
  }
  return(numer/denom)
}

pve2 = c(0)
for (m in 1:4){
  pve2 = c(pve2, find_pve(m))
}

print(pve2)


cluster1 = 0
cluster2 = 0
cluster3 = 0





