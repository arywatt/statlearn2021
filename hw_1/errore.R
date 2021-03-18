

er=rep(NA, length(j.seq)+1)

for(idx in 0:length(j.seq)){
  
  fo=tryCatch(integrate(function(x,j) (doppler.fun(x)- proj.cos(x, f.coeff,j.seq[idx]))^2, lower = 0, upper = 1,
                        j = idx)$value, error = function(e) NA )
  er[idx+1] = fo
}

er
