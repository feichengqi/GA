gen_linearcombo = function(x){
  # This function takes in a dataset and returns 
  # a vector that is a linear combination of 
  # a subset of the columns of the data
  
  keep = rep(FALSE, ncol(x))
  
  # Make sure we get a non trivial linear combination
  while(sum(keep) < 1){
    keep = rlogical(ncol(x))
  }
  
  betas = rnorm(sum(keep))
  
  # In case we get a combination of only one column
  if(sum(keep) == 1){
    return(betas*x[,keep])
  } else{
    return(rowSums(betas*x[,keep]))
  }
}

# Synthetic dataset creation
n = 2000
x = matrix(rnorm(n*5), n, 5)
linearcombos = replicate(15, gen_linearcombo(x))
probes = matrix(rnorm(n*80, 0, 5), n, 80)
betas = rnorm(5)
y = rnorm(n, betas[1]*x + betas[2]*x +
             betas[3]*x + betas[4]*x + betas[5])
x = cbind(x,linearcombos,probes)
