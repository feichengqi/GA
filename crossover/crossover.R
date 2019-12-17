ga_crossover = function(parentA, parentB){
  # This function take in two parents and uses a simple split
  # to create children. It then picks a random child to pass on 
  # in the algorithm
  
  n = length(parentA)
  splitpt = sample(1:(n-1),1)
  
  # Create two children
  childA = parentA
  childA[splitpt+1:n] = parentB[splitpt+1:n]
  childB = parentB
  childB[splitpt+1:n] = parentA[splitpt+1:n]
  
  # Randomly pick one child to return
  return(unlist(sample(list(childA, childB),1)))  
}

ga_mutate = function(child, mprob = 0.03){
  # This function takes in a child and randomly mutates each
  # gene with probability mprob
  
  changes = as.logical(rbinom(length(child), 1, prob = mprob))
  child[changes] = !child[changes]
  
  return(child)
}

parents = as.data.frame(matrix(rbinom(20*20,1,0.5), 20, 20))
child_a = ga_crossover(parents[,1], parents[,2])
child_b = ga_mutate(child)
