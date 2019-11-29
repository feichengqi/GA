library(rlist)
# This function takes dimension and the number of individuals
# and returns a list/matrix of initialized individuals
GA_initialize = function(dim, p = 20){
    individuals = list()
    for(i in 1:p){
        individual = as.integer(runif(n = dim) > 0.5)
        individuals = list.append(individuals, individual)
    }
    return(individuals)
}

# This function computes the GA results
GA_compute = function(dim, p, t = 100){
    pop = GA_initialize(dim, p)
    for(i in 1:t){
        #FITNESS(pop)
        #UPDATE(pop)
    }
    return(pop)
}