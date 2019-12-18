library(rlist)
library(data.table)
source('fitness/project.R')
source('crossover/crossover.R')


# This function takes dimension and the number of individuals
# and returns a list/matrix of initialized individuals
GA_initialize = function(dim, p = 20){
    individuals = list()
    for(i in 1:p){
        individual = runif(n = dim) > 0.5
        individuals = list.append(individuals, individual)
    }
    return(individuals)
}



# This function computes the GA results
# dim is the dimension of genes
# p is the number of individuals in population
# t is the time of iterating
GA_compute = function(dim, p, t = 100){
    pop = GA_initialize(dim, p)
    for(i in 1:t){
        # Find fitness
        fitness_score = fitness2(pop, data, fitness = AIC, func = lm)
        
        #UPDATE(pop)
        children = list()
        parents = pop[select_index(fitness_score)]
        p = length(parents)
        for(i in 1:floor(p/2)){
            childs = ga_crossover(parentA = parents[[i]], parentB = parents[[p-i+1]])
            children = list.append(children, childs[[1]], childs[[2]])
        }
        children = lapply(children, ga_mutate)
        pop = children
    }
    return(pop)
}

select_index = function(fitness_score){
    p = length(fitness_score)
    # Ascending rank with respect to fitness score
    fitness_rank = frankv(fitness_score, order = 1)
    selection_prob = 2*fitness_rank / (p^2 + p)
    parents = sample(1:p, size = p, replace = TRUE, prob = selection_prob)
    return(parents)
}





y = c(1,3,5,7,9)
data <- data.frame(x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))
pop = GA_compute(dim = 3, p = 20, t = 100)
rank = frankv(fitness2(pop, data, fitness = AIC, func = lm), order = -1, ties.method = 'first')
pop[rank == 1]