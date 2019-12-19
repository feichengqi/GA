library(rlist)
library(data.table)
source('fitness/project.R')
source('crossover/crossover.R')


GA_initialize = function(dim, p = 20){
    # This function takes dimension and the number of individuals
    # and returns a list of initialized individuals
    
    individuals = data.frame(matrix(runif(n = dim*p) > 0.5, dim, p))
    
    return(as.list(individuals))
}


GA_compute = function(dim, p, t = 100, selection_method = 'rank', partial_update = FALSE, parent_ratio = 0.5, ...){
    # This function computes the GA results
    # dim is the dimension of genes
    # p is the number of individuals in population
    # t is the time of iterating
    assert_that(parent_ratio >= 0 & parent_ratio <= 1, msg = 'Ratio of parents should be between 0 and 1')
    assert_that(nrow(data)>=dim, msg = 'The dimenstion exceeds the length of observed data vector.')
    pop = GA_initialize(dim, p)
    
    for(i in 1:t){
        # Find fitness
        fitness_scores = fitness_score(pop, ...)
        #UPDATE(pop)
        parents = pop[select_index(fitness_scores, method = selection_method)]
        p = length(parents)
        n_cross = floor(p/2)
        children = list(n_cross*2)

        for(i in 1:n_cross){
            childs = ga_crossover(parentA = parents[[i]], parentB = parents[[p-i+1]])
            children[[(i*2-1)]] = childs[[1]]
            children[[(i*2)]] = childs[[2]]
        }
        
        if(partial_update){
            ind = sample(1:p, floor(parent_ratio*p), replace = FALSE)
            children[ind] = pop[ind]
        }
        
        children = lapply(children, ga_mutate)
        pop = children
    }
    return(pop)
}

select_index = function(fitness_scores, method = 'rank'){
    assert_that(method %in% c('score', 'rank'), msg = "method should be {score, rank}")
    p = length(fitness_scores)
    if(method == 'rank'){
        fitness_rank = frankv(fitness_scores, order = 1)
        selection_prob = 2*fitness_rank / (p^2 + p)
        parents = sample(1:p, size = p, replace = TRUE, prob = selection_prob)
    }
    if(method == 'score'){
        parents = (1:p)[fitness_scores >= median(fitness_scores)]
        parents = c(parents, sample(1:p, size = p - length(parents), replace = TRUE))
    }
    return(parents)
}



#y = c(1,3,5,7,9)
#data <- data.frame(x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5),
#                   x4 =c (5,4,6,2,4),x5 = c(100,200,300,400,500))
data = read.table('madelon_train.data')
y = unlist(read.table('madelon_train.labels'))
pop = GA_compute(dim = 500, p = 20, t = 100, selection_method = 'rank', partial_update = TRUE, data = data, fitness = AIC, func = glm, response = y)
rank = frankv(fitness_score(pop, data, fitness = AIC, func = glm, response = y), order = -1, ties.method = 'first')
pop[rank == 1]
